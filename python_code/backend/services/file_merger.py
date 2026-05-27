"""
file_merger.py — mirrors 2.ghep_file.R

Bước 2 trong pipeline: đọc raw Excel đã upload, chuẩn hoá cấu trúc cột,
ghép với dữ liệu quý trước (nếu có), lưu ra .parquet trong cur_data/.

Hỗ trợ 3 luồng:
  - General (Eng/Fire/Marine/Misc/PA/Travel/Kcare LT|ST)
  - XCG / PA_NNTX
  - Vietjet
"""

import os
import re
import json
import unicodedata
from datetime import date
from typing import Optional, Dict, Any, List

import pandas as pd

# ---------------------------------------------------------------------------
# Load column schemas
# ---------------------------------------------------------------------------
_BASE_DIR = os.path.dirname(__file__)
_SCHEMAS_PATH = os.path.join(_BASE_DIR, "..", "schemas.json")
_schemas: Dict[str, List[str]] = {}
if os.path.exists(_SCHEMAS_PATH):
    with open(_SCHEMAS_PATH, "r", encoding="utf-8") as _f:
        _schemas = json.load(_f)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
DATA_ROOT = os.path.join(_BASE_DIR, "..", "data")
CUR_DATA_ROOT = DATA_ROOT

NUMERIC_COLS = [
    "Thoi_han_bao_hiem_Tu_Ngay", "Thoi_han_bao_hiem_Tu_Thang", "Thoi_han_bao_hiem_Tu_Nam",
    "Thoi_han_bao_hiem_Den_Ngay", "Thoi_han_bao_hiem_Den_Thang", "Thoi_han_bao_hiem_Den_Nam",
    "So_tien_bao_hiem_So_tien", "Tong_phi_bao_hiem_khong_thue_So_tien",
    "Ty_le_dong_bao_hiem_coinsurance", "Ty_le_giu_lai_cua_BHBV_TBH_cung_cap",
    "Ty_le_giu_lai_cua_BHBV_checked", "Ty_le_giu_lai_cua_BHBV",
]

MERGE_KEYS = [
    "So_don_Ma_hop_dong_Ma_SDBS", "So_don_Nhom_nganh_nghe_kinh_doanh",
    "So_don_Nhom_rui_ro", "So_InsureJ",
]

VIETJET_COLS = [
    "Nam_phat_sinh_doanh_thu", "Thang_phat_sinh_doanh_thu",
    "Vietjet_MO_RONG_Phi_bao_hiem_sau_dong_bao_hiem",
    "Vietjet_MO_RONG_Phi_BH_tinh_tai", "Vietjet_MO_RONG_Ty_le_tai_BH",
    "Vietjet_MO_RONG_Phi_TBH", "Vietjet_MO_RONG_Muc_trach_nhiem_BH",
    "Vietjet_MO_RONG_Hach_toan_BV_Vung_tau_100pct",
    "Vietjet_NOI_DIA_Phi_bao_hiem_sau_dong_bao_hiem",
    "Vietjet_NOI_DIA_Ty_le_tai_BH", "Vietjet_NOI_DIA_Phi_TBH",
    "Vietjet_NOI_DIA_Muc_trach_nhiem_BH", "Vietjet_NOI_DIA_Hach_toan_BV_Phu_My_100pct",
    "Vietjet_Staff_Phi_bao_hiem_sau_dong_bao_hiem", "Vietjet_Staff_Ty_le_tai_BH",
    "Vietjet_Staff_Phi_TBH", "Vietjet_Staff_Muc_trach_nhiem_BH",
    "Vietjet_Travel_Safe_Phi_bao_hiem_sau_dong_bao_hiem",
    "Vietjet_Travel_Safe_Phi_BH_tinh_tai", "Vietjet_Travel_Safe_Ty_le_tai_BH",
    "Vietjet_Travel_Safe_Phi_TBH", "Vietjet_Travel_Safe_Muc_trach_nhiem_BH",
    "Vietjet_Travel_Safe_Hach_toan_BV_Phu_My_20pct",
]

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _generate_fee_cols(ky: int) -> List[str]:
    p = f"Ky_phi_{ky}"
    return [
        f"{p}_So_tien_VND", f"{p}_So_tien_USD", f"{p}_So_tien_EUR",
        f"{p}_Tu_Ngay", f"{p}_Tu_Thang", f"{p}_Tu_Nam",
        f"{p}_Den_Ngay", f"{p}_Den_Thang", f"{p}_Den_Nam",
        f"{p}_Ghi_Ngay", f"{p}_Ghi_Thang", f"{p}_Ghi_Nam",
    ]


def _to_numeric(series: pd.Series) -> pd.Series:
    """
    Convert a series of mixed strings to numeric.
    Mirrors R's convert_to_numeric() in 0.start.R (lines 66-153) exactly.
    """
    def _parse(v):
        if pd.isna(v):
            return None
        s = str(v).strip()
        if s == "" or s.lower() in ("none", "nan", "na"):
            return None

        # Remove all whitespace (mimicking gsub("[[:space:]]", "", s))
        s0 = re.sub(r"\s+", "", s)
        if not s0:
            return None

        sign = 1
        # Parenthesized negative notation, e.g. (123) -> -123
        if s0.startswith("(") and s0.endswith(")"):
            sign = -1
            s0 = s0[1:-1]

        # Leading minus
        if s0.startswith("-"):
            sign = -1
            s0 = s0[1:]

        # If no separator present
        if "." not in s0 and "," not in s0:
            try:
                return sign * float(s0)
            except ValueError:
                return None

        has_dot = "." in s0
        has_comma = "," in s0

        if has_dot and has_comma:
            # Both separators: find the last pos, treat it as decimal dot, remove all other seps
            seps = [i for i, c in enumerate(s0) if c in (".", ",")]
            last_pos = seps[-1]
            out = []
            for j, c in enumerate(s0):
                if c in (".", ","):
                    if j == last_pos:
                        out.append(".")
                    else:
                        continue
                else:
                    out.append(c)
            clean = "".join(out)
        else:
            # Only one type of separator
            sep_char = "." if has_dot else ","
            last_pos = s0.rfind(sep_char)
            after = s0[last_pos + 1:]

            if re.match(r"^\d{3}$", after):
                # Thousand separator: if comma, remove it, else leave dot as-is
                if sep_char == ",":
                    clean = s0.replace(",", "")
                else:
                    clean = s0
            else:
                # Decimal separator: if comma, replace with dot
                if sep_char == ",":
                    clean = s0.replace(",", ".")
                else:
                    clean = s0

        try:
            return sign * float(clean)
        except ValueError:
            return None

    return series.apply(_parse)


def _get_schema_cols(group_code: str) -> List[str]:
    gc = group_code or ""
    # Direct key
    for suffix in ("_Pre", "_LT_Pre", "_ST_Pre"):
        key = gc + suffix
        if key in _schemas:
            return _schemas[key]
    # Marine → 7_term
    if re.search(r"Marine", gc, re.IGNORECASE):
        return _schemas.get("7_term", [])
    return _schemas.get("Eng_LT_Pre", [])


def _find_data_start(df: pd.DataFrame):
    """Find the row index where real data starts (header row + 1)."""
    work = df.copy()

    # If col 10 (0-based) has "Ngày" → insert 2 blank cols at positions 5-6
    if work.shape[1] >= 10:
        col10 = work.iloc[:, 9].astype(str)
        if col10.str.match(r"^Ng[aà]y", case=False).any():
            left = work.iloc[:, :5]
            blanks = pd.DataFrame([[None, None]] * len(work), index=work.index, columns=["_p1", "_p2"])
            right = work.iloc[:, 5:]
            work = pd.concat([left, blanks, right], axis=1)
            work.columns = range(work.shape[1])

    max_drops = max(0, work.shape[1] - 12)
    for _ in range(max_drops + 1):
        if work.shape[1] < 12:
            break
        col12 = work.iloc[:, 11].astype(str)
        mask = col12.str.match(r"^Ng[aà]y", case=False)
        if mask.any():
            return int(mask.idxmax())
        work = work.iloc[:, 1:]
        work.columns = range(work.shape[1])
    return None


def get_quarter_dir(quarter_id: str) -> str:
    """Return the quarter folder path under DATA_ROOT (using dot format, e.g. Q1.2026)."""
    dot_qid = quarter_id.replace("_", ".")
    return os.path.join(DATA_ROOT, dot_qid)


def _cur_data_path(quarter_id: str, group_code: str) -> str:
    folder = get_quarter_dir(quarter_id)
    os.makedirs(folder, exist_ok=True)
    return os.path.join(folder, f"{group_code}.parquet")


def _prev_quarter_db_path(prev_qid: str) -> Optional[str]:
    """Check for previous quarter database in data/ (dot or underscore)."""
    dot_format = prev_qid.replace("_", ".")
    candidates = [
        os.path.join(DATA_ROOT, f"{dot_format}.db"),
        os.path.join(DATA_ROOT, f"{prev_qid}.db")
    ]
    for c in candidates:
        if os.path.exists(c):
            return c
    return None


def _prev_quarter_parquet_dir(prev_qid: str) -> Optional[str]:
    """Check for previous quarter parquet directory in data/ (dot or underscore)."""
    dot_format = prev_qid.replace("_", ".")
    candidates = [
        os.path.join(DATA_ROOT, prev_qid),
        os.path.join(DATA_ROOT, dot_format)
    ]
    for c in candidates:
        if os.path.exists(c) and os.path.isdir(c):
            return c
    return None


def _prev_quarter_id(quarter_id: str) -> Optional[str]:
    """Return the previous quarter string, e.g. Q1_2026 → Q4_2025."""
    m = re.match(r"Q([1-4])_(\d{4})", quarter_id)
    if not m:
        return None
    q, y = int(m.group(1)), int(m.group(2))
    if q == 1:
        return f"Q4_{y - 1}"
    return f"Q{q - 1}_{y}"


# ===========================================================================
# Public API
# ===========================================================================

def merge_file(
    file_path: str,
    sheet_name: str,
    group_code: str,
    quarter_id: str,
    dpnv_date: Optional[date] = None,
) -> Dict[str, Any]:
    """
    Process & merge a validated Excel file into standardised parquet format.

    Returns:
        {
            "ok": bool,
            "rows": int,
            "output_path": str,
            "changes": { "new": int, "changed": int, "duplicate": int, "removed": int },
            "errors": [str]
        }
    """
    # 1. Read
    try:
        raw = pd.read_excel(file_path, sheet_name=sheet_name, header=None, dtype=str)
        if isinstance(raw, dict):
            raw = next(iter(raw.values()))
    except Exception as e:
        return {"ok": False, "errors": [f"Không đọc được file: {e}"], "rows": 0}

    # 2. Route
    gc = group_code or ""
    if gc == "Vietjet":
        df = _process_vietjet(raw)
    elif re.search(r"XCG|PA_NNTX", gc, re.IGNORECASE):
        df = _process_xcg(raw, gc)
    else:
        df = _process_general(raw, gc)

    if df is None or df.empty:
        return {"ok": False, "errors": ["Không trích xuất được dữ liệu từ file."], "rows": 0}

    # 3. Merge with previous quarter data
    changes = {"new": 0, "changed": 0, "duplicate": 0, "removed": 0, "ketqua": None}
    output_path = _cur_data_path(quarter_id, group_code)
    prev_qid = _prev_quarter_id(quarter_id)

    if prev_qid and gc not in ("Vietjet",):
        prev_df = None
        db_file = _prev_quarter_db_path(prev_qid)
        if db_file:
            import sqlite3
            print(f"Loading previous quarter {prev_qid} data from SQLite database: {db_file}")
            try:
                conn = sqlite3.connect(db_file)
                cursor = conn.cursor()
                cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name=?", (group_code,))
                if cursor.fetchone():
                    prev_df = pd.read_sql_query(f'SELECT * FROM "{group_code}"', conn)
                    print(f"Successfully loaded {len(prev_df)} rows from table '{group_code}' in {db_file}")
                else:
                    print(f"Table '{group_code}' not found in database {db_file}")
                conn.close()
            except Exception as e:
                print(f"Error loading previous quarter data from DB: {e}")

        # Fallback to parquet folder
        if prev_df is None:
            prev_dir = _prev_quarter_parquet_dir(prev_qid)
            if prev_dir:
                prev_path = os.path.join(prev_dir, f"{group_code}.parquet")
                if os.path.exists(prev_path):
                    print(f"Fallback: loading previous quarter {prev_qid} data from parquet file: {prev_path}")
                    try:
                        prev_df = pd.read_parquet(prev_path)
                    except Exception as e:
                        print(f"Error loading fallback parquet file: {e}")

        if prev_df is not None:
            df, changes = _merge_with_prev(df, prev_df, dpnv_date)

    # 4. Save merged data
    df.to_parquet(output_path, index=False)

    # 4.1 Save to current quarter SQLite database (e.g. data/Q1.2026.db)
    dot_qid = quarter_id.replace("_", ".")
    db_path = os.path.join(DATA_ROOT, f"{dot_qid}.db")
    try:
        import sqlite3
        print(f"Saving merged data to SQLite database: {db_path} (Table: {group_code})")
        conn = sqlite3.connect(db_path)
        
        # Clean column datatypes to ensure SQLite compatibility
        db_df = df.copy()
        for col in db_df.columns:
            if pd.api.types.is_datetime64_any_dtype(db_df[col]):
                db_df[col] = db_df[col].dt.strftime('%Y-%m-%d %H:%M:%S')
            elif db_df[col].dtype == 'object' or str(db_df[col].dtype) in ['string', 'category']:
                db_df[col] = db_df[col].astype(str)
                db_df[col] = db_df[col].replace({'nan': None, '<NA>': None, 'None': None, 'NAT': None})
            elif str(db_df[col].dtype).startswith('Int') or str(db_df[col].dtype).startswith('Float'):
                db_df[col] = db_df[col].where(db_df[col].notna(), None)
                
        db_df.to_sql(group_code, conn, if_exists="replace", index=False)
        conn.close()
        print(f"Successfully saved merged data to database table: {group_code}")
    except Exception as e:
        print(f"Error saving merged data to SQLite DB: {e}")

    # 5. Save ketqua audit DataFrame (mirrors R's checked_df1(ketqua))
    ketqua_path = None
    ketqua_df = changes.pop("ketqua", None)  # extract before returning changes dict
    if ketqua_df is not None and not ketqua_df.empty:
        ketqua_path = output_path.replace(".parquet", "_ketqua.parquet")
        # Ensure all object columns are str-safe for parquet
        for col in ketqua_df.select_dtypes(include=["object"]).columns:
            ketqua_df[col] = ketqua_df[col].astype(str)
        ketqua_df.to_parquet(ketqua_path, index=False)

    return {
        "ok": True,
        "rows": len(df),
        "output_path": output_path,
        "ketqua_path": ketqua_path,
        "changes": changes,
        "errors": [],
    }


# ---------------------------------------------------------------------------
# Internal processors
# ---------------------------------------------------------------------------

def _process_general(raw: pd.DataFrame, group_code: str) -> Optional[pd.DataFrame]:
    """Mirror the 'else' branch in 2.ghep_file.R for general LT/ST forms."""
    schema_cols = _get_schema_cols(group_code)
    if not schema_cols:
        return None

    # Special column adjustments for Kcare and Travel (CTTV/BHTT)
    df = raw.copy()
    if re.search(r"kcare", group_code, re.IGNORECASE):
        # Drop cols 12-14 (0-based: 11,12,13), insert 3 NA cols at position 21
        cols = list(df.columns)
        df = df.drop(df.columns[[11, 12, 13]], axis=1)
        df.columns = range(df.shape[1])
        for i in range(3):
            df.insert(21 + i, f"_kc_pad_{i}", None)
        df.columns = range(df.shape[1])

    elif re.search(r"CTTV|BHTT", group_code, re.IGNORECASE):
        # Insert 2 NA cols after position 5, then 2 NA cols after position 9
        for i in range(2):
            df.insert(5 + i, f"_p1_{i}", None)
        df.columns = range(df.shape[1])
        for i in range(2):
            df.insert(9 + i, f"_p2_{i}", None)
        df.columns = range(df.shape[1])
        schema_cols = schema_cols[:36]  # CTTV/BHTT uses only first 36 cols of 7_term

    # Find real data start row
    header_idx = _find_data_start(df)
    if header_idx is None:
        return None

    data = df.iloc[header_idx + 1 :].reset_index(drop=True)

    # Pad / trim
    n_data, n_schema = data.shape[1], len(schema_cols)
    if n_data < n_schema:
        for i in range(n_schema - n_data):
            data[f"_pad_{i}"] = None
    elif n_data > n_schema:
        data = data.iloc[:, :n_schema]
    data.columns = schema_cols

    # Drop junk rows
    data = data[data.isnull().sum(axis=1) <= (data.shape[1] - 4)]
    data = data[~data.iloc[:, 0].astype(str).str.contains(r"Tổng|Tong", case=False, na=False)]
    data = data.reset_index(drop=True)

    if data.empty:
        return None

    # Convert date columns to int
    date_cols = [c for c in data.columns if re.search(r"_Ngay$|_Thang$|_Nam$", c)]
    for col in date_cols:
        data[col] = pd.to_numeric(data[col], errors="coerce").astype("Int64")

    # Convert numeric money columns
    exist_num = [c for c in NUMERIC_COLS if c in data.columns]
    for col in exist_num:
        data[col] = _to_numeric(data[col])

    # Convert fee amount columns (_So_tien_VND / _So_tien_USD)
    fee_amount_cols = [c for c in data.columns if re.search(r"_So_tien_VND$|_So_tien_USD$", c)]
    for col in fee_amount_cols:
        data[col] = _to_numeric(data[col])

    return data


def _process_xcg(raw: pd.DataFrame, group_code: str) -> Optional[pd.DataFrame]:
    """Mirror the XCG/PA_NNTX branch in 2.ghep_file.R."""
    # XCG files already have headers in row 0
    df = raw.copy()
    df.columns = df.iloc[0].astype(str).str.strip()
    df = df.iloc[1:].reset_index(drop=True)

    # Drop junk rows
    df = df[df.isnull().sum(axis=1) <= (df.shape[1] - 4)]
    df = df[~df.iloc[:, 0].astype(str).str.contains(r"Tổng|Tong", case=False, na=False)]
    df = df.reset_index(drop=True)

    if df.empty:
        return None

    # Build standard schema with extra fee cols (up to 10 installments)
    base_cols = _schemas.get("Eng_LT_Pre", [])
    extra_cols = [col for ky in range(7, 11) for col in _generate_fee_cols(ky)]
    all_cols = base_cols + extra_cols

    blank = pd.DataFrame(index=df.index, columns=all_cols)
    blank[:] = None

    # Map XCG columns → standard columns
    col_map = {
        "STT": df.iloc[:, 0] if df.shape[1] > 0 else None,
        "Ten_cong_ty_Ten_ban": df.get("CONG_TY"),
        "So_don_Ma_nghiep_vu": df.get("SO_DON"),
        "So_don_Ma_hop_dong_Ma_SDBS": df.get("LOAI_HINH_NAME"),
        "Ten_khach_hang": df.get("BEN_MUA_BAO_HIEM"),
        "So_InsureJ": df.get("BKS"),
        "Thoi_han_bao_hiem_Tu_Ngay": df.get("NGAY_HIEU_LUC_TU"),
        "Thoi_han_bao_hiem_Tu_Thang": df.get("THANG_HIEU_LUC_TU"),
        "Thoi_han_bao_hiem_Tu_Nam": df.get("NAM_HIEU_LUC_TU"),
        "Thoi_han_bao_hiem_Den_Ngay": df.get("NGAY_HIEU_LUC_DEN"),
        "Thoi_han_bao_hiem_Den_Thang": df.get("THANG_HIEU_LUC_DEN"),
        "Thoi_han_bao_hiem_Den_Nam": df.get("NAM_HIEU_LUC_DEN"),
        "So_tien_bao_hiem_So_tien": df.get("SO_TIEN_BH"),
        "So_tien_bao_hiem_Loai_tien": df.get("LOAI_TIEN_BH"),
        "Tong_phi_bao_hiem_khong_thue_So_tien": df.get("SUM(PHI_BAO_HIEM)"),
        "Tong_phi_bao_hiem_khong_thue_Loai_tien": df.get("BILLING_CURRENCY"),
    }

    # Find retention/giữ lại column
    retention_col = next(
        (c for c in df.columns if re.search(r"gi[uữ].{0,3}l[aạ]i|Retention", c, re.IGNORECASE)),
        None
    )
    if retention_col:
        col_map["Ty_le_giu_lai_cua_BHBV_checked"] = df[retention_col]

    for std_col, series in col_map.items():
        if series is not None and std_col in blank.columns:
            blank[std_col] = series.values

    # Map kỳ phí (KY1..KY10)
    for i in range(1, 11):
        ky = f"KY{i}"
        ky_next = f"KY{i + 1}"

        vnd = df.get(f"{ky}_PHI_THUC_THU")
        if vnd is None:
            continue

        blank[f"Ky_phi_{i}_So_tien_VND"] = vnd.values
        blank[f"Ky_phi_{i}_Tu_Ngay"] = df.get(f"{ky}_DUE_DATE_DAY", pd.Series([None]*len(df))).values
        blank[f"Ky_phi_{i}_Tu_Thang"] = df.get(f"{ky}_DUE_DATE_MONTH", pd.Series([None]*len(df))).values
        blank[f"Ky_phi_{i}_Tu_Nam"] = df.get(f"{ky}_DUE_DATE_YEAR", pd.Series([None]*len(df))).values

        # Den_Ngay/Den_Thang/Den_Nam:
        # R logic (2.ghep_file.R L160-182):
        #   if KYi has a date (not NA and != 0):
        #     if KY(i+1) phi != 0 → use KY(i+1) date
        #     else                 → use contract end date (NGAY/THANG/NAM_HIEU_LUC_DEN)
        #   else → NA
        tu_day   = df.get(f"{ky}_DUE_DATE_DAY",   pd.Series([None]*len(df)))
        next_phi = pd.to_numeric(
            df.get(f"{ky_next}_PHI_THUC_THU", pd.Series([0]*len(df))).astype(str).str.replace(",", "", regex=False),
            errors="coerce"
        ).fillna(0)

        # Mask: True where KYi has a valid date (not NA, not "0")
        has_date = tu_day.notna() & (tu_day.astype(str).str.strip() != "0") & (tu_day.astype(str).str.strip() != "")

        # Den_Ngay
        next_day   = df.get(f"{ky_next}_DUE_DATE_DAY",   pd.Series([None]*len(df)))
        den_ngay_hd = df.get("NGAY_HIEU_LUC_DEN", pd.Series([None]*len(df)))
        den_ngay = pd.Series([None]*len(df), dtype=object)
        den_ngay[has_date & (next_phi != 0)] = next_day[has_date & (next_phi != 0)].values
        den_ngay[has_date & (next_phi == 0)] = den_ngay_hd[has_date & (next_phi == 0)].values
        blank[f"Ky_phi_{i}_Den_Ngay"] = den_ngay.values

        # Den_Thang
        next_thang   = df.get(f"{ky_next}_DUE_DATE_MONTH", pd.Series([None]*len(df)))
        den_thang_hd = df.get("THANG_HIEU_LUC_DEN", pd.Series([None]*len(df)))
        den_thang = pd.Series([None]*len(df), dtype=object)
        den_thang[has_date & (next_phi != 0)] = next_thang[has_date & (next_phi != 0)].values
        den_thang[has_date & (next_phi == 0)] = den_thang_hd[has_date & (next_phi == 0)].values
        blank[f"Ky_phi_{i}_Den_Thang"] = den_thang.values

        # Den_Nam
        next_nam   = df.get(f"{ky_next}_DUE_DATE_YEAR", pd.Series([None]*len(df)))
        den_nam_hd = df.get("NAM_HIEU_LUC_DEN", pd.Series([None]*len(df)))
        den_nam = pd.Series([None]*len(df), dtype=object)
        den_nam[has_date & (next_phi != 0)] = next_nam[has_date & (next_phi != 0)].values
        den_nam[has_date & (next_phi == 0)] = den_nam_hd[has_date & (next_phi == 0)].values
        blank[f"Ky_phi_{i}_Den_Nam"] = den_nam.values

        blank[f"Ky_phi_{i}_Ghi_Ngay"] = df.get(f"{ky}_DUE_DATE_REAL_DAY", pd.Series([None]*len(df))).values
        blank[f"Ky_phi_{i}_Ghi_Thang"] = df.get(f"{ky}_DUE_DATE_REAL_MONTH", pd.Series([None]*len(df))).values
        blank[f"Ky_phi_{i}_Ghi_Nam"] = df.get(f"{ky}_DUE_DATE_REAL_YEAR", pd.Series([None]*len(df))).values

    result = blank.copy()
    result = result[result.isnull().sum(axis=1) <= (result.shape[1] - 4)]
    result = result[~result.iloc[:, 0].astype(str).str.contains(r"Tổng|Tong", case=False, na=False)]
    result = result.reset_index(drop=True)

    # Convert numeric
    all_fee_cols = [c for ky in range(1, 11) for c in _generate_fee_cols(ky)]
    num_cols = [c for c in (NUMERIC_COLS + all_fee_cols) if c in result.columns]
    for col in num_cols:
        result[col] = _to_numeric(result[col])

    return result


def _process_vietjet(raw: pd.DataFrame) -> Optional[pd.DataFrame]:
    """Mirror the Vietjet branch in 2.ghep_file.R — pivot wide → long → wide."""
    df = raw.copy()
    if df.shape[1] < 25:
        return None

    col25 = df.iloc[:, 24].astype(str)
    chi_so = col25[col25.str.contains("phí bảo hiểm", case=False, na=False)].index.tolist()
    if len(chi_so) < 2:
        return None

    s, e = chi_so[0], chi_so[1]
    max_col = min(45, df.shape[1])

    het = df.iloc[s + 1 : e - 3, [0, 1] + list(range(24, max_col))].copy()
    con = df.iloc[e + 1 : len(df) - 1, [0, 1] + list(range(24, max_col))].copy()

    # Filter junk rows
    het = het[het.isnull().sum(axis=1) <= (het.shape[1] - 2)].reset_index(drop=True)
    con = con[con.isnull().sum(axis=1) <= (con.shape[1] - 2)].reset_index(drop=True)

    n_cot = len(VIETJET_COLS)
    for part in (het, con):
        if part.shape[1] == n_cot:
            part.columns = VIETJET_COLS

    het["hieu_luc"] = "het"
    con["hieu_luc"] = "con"

    combined = pd.concat([het, con], ignore_index=True)

    id_cols = ["Nam_phat_sinh_doanh_thu", "Thang_phat_sinh_doanh_thu", "hieu_luc"]
    available_id = [c for c in id_cols if c in combined.columns]
    value_cols = [c for c in combined.columns if c not in id_cols]

    long = combined.melt(id_vars=available_id, value_vars=value_cols,
                          var_name="Chi_tieu", value_name="Gia_tri")

    # Extract entity (Don_vi_lien_ket) and metric (Dau_muc) from column name
    # R: str_extract(Chi_tieu, "_(Phi|Ty|Muc|Hach).*") %>% str_remove("^_")
    # Capture everything from the first _(Phi|Ty|Muc|Hach) onwards, then strip leading _
    long["Dau_muc"] = long["Chi_tieu"].str.extract(
        r"_((?:Phi|Ty|Muc|Hach).*)$", expand=False
    )
    long["Don_vi_lien_ket"] = long["Chi_tieu"].str.replace(
        r"_(?:Phi|Ty|Muc|Hach).*$", "", regex=True
    )

    wide = long.pivot_table(
        index=available_id + ["Don_vi_lien_ket"],
        columns="Dau_muc",
        values="Gia_tri",
        aggfunc="first",
    ).reset_index()
    wide.columns.name = None

    # Rename to standard
    rename_map = {
        "Phi_bao_hiem_sau_dong_bao_hiem": "Phi_bao_hiem_goc",
        "Phi_TBH": "Phi_bao_hiem_tai",
    }
    wide = wide.rename(columns=rename_map)

    for col in ("Phi_bao_hiem_goc", "Phi_bao_hiem_tai"):
        if col in wide.columns:
            wide[col] = pd.to_numeric(wide[col], errors="coerce")

    return wide


# ---------------------------------------------------------------------------
# Merge with previous quarter
# ---------------------------------------------------------------------------

def _merge_with_prev(
    cur_df: pd.DataFrame,
    prev_df: pd.DataFrame,
    dpnv_date: Optional[date],
) -> tuple[pd.DataFrame, Dict[str, Any]]:
    """
    Merge current quarter data with previous quarter data.
    Mirrors R logic in 2.ghep_file.R (lines 460-700).
    Returns (merged_df, result_dict) where result_dict contains:
      - 'new', 'changed', 'duplicate', 'removed': count ints
      - 'ketqua': DataFrame with Loai_dong and Cac_cot_thay_doi columns (audit trail)
    """
    common_cols = [c for c in cur_df.columns if c in prev_df.columns]
    keys = [k for k in MERGE_KEYS if k in common_cols]

    if not keys or dpnv_date is None:
        # No merge key or no dpnv date → just concatenate
        merged = pd.concat([prev_df[common_cols], cur_df[common_cols]], ignore_index=True)
        return merged, {"new": len(cur_df), "changed": 0, "duplicate": 0, "removed": 0, "ketqua": None}

    # Align column types between prev_df and cur_df
    prev = prev_df[common_cols].copy()
    cur = cur_df[common_cols].copy()

    for col in common_cols:
        if cur[col].dtype != prev[col].dtype:
            try:
                prev[col] = prev[col].astype(cur[col].dtype)
            except Exception:
                pass

    # ── _end_date_filter: mirrors R's filter(ngay_ket_thuc > ngay_input()) ─
    # R uses make_date() which converts invalid dates to NA (and NA > date = FALSE, so they're dropped)
    # We replicate this safely, logging any unexpected errors explicitly.
    def _end_date_filter(df: pd.DataFrame, label: str = "") -> pd.DataFrame:
        try:
            d_den = pd.to_numeric(df.get("Thoi_han_bao_hiem_Den_Ngay", pd.Series(dtype=float)), errors="coerce").fillna(1).astype(int)
            m_den = pd.to_numeric(df.get("Thoi_han_bao_hiem_Den_Thang", pd.Series(dtype=float)), errors="coerce").fillna(1).astype(int)
            y_den = pd.to_numeric(df.get("Thoi_han_bao_hiem_Den_Nam", pd.Series(dtype=float)), errors="coerce").fillna(1900).astype(int)

            # Re-index to align with df's index
            d_den = d_den.reindex(df.index).clip(1, 31).fillna(1).astype(int)
            m_den = m_den.reindex(df.index).clip(1, 12).fillna(1).astype(int)
            y_den = y_den.reindex(df.index).clip(1800, 2200).fillna(1900).astype(int)

            import datetime, calendar
            dates = []
            for y_val, m_val, d_val in zip(y_den, m_den, d_den):
                try:
                    dates.append(datetime.date(int(y_val), int(m_val), int(d_val)))
                except ValueError:
                    # Invalid day (e.g. 31 Feb) → clamp to last valid day, mirrors R's NA behaviour
                    d_max = calendar.monthrange(int(y_val), int(m_val))[1]
                    dates.append(datetime.date(int(y_val), int(m_val), d_max))

            end_ts = pd.to_datetime(dates)
            return df[end_ts > pd.Timestamp(dpnv_date)]
        except Exception as exc:
            # Log explicitly — never silently return unfiltered data
            print(f"[_end_date_filter] ERROR filtering '{label}': {exc}. Skipping filter — all rows kept.")
            return df

    cur_f  = _end_date_filter(cur,  label="cur")
    prev_f = _end_date_filter(prev, label="prev")

    # Cast comparison columns to string (mirrors R's across(all_of(cols_cmp), as.character))
    cols_cmp = [c for c in common_cols[11:] if not c.startswith("Check_") and c != "check_trung"]

    cur_f_str  = cur_f.copy()
    prev_f_str = prev_f.copy()
    for col in cols_cmp:
        cur_f_str[col]  = cur_f_str[col].astype(str).str.strip().str.lower()
        prev_f_str[col] = prev_f_str[col].astype(str).str.strip().str.lower()

    cur_f_str["source"]  = "cur"
    prev_f_str["source"] = "prev"

    # ── dong_moi: rows in cur that have NO matching key in prev ─────────────
    # Mirrors R: anti_join(cur2, pre2, by = keys) %>% mutate(Loai_dong = "Mới hoàn toàn")
    prev_keys = prev_f_str[keys].drop_duplicates()
    _new_merged = cur_f_str.merge(prev_keys, on=keys, how="left", indicator=True)
    dong_moi = _new_merged[_new_merged["_merge"] == "left_only"].drop(columns=["_merge"])
    dong_moi = dong_moi.drop(columns=["source"], errors="ignore")
    dong_moi["Loai_dong"] = "Mới hoàn toàn"
    dong_moi["Cac_cot_thay_doi"] = None
    n_new = len(dong_moi)

    # ── dong_trung: rows where ALL cols_cmp are identical in both quarters ──
    # Mirrors R: group_by(keys + cols_cmp) %>% filter(n_distinct(source) >= 2)
    all_check_cols = keys + cols_cmp
    _dup_merged = cur_f_str.merge(
        prev_f_str[all_check_cols].drop_duplicates(),
        on=all_check_cols, how="inner"
    )
    _dup_merged = _dup_merged.drop(columns=["source"], errors="ignore")
    _dup_merged["Loai_dong"] = "Trùng"
    _dup_merged["Cac_cot_thay_doi"] = None
    dong_trung = _dup_merged
    n_dup = len(dong_trung)

    # ── dong_thay_doi: rows with matching key but differing in ≥1 cols_cmp ─
    # Mirrors R: group_by(keys) %>% filter(n_distinct(source)>=2) %>%
    #             mutate(Cac_cot_thay_doi = paste(names where n_distinct>1))
    _all_data = pd.concat([cur_f_str, prev_f_str], ignore_index=True)
    # Find keys that appear in BOTH sources
    _both_keys = (
        _all_data.groupby(keys)["source"]
        .nunique()
        .reset_index()
    )
    _both_keys = _both_keys[_both_keys["source"] >= 2][keys]
    _changed_candidates = _all_data.merge(_both_keys, on=keys, how="inner")

    # For each unique key combo, detect which cols_cmp differ between sources
    changed_rows = []
    _none_strs = {"none", "nan", "na", "nat", "<na>"}
    for key_vals, grp in _changed_candidates.groupby(keys, dropna=False):
        changed_cols = [
            col for col in cols_cmp
            if grp[col].nunique(dropna=False) > 1
        ]
        if not changed_cols:
            continue  # identical — classified as Trùng already
        cac_cot_str = ", ".join(changed_cols)
        # Keep the cur row (source == 'cur') as the representative row
        cur_rows = grp[grp["source"] == "cur"].copy()
        if cur_rows.empty:
            continue
        cur_rows = cur_rows.drop(columns=["source"], errors="ignore")

        # co_dong_rong: mirrors R's any(is.na(as.matrix(pick(all_of(cols_cmp)))))
        # If any cell in the group's cols_cmp is NA/None → "Thêm thông tin"
        grp_vals = grp[cols_cmp].values.flatten().astype(str)
        co_dong_rong = any(v.lower() in _none_strs for v in grp_vals)

        cur_rows["Loai_dong"] = "Thêm thông tin" if co_dong_rong else "Thay đổi"
        cur_rows["Cac_cot_thay_doi"] = cac_cot_str
        changed_rows.append(cur_rows)

    dong_thay_doi = pd.concat(changed_rows, ignore_index=True) if changed_rows else pd.DataFrame()
    n_changed = len(dong_thay_doi)

    # ── dong_bi_bo: rows in prev that have NO matching key in cur ───────────
    # Mirrors R (full-pre-merge branch): anti_join(pre2, cur2, by = keys)
    cur_keys_only = cur_f_str[keys].drop_duplicates()
    _removed_merged = prev_f_str.merge(cur_keys_only, on=keys, how="left", indicator=True)
    _removed_rows = _removed_merged[_removed_merged["_merge"] == "left_only"].drop(columns=["_merge"])
    _removed_rows = _removed_rows.drop(columns=["source"], errors="ignore")
    _removed_rows["Loai_dong"] = "Bị bỏ"
    _removed_rows["Cac_cot_thay_doi"] = None
    dong_bi_bo = _removed_rows
    n_removed = len(dong_bi_bo)

    # ── ketqua: combine all four groups (mirrors R's rbind) ─────────────────
    _ketqua_parts = [
        df for df in [dong_moi, dong_thay_doi, dong_trung, dong_bi_bo]
        if df is not None and not df.empty
    ]
    ketqua = pd.concat(_ketqua_parts, ignore_index=True) if _ketqua_parts else pd.DataFrame()

    # Drop helper columns not present in original schema
    ketqua = ketqua.drop(columns=["source", "ngay_ket_thuc"], errors="ignore")

    # ── Concat raw unfiltered data for the final merged parquet ─────────────
    # Mirrors R: bind_rows(rds[, common_cols], df[, common_cols])
    merged = pd.concat([prev, cur], ignore_index=True)

    # Ensure money columns are numeric
    money_cols = [c for c in merged.columns if c.endswith("_So_tien_VND") or c.endswith("_So_tien_USD")]
    for col in money_cols:
        merged[col] = _to_numeric(merged[col])

    return merged, {
        "new":       n_new,
        "changed":   n_changed,
        "duplicate": n_dup,
        "removed":   n_removed,
        "ketqua":    ketqua,
    }
