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
CUR_DATA_ROOT = os.path.join(_BASE_DIR, "..", "cur_data")

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
    """Convert a series of mixed strings to numeric, handling Vietnamese number formats."""
    def _parse(v):
        if pd.isna(v) or str(v).strip() in ("", "None", "nan"):
            return None
        s = str(v).strip()
        # Remove parentheses → negative
        neg = s.startswith("(") and s.endswith(")")
        s = s.strip("()")
        # Remove thousand separators
        s = re.sub(r"[,\s]", "", s)
        # Handle comma as decimal (European style)
        if "," in s and "." not in s:
            s = s.replace(",", ".")
        try:
            val = float(s)
            return -val if neg else val
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


def _cur_data_path(quarter_id: str, group_code: str) -> str:
    folder = os.path.join(CUR_DATA_ROOT, quarter_id)
    os.makedirs(folder, exist_ok=True)
    return os.path.join(folder, f"{group_code}.parquet")


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
    changes = {"new": 0, "changed": 0, "duplicate": 0, "removed": 0}
    output_path = _cur_data_path(quarter_id, group_code)
    prev_qid = _prev_quarter_id(quarter_id)

    if prev_qid and gc not in ("Vietjet",):
        prev_path = _cur_data_path(prev_qid, group_code)
        if os.path.exists(prev_path):
            prev_df = pd.read_parquet(prev_path)
            df, changes = _merge_with_prev(df, prev_df, dpnv_date)

    # 4. Save
    df.to_parquet(output_path, index=False)

    return {
        "ok": True,
        "rows": len(df),
        "output_path": output_path,
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

        # Den_Ngay/Thang/Nam: use next KY's date if it has value, else use contract end date
        next_phi = df.get(f"{ky_next}_PHI_THUC_THU", pd.Series([0]*len(df)))
        tu_day = df.get(f"{ky}_DUE_DATE_DAY", pd.Series([None]*len(df)))

        blank[f"Ky_phi_{i}_Den_Ngay"] = tu_day.where(
            tu_day.isna() | (tu_day.astype(str).str.strip() == "0"),
            other=next_phi.where(next_phi != "0",
                                  df.get("NGAY_HIEU_LUC_DEN", pd.Series([None]*len(df)))).where(
                next_phi == "0",
                df.get(f"{ky_next}_DUE_DATE_DAY", pd.Series([None]*len(df)))
            )
        ).values

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
    long["Dau_muc"] = long["Chi_tieu"].str.extract(r"_(Phi|Ty|Muc|Hach.*)$", expand=False)
    long["Don_vi_lien_ket"] = long["Chi_tieu"].str.replace(r"_(Phi|Ty|Muc|Hach).*$", "", regex=True)

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
) -> tuple[pd.DataFrame, Dict[str, int]]:
    """
    Merge current quarter data with previous quarter data.
    Returns (merged_df, change_summary).
    """
    common_cols = [c for c in cur_df.columns if c in prev_df.columns]
    keys = [k for k in MERGE_KEYS if k in common_cols]

    if not keys or dpnv_date is None:
        # No merge key or no dpnv date → just return current data
        return cur_df, {"new": len(cur_df), "changed": 0, "duplicate": 0, "removed": 0}

    cur = cur_df[common_cols].copy()
    prev = prev_df[common_cols].copy()

    # Filter: only keep records whose end date > dpnv_date
    def _end_date_filter(df):
        try:
            end = pd.to_datetime({
                "year": pd.to_numeric(df.get("Thoi_han_bao_hiem_Den_Nam", pd.Series()), errors="coerce"),
                "month": pd.to_numeric(df.get("Thoi_han_bao_hiem_Den_Thang", pd.Series()), errors="coerce"),
                "day": pd.to_numeric(df.get("Thoi_han_bao_hiem_Den_Ngay", pd.Series()), errors="coerce"),
            }, errors="coerce")
            return df[end > pd.Timestamp(dpnv_date)]
        except Exception:
            return df

    cur_f = _end_date_filter(cur)
    prev_f = _end_date_filter(prev)

    # New rows (in cur but not in prev)
    new_rows = cur_f.merge(prev_f[keys].drop_duplicates(), on=keys, how="left", indicator=True)
    new_rows = new_rows[new_rows["_merge"] == "left_only"].drop(columns=["_merge"])
    n_new = len(new_rows)

    # Duplicate rows (identical in both)
    dup = cur_f.merge(prev_f, on=common_cols, how="inner")
    n_dup = len(dup)

    # Removed rows (in prev but not in cur)
    removed = prev_f.merge(cur_f[keys].drop_duplicates(), on=keys, how="left", indicator=True)
    removed = removed[removed["_merge"] == "left_only"].drop(columns=["_merge"])
    n_removed = len(removed)

    # Changed rows
    n_changed = len(cur_f) - n_new - n_dup

    # Final merged dataset: prev + cur (deduped by keys, prefer cur)
    merged = pd.concat([prev[common_cols], cur[common_cols]], ignore_index=True)
    merged = merged.drop_duplicates(subset=keys, keep="last")

    return merged, {
        "new": n_new,
        "changed": max(0, n_changed),
        "duplicate": n_dup,
        "removed": n_removed,
    }
