# -*- coding: utf-8 -*-
"""
form_validator.py — mirrors 1.check_input.R exactly.

Three validation paths:
  - Vietjet      → _validate_vietjet()
  - XCG/PA_NNTX  → _validate_xcg()
  - General LT/ST → _validate_general()

Key annotation columns written into the returned DataFrame (mirrors R):
  - Check_Ngay_Hop_Le  : "Ngày hợp lệ"  or  "Sai: <col_prefix>, ..."
  - Check_So_Tien      : "Số tiền hợp lệ"  or  "Sai: <col_name>, ..."
  - check_trung        : "Trùng"  or  0  (integer, mirrors R's ifelse(is.na, 0, "Trùng"))
"""

import os
import re
import json
import unicodedata
import pandas as pd
import numpy as np
from typing import List, Dict, Any, Optional

# ---------------------------------------------------------------------------
# Load column schemas  (mirrors pre_data/*.rds in R)
# ---------------------------------------------------------------------------
SCHEMAS_PATH = os.path.join(os.path.dirname(__file__), "..", "schemas.json")
_schemas: Dict[str, List[str]] = {}
if os.path.exists(SCHEMAS_PATH):
    with open(SCHEMAS_PATH, "r", encoding="utf-8") as f:
        _schemas = json.load(f)

# ---------------------------------------------------------------------------
# Duplicate-check key columns (mirrors 1.check_input.R lines 651-661)
# ---------------------------------------------------------------------------
GENERAL_DUP_COLS = [
    "So_don_Ma_nghiep_vu",
    "So_don_Ma_hop_dong_Ma_SDBS",
    "So_InsureJ",
    "Doi_tuong_duoc_bao_hiem",
    "Dia_diem_bao_hiem",
    "Thoi_han_bao_hiem_Tu_Thang",
    "Thoi_han_bao_hiem_Tu_Nam",
    "Thoi_han_bao_hiem_Den_Ngay",
    "Thoi_han_bao_hiem_Den_Thang",
    "Thoi_han_bao_hiem_Den_Nam",
    "So_tien_bao_hiem_So_tien",
    "Tong_phi_bao_hiem_khong_thue_So_tien",
]

# XCG check_cols (mirrors lines 409-415)
XCG_CHECK_COLS = [
    "BKS",
    "LOAI_HINH_NAME",
    "NGAY_HIEU_LUC_TU", "THANG_HIEU_LUC_TU", "NAM_HIEU_LUC_TU",
    "NGAY_HIEU_LUC_DEN", "THANG_HIEU_LUC_DEN", "NAM_HIEU_LUC_DEN",
    "SO_TIEN_BH", "LOAI_TIEN_BH",
    "SUM(PHI_BAO_HIEM)", "BILLING_CURRENCY",
]


# ===========================================================================
# Core helper functions
# ===========================================================================

def _remove_accents(text: str) -> str:
    """Mirror remove_accents() in R (stringi Latin-ASCII)."""
    nfkd = unicodedata.normalize("NFKD", str(text))
    return "".join(c for c in nfkd if not unicodedata.combining(c))


def kiem_tra_ngay_hop_le(day, month, year) -> bool:
    """
    Mirror kiem_tra_ngay_hop_le() in 1.check_input.R exactly.
    - All blank/NA  → TRUE
    - All zero      → TRUE
    - Any one blank → FALSE
    - Otherwise     → try to parse date, return whether parsed day == input day
    """
    _BLANK = {"", "none", "nan", "nat", "null", "na"}

    def _parse(v):
        s = str(v).strip().lower()
        if s in _BLANK:
            return None
        try:
            return int(float(s))
        except (ValueError, TypeError):
            return None

    d, m, y = _parse(day), _parse(month), _parse(year)

    if d is None and m is None and y is None:
        return True
    if d == 0 and m == 0 and y == 0 and None not in (d, m, y):
        return True
    if None in (d, m, y):
        return False

    try:
        d_str = f"{y:04d}-{m:02d}-{d:02d}"
        import datetime
        parsed = datetime.date.fromisoformat(d_str)
        return parsed.day == d
    except Exception:
        return False


def kiem_tra_so_tien(x) -> str:
    """
    Mirror kiem_tra_so_tien() in 0.start.R exactly.
    Returns: "Hợp lệ" | "Chứa chữ" | "Ký tự không hợp lệ" | "Không chuyển được sang số"
    """
    if x is None:
        return "Hợp lệ"
    s = str(x).strip()
    if s in ("", "None", "nan", "NaN", "NA"):
        return "Hợp lệ"
    if re.search(r"[A-Za-z]", s):
        return "Chứa chữ"
    if re.search(r"[^0-9,.\-]", s):
        return "Ký tự không hợp lệ"
    clean = s.replace(",", "")
    try:
        float(clean)
        return "Hợp lệ"
    except ValueError:
        return "Không chuyển được sang số"


# ===========================================================================
# Header-row finder for general forms
# ===========================================================================

def _find_header_row(df: pd.DataFrame) -> tuple[Optional[int], pd.DataFrame]:
    """
    Mirrors the while-loop in 1.check_input.R (lines 537-555).
    Returns (row index of the header row, modified dataframe).
    """
    work = df.copy()

    # Step 1: if col 10 (index 9) starts with Ngày → insert 2 blank cols after col 5
    if work.shape[1] >= 10:
        col10 = work.iloc[:, 9].astype(str)
        if col10.str.match(r"^Ng[aà]y", case=False, na=False).any():
            left  = work.iloc[:, :5]
            blank2 = pd.DataFrame(
                [[None, None]] * len(work),
                index=work.index,
                columns=["_p1", "_p2"]
            )
            right = work.iloc[:, 5:]
            work  = pd.concat([left, blank2, right], axis=1)
            work.columns = range(work.shape[1])

    # Step 2: drop leading columns until col 12 (index 11) has 'Ngày'
    max_drops = max(0, work.shape[1] - 12)
    for _ in range(max_drops + 1):
        if work.shape[1] < 12:
            break
        col12 = work.iloc[:, 11].astype(str)
        hits = col12.str.match(r"^Ng[aà]y", case=False, na=False)
        if hits.any():
            return int(hits.idxmax()), work
        work = work.iloc[:, 1:]
        work.columns = range(work.shape[1])

    return None, work


# ===========================================================================
# Public API
# ===========================================================================

def validate_form(file_path: str, sheet_name: str, group_code: str) -> Dict[str, Any]:
    """
    Main entry point. Returns dict with keys:
      ok, errors, stats, warnings, df (annotated DataFrame)
    """
    try:
        raw = pd.read_excel(file_path, sheet_name=sheet_name, header=None, dtype=str)
        if isinstance(raw, dict):
            raw = next(iter(raw.values()))
    except Exception as e:
        return _err(f"Không đọc được file Excel: {e}")

    if not isinstance(raw, pd.DataFrame) or raw.empty:
        return _err("File rỗng.")

    gc = (group_code or "").strip()

    if gc == "Vietjet":
        return _validate_vietjet(raw)
    elif re.search(r"XCG|PA_NNTX", gc, re.IGNORECASE):
        return _validate_xcg(raw, gc)
    else:
        return _validate_general(raw, gc)


def get_annotated_dataframe(file_path: str, sheet_name: str, group_code: str) -> pd.DataFrame:
    """Returns the annotated DataFrame from validation (with Check_ columns)."""
    res = validate_form(file_path, sheet_name, group_code)
    if not res.get("ok"):
        raise ValueError(f"Validation failed: {res.get('errors', [])}")
    df = res.get("df")
    if df is None or not isinstance(df, pd.DataFrame):
        return pd.DataFrame()
    return df


# ===========================================================================
# Vietjet validator  (mirrors check_form_vietjet in 1.check_input.R)
# ===========================================================================

def _validate_vietjet(df: pd.DataFrame) -> Dict[str, Any]:
    errors: List[str] = []

    if df.shape[1] < 25:
        errors.append(f"File không đủ 25 cột (hiện có {df.shape[1]}).")
        return _fatal(errors)

    col25 = df.iloc[:, 24].astype(str)
    chi_so = col25[col25.str.contains("phí bảo hiểm", case=False, na=False)].index.tolist()
    if len(chi_so) < 2:
        errors.append("Không tìm thấy đủ 2 dòng chứa 'phí bảo hiểm' trong cột 25.")
        return _fatal(errors)

    s, e = chi_so[0], chi_so[1]

    if (s + 1) > (e - 4) or (e + 1) > (len(df) - 1):
        errors.append("Vị trí các dòng 'phí bảo hiểm' không hợp lệ.")
        return _fatal(errors)

    max_col = min(45, df.shape[1])
    het = df.iloc[s + 1: e - 3, [0, 1] + list(range(24, max_col))]
    con = df.iloc[e + 1: len(df) - 1, [0, 1] + list(range(24, max_col))]
    total_rows = len(het) + len(con)

    return {
        "ok": True,
        "errors": [],
        "stats": {
            "total_rows": total_rows,
            "date_errors": 0,
            "money_errors": 0,
            "duplicate_rows": 0,
        },
        "warnings": [],
        "df": pd.concat([het, con]).reset_index(drop=True),
    }


# ===========================================================================
# XCG / PA_NNTX validator  (mirrors lines 335-466 in 1.check_input.R)
# ===========================================================================

def _validate_xcg(df: pd.DataFrame, group_code: str) -> Dict[str, Any]:
    warnings: List[str] = []

    df = df.copy()
    df.columns = df.iloc[0].astype(str).str.strip()
    df = df.iloc[1:].reset_index(drop=True)

    # Drop junk rows
    df = df[df.isnull().sum(axis=1) <= (df.shape[1] - 4)]
    if df.shape[1] > 0:
        df = df[~df.iloc[:, 0].astype(str).str.contains("Tổng|Tong", case=False, na=False)]
    df = df.reset_index(drop=True)

    total_rows = len(df)
    if total_rows == 0:
        return _err("File không có dữ liệu hợp lệ sau khi làm sạch.")

    # ── Date validation ──────────────────────────────────────────────────────
    # R lines 341-374: for each NGAY_ col, build _Hop_Le per triple,
    # then Check_Ngay_Hop_Le = "Ngày hợp lệ" or "Sai: <prefix>, ..."
    ngay_cols = [c for c in df.columns if re.search(r"NGAY_|_DAY$", str(c), re.IGNORECASE)]
    col_list  = list(df.columns)

    _BLANK = {"", "none", "nan", "nat", "null", "na"}
    def _to_num_series(s):
        s_str = s.astype(str).str.strip().str.lower()
        s_str = s_str.replace(list(_BLANK), np.nan)
        return pd.to_numeric(s_str, errors='coerce')

    # Intermediate per-triple validity series
    hop_le_map: Dict[str, pd.Series] = {}
    for ngay_col in ngay_cols:
        try:
            idx = col_list.index(ngay_col)
        except ValueError:
            continue
        if idx + 2 < len(col_list):
            thang_col = col_list[idx + 1]
            nam_col   = col_list[idx + 2]
            prefix    = re.sub(r"(NGAY_|_DAY)$", "", ngay_col, flags=re.IGNORECASE)
            
            d = _to_num_series(df[ngay_col])
            m = _to_num_series(df[thang_col])
            y = _to_num_series(df[nam_col])

            is_all_null = d.isna() & m.isna() & y.isna()
            is_all_zero = (d == 0) & (m == 0) & (y == 0)
            is_any_null = d.isna() | m.isna() | y.isna()

            d_int = d.fillna(1).astype(int)
            m_int = m.fillna(1).astype(int)
            y_int = y.fillna(2000).astype(int)

            valid_range = (y_int >= 1) & (y_int <= 9999) & (m_int >= 1) & (m_int <= 12) & (d_int >= 1) & (d_int <= 31)
            
            date_str = (
                y_int.astype(str).str.zfill(4) + '-' +
                m_int.astype(str).str.zfill(2) + '-' +
                d_int.astype(str).str.zfill(2)
            )
            
            parsed_dates = pd.to_datetime(date_str, format='%Y-%m-%d', errors='coerce')
            is_valid_date = parsed_dates.notna() & valid_range & (parsed_dates.dt.day == d_int)

            valid_series = is_all_null | (is_all_zero & ~is_any_null) | (~is_any_null & is_valid_date)
            hop_le_map[prefix] = valid_series

    if hop_le_map:
        errors_combined = np.array([""] * total_rows, dtype=object)
        for prefix, is_valid in hop_le_map.items():
            errors_combined = np.where(
                ~is_valid,
                np.where(errors_combined == "", prefix, errors_combined + ", " + prefix),
                errors_combined
            )
        df["Check_Ngay_Hop_Le"] = np.where(errors_combined == "", "Ngày hợp lệ", "Sai: " + errors_combined)
    else:
        df["Check_Ngay_Hop_Le"] = "Ngày hợp lệ"

    date_errors = int((df["Check_Ngay_Hop_Le"] != "Ngày hợp lệ").sum())

    # ── Money validation ─────────────────────────────────────────────────────
    # R lines 383-405: grep("_So_tien|PHI_PS"), per-col _check, then Check_So_Tien
    tien_cols = [c for c in df.columns if re.search(r"_SO_TIEN|PHI_PS",
                                                      str(c), re.IGNORECASE)]
    check_money_results: Dict[str, pd.Series] = {}
    for col in tien_cols:
        s = df[col].astype(str).str.strip()
        is_empty = s.isin(["", "None", "nan", "NaN", "NA"]) | df[col].isna()
        has_alpha = s.str.contains(r"[A-Za-z]", regex=True, na=False)
        has_invalid = s.str.contains(r"[^0-9,.\-]", regex=True, na=False)
        
        clean_s = s.str.replace(",", "", regex=False)
        converted = pd.to_numeric(clean_s, errors='coerce')
        is_convertible = converted.notna() | is_empty
        
        condlist = [is_empty, has_alpha, has_invalid, ~is_convertible]
        choicelist = ["Hợp lệ", "Chứa chữ", "Ký tự không hợp lệ", "Không chuyển được sang số"]
        check_money_results[col] = np.select(condlist, choicelist, default="Hợp lệ")

    if check_money_results:
        errors_combined_money = np.array([""] * total_rows, dtype=object)
        for col, status_series in check_money_results.items():
            is_valid_money = status_series == "Hợp lệ"
            errors_combined_money = np.where(
                ~is_valid_money,
                np.where(errors_combined_money == "", col, errors_combined_money + ", " + col),
                errors_combined_money
            )
        df["Check_So_Tien"] = np.where(errors_combined_money == "", "Số tiền hợp lệ", "Sai: " + errors_combined_money)
    else:
        df["Check_So_Tien"] = "Số tiền hợp lệ"

    money_errors = int((df["Check_So_Tien"] != "Số tiền hợp lệ").sum())

    # ── Duplicate detection (XCG) ─────────────────────────────────────────────
    # R lines 432-465: for each PHI_THUC_THU col, group by check_cols,
    # keep groups with >1 non-NA phi, flag duplicates, anti_join for next round.
    # Then semi_join with original df and mutate check_trung = "Trùng".
    # Non-matching rows get check_trung = 0.
    check_cols = [c for c in XCG_CHECK_COLS if c in df.columns]
    phi_cols   = [c for c in df.columns if re.search(r"PHI_THUC_THU", str(c), re.IGNORECASE)]

    df_for_check = df.copy()
    for pc in phi_cols:
        df_for_check[pc] = pd.to_numeric(
            df_for_check[pc].astype(str).str.replace(",", "", regex=False),
            errors="coerce"
        )
        df_for_check.loc[df_for_check[pc] == 0, pc] = None

    dup_rows_idx: set = set()
    remaining = df_for_check.copy()

    for phi_col in phi_cols:
        if phi_col not in remaining.columns or not check_cols:
            continue
        all_cols  = check_cols + [phi_col]
        avail     = [c for c in all_cols if c in remaining.columns]
        group_keys = [c for c in check_cols if c in remaining.columns]
        if not avail or not group_keys:
            continue

        grouped_count = remaining.groupby(group_keys, dropna=False)[phi_col].transform(
            lambda x: x.notna().sum()
        )
        candidates = remaining[grouped_count > 1]
        avail_key  = [c for c in avail if c in candidates.columns]
        if not avail_key:
            continue

        is_dup = candidates.duplicated(subset=avail_key, keep=False)
        dups   = candidates[is_dup]
        dup_rows_idx.update(dups.index.tolist())
        remaining = remaining.drop(index=dups.index, errors="ignore")

    # Mirror R's semi_join logic: any row in the original df that shares the exact check_cols
    # with any flagged duplicate row should be marked as "Trùng".
    if dup_rows_idx and check_cols:
        dup_keys = df.loc[list(dup_rows_idx), check_cols].drop_duplicates()
        merged_keys = df.reset_index().merge(dup_keys, on=check_cols, how="inner")
        semi_join_indices = set(merged_keys["index"])
    else:
        semi_join_indices = set()

    duplicate_rows = len(semi_join_indices)

    # Mirror R: check_trung = "Trùng" or 0 (integer)
    df["check_trung"] = [
        "Trùng" if i in semi_join_indices else 0
        for i in df.index
    ]

    # ── Warnings (mirrors R output$info2 lines 719-721) ──────────────────────
    if date_errors > 0:
        warnings.append(f"⚠️ Số dòng sai ngày: {date_errors} dòng")
    if money_errors > 0:
        warnings.append(f"💰 {money_errors} dòng có số tiền không hợp lệ.")
    if duplicate_rows > 0:
        warnings.append(f"🔁 Số dòng trùng: {duplicate_rows} dòng")

    return {
        "ok": True,
        "errors": [],
        "stats": {
            "total_rows": total_rows,
            "date_errors": date_errors,
            "money_errors": money_errors,
            "duplicate_rows": duplicate_rows,
        },
        "warnings": warnings,
        "df": df,
    }


# ===========================================================================
# General LT/ST validator  (mirrors 'else' branch, lines 469-678)
# ===========================================================================

def _validate_general(df: pd.DataFrame, group_code: str) -> Dict[str, Any]:
    warnings: List[str] = []
    gc = group_code or ""

    # ── Determine schema ──────────────────────────────────────────────────────
    schema_key    = _get_schema_key(gc)
    expected_cols: List[str] = _schemas.get(schema_key, _schemas.get("Eng_LT_Pre", []))
    if not expected_cols:
        expected_cols = _schemas.get("Eng_LT_Pre", [])

    work = df.copy()

    # kcare: drop cols 12-14 (0-based 11,12,13), insert 3 NA at pos 21
    if re.search(r"kcare", gc, re.IGNORECASE):
        work = work.drop(work.columns[[11, 12, 13]], axis=1)
        work.columns = range(work.shape[1])
        for i in range(3):
            work.insert(21 + i, f"_kc_{i}", None)
        work.columns = range(work.shape[1])

    # CTTV/BHTT: insert 2 NA at pos 5, then 2 NA at pos 9
    elif re.search(r"CTTV|BHTT", gc, re.IGNORECASE):
        for i in range(2):
            work.insert(5 + i, f"_p1_{i}", None)
        work.columns = range(work.shape[1])
        for i in range(2):
            work.insert(9 + i, f"_p2_{i}", None)
        work.columns = range(work.shape[1])
        expected_cols = expected_cols[:36]

    # ── Find header row ───────────────────────────────────────────────────────
    header_idx, work = _find_header_row(work)
    if header_idx is None:
        return _err("Không tìm thấy dòng header hợp lệ (tìm 'Ngày' ở cột 12). Form không đúng định dạng.")

    data = work.iloc[header_idx + 1:].reset_index(drop=True)

    # ── Pad / trim to schema width ────────────────────────────────────────────
    n_data, n_exp = data.shape[1], len(expected_cols)
    if n_data < n_exp:
        warnings.append(f"Dữ liệu có ÍT cột hơn chuẩn ({n_data} < {n_exp}). Sẽ thêm NA.")
        for i in range(n_exp - n_data):
            data[f"_pad_{i}"] = None
    elif n_data > n_exp:
        warnings.append(f"Dữ liệu có NHIỀU cột hơn chuẩn ({n_data} > {n_exp}). Sẽ cắt bớt.")
        data = data.iloc[:, :n_exp]

    data.columns = expected_cols

    # ── Drop junk rows ────────────────────────────────────────────────────────
    data = data[data.isnull().sum(axis=1) <= (data.shape[1] - 4)]
    data = data[~data.iloc[:, 0].astype(str).str.contains("Tổng|Tong", case=False, na=False)]
    data = data.reset_index(drop=True)

    total_rows = len(data)
    if total_rows == 0:
        return _err("File không có dữ liệu hợp lệ sau khi làm sạch.")

    # ── Date validation ───────────────────────────────────────────────────────
    # R lines 587-616: cols ending _Ngay, paired with _Thang and _Nam
    # Check_Ngay_Hop_Le = "Ngày hợp lệ" or "Sai: <prefix>, ..."
    ngay_cols = [c for c in data.columns if c.endswith("_Ngay")]
    hop_le_map: Dict[str, pd.Series] = {}

    _BLANK = {"", "none", "nan", "nat", "null", "na"}
    def _to_num_series(s):
        s_str = s.astype(str).str.strip().str.lower()
        s_str = s_str.replace(list(_BLANK), np.nan)
        return pd.to_numeric(s_str, errors='coerce')

    for ngay_col in ngay_cols:
        prefix    = ngay_col[:-len("_Ngay")]
        thang_col = f"{prefix}_Thang"
        nam_col   = f"{prefix}_Nam"
        if thang_col in data.columns and nam_col in data.columns:
            d = _to_num_series(data[ngay_col])
            m = _to_num_series(data[thang_col])
            y = _to_num_series(data[nam_col])

            is_all_null = d.isna() & m.isna() & y.isna()
            is_all_zero = (d == 0) & (m == 0) & (y == 0)
            is_any_null = d.isna() | m.isna() | y.isna()

            d_int = d.fillna(1).astype(int)
            m_int = m.fillna(1).astype(int)
            y_int = y.fillna(2000).astype(int)

            valid_range = (y_int >= 1) & (y_int <= 9999) & (m_int >= 1) & (m_int <= 12) & (d_int >= 1) & (d_int <= 31)
            
            date_str = (
                y_int.astype(str).str.zfill(4) + '-' +
                m_int.astype(str).str.zfill(2) + '-' +
                d_int.astype(str).str.zfill(2)
            )
            
            parsed_dates = pd.to_datetime(date_str, format='%Y-%m-%d', errors='coerce')
            is_valid_date = parsed_dates.notna() & valid_range & (parsed_dates.dt.day == d_int)

            valid_series = is_all_null | (is_all_zero & ~is_any_null) | (~is_any_null & is_valid_date)
            hop_le_map[prefix] = valid_series

    if hop_le_map:
        errors_combined = np.array([""] * total_rows, dtype=object)
        for prefix, is_valid in hop_le_map.items():
            errors_combined = np.where(
                ~is_valid,
                np.where(errors_combined == "", prefix, errors_combined + ", " + prefix),
                errors_combined
            )
        data["Check_Ngay_Hop_Le"] = np.where(errors_combined == "", "Ngày hợp lệ", "Sai: " + errors_combined)
    else:
        data["Check_Ngay_Hop_Le"] = "Ngày hợp lệ"

    date_errors = int((data["Check_Ngay_Hop_Le"] != "Ngày hợp lệ").sum())

    # ── Money validation ──────────────────────────────────────────────────────
    # R lines 620-642: grep("_So_tien"), per-col _check, then Check_So_Tien
    tien_cols = [c for c in data.columns if "_So_tien" in c]
    check_money_results: Dict[str, np.ndarray] = {}
    for col in tien_cols:
        s = data[col].astype(str).str.strip()
        is_empty = s.isin(["", "None", "nan", "NaN", "NA"]) | data[col].isna()
        has_alpha = s.str.contains(r"[A-Za-z]", regex=True, na=False)
        has_invalid = s.str.contains(r"[^0-9,.\-]", regex=True, na=False)
        
        clean_s = s.str.replace(",", "", regex=False)
        converted = pd.to_numeric(clean_s, errors='coerce')
        is_convertible = converted.notna() | is_empty
        
        condlist = [is_empty, has_alpha, has_invalid, ~is_convertible]
        choicelist = ["Hợp lệ", "Chứa chữ", "Ký tự không hợp lệ", "Không chuyển được sang số"]
        check_money_results[col] = np.select(condlist, choicelist, default="Hợp lệ")

    if check_money_results:
        errors_combined_money = np.array([""] * total_rows, dtype=object)
        for col, status_series in check_money_results.items():
            is_valid_money = status_series == "Hợp lệ"
            errors_combined_money = np.where(
                ~is_valid_money,
                np.where(errors_combined_money == "", col, errors_combined_money + ", " + col),
                errors_combined_money
            )
        data["Check_So_Tien"] = np.where(errors_combined_money == "", "Số tiền hợp lệ", "Sai: " + errors_combined_money)
    else:
        data["Check_So_Tien"] = "Số tiền hợp lệ"

    money_errors = int((data["Check_So_Tien"] != "Số tiền hợp lệ").sum())

    # ── Duplicate detection ───────────────────────────────────────────────────
    # R lines 651-678: duplicated(across(check_cols)) | duplicated(fromLast=TRUE)
    # check_trung = "Trùng" or 0 (integer)
    avail_dup = [c for c in GENERAL_DUP_COLS if c in data.columns]
    if avail_dup:
        dup_mask = data.duplicated(subset=avail_dup, keep=False)
        duplicate_rows = int(dup_mask.sum())
        data["check_trung"] = dup_mask.map({True: "Trùng", False: 0})
    else:
        duplicate_rows = 0
        data["check_trung"] = 0

    # ── Warnings ─────────────────────────────────────────────────────────────
    if date_errors > 0:
        warnings.append(f"⚠️ Số dòng sai ngày: {date_errors} dòng")
    if money_errors > 0:
        warnings.append(f"💰 {money_errors} dòng có số tiền không hợp lệ.")
    if duplicate_rows > 0:
        warnings.append(f"🔁 Số dòng trùng: {duplicate_rows} dòng")

    return {
        "ok": True,
        "errors": [],
        "stats": {
            "total_rows": total_rows,
            "date_errors": date_errors,
            "money_errors": money_errors,
            "duplicate_rows": duplicate_rows,
        },
        "warnings": warnings,
        "df": data,
    }


# ===========================================================================
# Helpers
# ===========================================================================

def _err(msg: str) -> Dict[str, Any]:
    return {"ok": False, "errors": [msg], "stats": {}, "warnings": []}


def _fatal(errors: List[str]) -> Dict[str, Any]:
    return {"ok": False, "errors": errors, "stats": {}, "warnings": []}


def _get_schema_key(group_code: str) -> str:
    """Map group_code → schemas.json key, mirroring R's rds_name logic."""
    gc = group_code or ""

    for key in _schemas:
        if key.lower() == gc.lower():
            return key

    for suffix in ("_LT_Pre", "_ST_Pre", "_Pre"):
        candidate = gc + suffix
        if candidate in _schemas:
            return candidate

    if re.search(r"Marine", gc, re.IGNORECASE):
        return "7_term"

    if re.search(r"fire_st", gc, re.IGNORECASE):
        return "Eng_LT_Pre"

    return "Eng_LT_Pre"
