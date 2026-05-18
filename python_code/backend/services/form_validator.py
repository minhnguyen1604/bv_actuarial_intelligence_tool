import os
import re
import json
import calendar
import unicodedata
import pandas as pd
from typing import List, Dict, Any, Optional

# ---------------------------------------------------------------------------
# Load column schemas (mirrors pre_data/*.rds in R)
# ---------------------------------------------------------------------------
SCHEMAS_PATH = os.path.join(os.path.dirname(__file__), "..", "schemas.json")
_schemas: Dict[str, List[str]] = {}
if os.path.exists(SCHEMAS_PATH):
    with open(SCHEMAS_PATH, "r", encoding="utf-8") as f:
        _schemas = json.load(f)

# ---------------------------------------------------------------------------
# General-purpose key columns for duplicate detection (matches 1.check_input.R)
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

XCG_DUP_COLS = [
    "BKS",
    "LOAI_HINH_NAME",
    "NGAY_HIEU_LUC_TU",
    "THANG_HIEU_LUC_TU",
    "NAM_HIEU_LUC_TU",
    "NGAY_HIEU_LUC_DEN",
    "THANG_HIEU_LUC_DEN",
    "NAM_HIEU_LUC_DEN",
    "SO_TIEN_BH",
    "LOAI_TIEN_BH",
    "SUM(PHI_BAO_HIEM)",
    "BILLING_CURRENCY",
]

# ---------------------------------------------------------------------------
# Helper: remove Vietnamese accents (mirrors remove_accents in R)
# ---------------------------------------------------------------------------
def _remove_accents(text: str) -> str:
    if not text:
        return ""
    nfkd = unicodedata.normalize("NFKD", str(text))
    return "".join(c for c in nfkd if not unicodedata.combining(c))


# ---------------------------------------------------------------------------
# Helper: validate a date triple (mirrors kiem_tra_ngay_hop_le in R)
# ---------------------------------------------------------------------------
def _is_valid_date(day, month, year) -> bool:
    """Return True when d/m/y form a real calendar date, or when all are blank/0."""
    _BLANK = {"", "none", "nan", "nat", "null"}

    def _parse(v):
        s = str(v).strip().lower()
        if s in _BLANK:
            return None
        try:
            return int(float(s))
        except (ValueError, TypeError):
            return None

    d, m, y = _parse(day), _parse(month), _parse(year)

    # All blank → treat as valid (same as R: all NA → TRUE)
    if d is None and m is None and y is None:
        return True
    # All zero → valid
    if d == 0 and m == 0 and y == 0:
        return True
    # Partial fill → invalid
    if None in (d, m, y):
        return False

    try:
        if m < 1 or m > 12:
            return False
        max_day = calendar.monthrange(y, m)[1]
        return 1 <= d <= max_day
    except Exception:
        return False


# ---------------------------------------------------------------------------
# Helper: validate a money string (mirrors kiem_tra_so_tien in R)
# ---------------------------------------------------------------------------
def _is_valid_money(value) -> str:
    """Return 'Hợp lệ' or an error reason string."""
    s = str(value).strip() if value is not None else ""
    if s in ("", "None", "nan", "NaN"):
        return "Hợp lệ"  # blank is ok

    if re.search(r"[A-Za-z]", s):
        return "Chứa chữ"

    if re.search(r"[^0-9,.\-\(\) ]", s):
        return "Ký tự không hợp lệ"

    # Try to convert: strip thousand separators
    clean = re.sub(r"[,\s]", "", s)
    clean = clean.replace("(", "-").replace(")", "")
    try:
        float(clean)
        return "Hợp lệ"
    except ValueError:
        return "Không chuyển được sang số"


# ---------------------------------------------------------------------------
# Helper: find the true data-start row in general forms
# (mirrors the while-loop in 1.check_input.R that looks for "Ngày" in col 12)
# ---------------------------------------------------------------------------
def _find_header_row(df: pd.DataFrame) -> Optional[int]:
    """
    Scan column index 11 (0-based, = col 12 in R 1-based) for a cell
    whose text starts with 'Ngay' or 'Ngày'. Return the row index of the
    first data row (header row + 1) in the raw DataFrame, or None.
    """
    work = df.copy()

    # If col 10 (0-based) already has "Ngay/Ngày" → insert 2 blank cols at pos 5-6 (R logic)
    if work.shape[1] >= 10:
        col10_vals = work.iloc[:, 9].astype(str)
        if col10_vals.str.match(r"^Ng[aà]y", case=False).any():
            left = work.iloc[:, :5]
            blank2 = pd.DataFrame(
                [[None] * 2] * len(work), columns=["_pad1", "_pad2"], index=work.index
            )
            right = work.iloc[:, 5:]
            work = pd.concat([left, blank2, right], axis=1)
            work.columns = range(work.shape[1])

    # Iteratively drop first column until col-12 (0-based 11) has "Ngay/Ngày"
    max_drops = work.shape[1] - 12
    for _ in range(max(0, max_drops) + 1):
        if work.shape[1] < 12:
            break
        col12 = work.iloc[:, 11].astype(str)
        matches = col12.str.match(r"^Ng[aà]y", case=False)
        if matches.any():
            header_row = matches.idxmax()  # first matching row index
            return header_row  # caller will do +1 to get data start
        # Drop first column and retry
        work = work.iloc[:, 1:]
        work.columns = range(work.shape[1])

    return None


# ---------------------------------------------------------------------------
# Helper: check duplicates in a DataFrame
# ---------------------------------------------------------------------------
def _check_duplicates(df: pd.DataFrame, key_cols: List[str]) -> pd.Series:
    """Return a boolean Series: True where row is a duplicate among key_cols."""
    available = [c for c in key_cols if c in df.columns]
    if not available:
        return pd.Series([False] * len(df), index=df.index)
    dup_mask = df.duplicated(subset=available, keep=False)
    return dup_mask


# ===========================================================================
# Public API
# ===========================================================================

def validate_form(file_path: str, sheet_name: str, group_code: str) -> Dict[str, Any]:
    """
    Validate a specific sheet in an Excel file based on group_code rules.

    Returns:
        {
            "ok": bool,
            "errors": [str, ...],          # fatal structural errors
            "stats": {
                "total_rows": int,
                "date_errors": int,
                "money_errors": int,
                "duplicate_rows": int,
            },
            "warnings": [str, ...]         # non-fatal issues shown to user
        }
    """
    # --- 1. Read file ---
    try:
        result = pd.read_excel(file_path, sheet_name=sheet_name, header=None, dtype=str)
        # read_excel can return a dict if sheet_name matches multiple sheets
        if isinstance(result, dict):
            if not result:
                return _err("Không đọc được sheet Excel.")
            df = next(iter(result.values()))
        else:
            df = result
    except Exception as e:
        return _err(f"Không đọc được file Excel: {e}")

    if not isinstance(df, pd.DataFrame) or df.empty:
        return _err("File rỗng.")

    # --- 2. Route to appropriate validator ---
    gc = group_code or ""
    if gc == "Vietjet":
        return _validate_vietjet(df)
    elif re.search(r"XCG|PA_NNTX", gc, re.IGNORECASE):
        return _validate_xcg(df, gc)
    else:
        return _validate_general(df, gc)


# ---------------------------------------------------------------------------
# Internal validators
# ---------------------------------------------------------------------------

def _err(msg: str) -> Dict[str, Any]:
    return {"ok": False, "errors": [msg], "stats": {}, "warnings": []}


def _validate_vietjet(df: pd.DataFrame) -> Dict[str, Any]:
    """Mirrors check_form_vietjet() in 1.check_input.R"""
    errors = []
    warnings = []

    if df.shape[1] < 25:
        errors.append(f"File không đủ 25 cột (hiện có {df.shape[1]}).")
        return {"ok": False, "errors": errors, "stats": {}, "warnings": warnings}

    # Find 2 rows containing "phí bảo hiểm" in column 25 (index 24)
    col25 = df.iloc[:, 24].astype(str)
    chi_so = col25[col25.str.contains("phí bảo hiểm", case=False, na=False)].index.tolist()

    if len(chi_so) < 2:
        errors.append("Không tìm thấy đủ 2 dòng chứa 'phí bảo hiểm' trong cột 25.")
        return {"ok": False, "errors": errors, "stats": {}, "warnings": warnings}

    s, e = chi_so[0], chi_so[1]

    if (s + 1) > (e - 4) or (e + 1) > (len(df) - 1):
        errors.append("Vị trí các dòng 'phí bảo hiểm' không hợp lệ.")
        return {"ok": False, "errors": errors, "stats": {}, "warnings": warnings}

    max_col = min(45, df.shape[1])
    het_hieu_luc = df.iloc[s + 1 : e - 3, [0, 1] + list(range(24, max_col))]
    con_hieu_luc = df.iloc[e + 1 : len(df) - 1, [0, 1] + list(range(24, max_col))]

    total_rows = len(het_hieu_luc) + len(con_hieu_luc)

    return {
        "ok": True,
        "errors": [],
        "stats": {
            "total_rows": total_rows,
            "date_errors": 0,
            "money_errors": 0,
            "duplicate_rows": 0,
        },
        "warnings": warnings,
    }


def _validate_xcg(df: pd.DataFrame, group_code: str) -> Dict[str, Any]:
    """Mirrors XCG/PA_NNTX branch in 1.check_input.R"""
    errors = []
    warnings = []

    # XCG files typically have headers in row 0 already
    # Try to use first row as header
    df.columns = df.iloc[0]
    df = df.iloc[1:].reset_index(drop=True)

    # Drop rows with too many NAs and rows containing "Tổng"
    df = df[df.isnull().sum(axis=1) <= (df.shape[1] - 4)]
    if df.shape[1] > 0:
        df = df[~df.iloc[:, 0].astype(str).str.contains("Tổng|Tong", case=False, na=False)]

    df = df.reset_index(drop=True)
    total_rows = len(df)

    if total_rows == 0:
        return _err("File không có dữ liệu hợp lệ sau khi làm sạch.")

    # --- Date validation ---
    date_errors = 0
    ngay_cols = [c for c in df.columns if re.search(r"NGAY_|_DAY", str(c), re.IGNORECASE)]
    for ngay_col in ngay_cols:
        col_list = list(df.columns)
        idx = col_list.index(ngay_col)
        if idx + 2 < len(col_list):
            thang_col = col_list[idx + 1]
            nam_col = col_list[idx + 2]
            invalid = df.apply(
                lambda row: not _is_valid_date(row[ngay_col], row[thang_col], row[nam_col]),
                axis=1,
            )
            date_errors += int(invalid.sum())

    # --- Money validation ---
    money_errors = 0
    tien_cols = [c for c in df.columns if re.search(r"_SO_TIEN|PHI_PS|PHI_THUC_THU", str(c), re.IGNORECASE)]
    for col in tien_cols:
        invalid = df[col].apply(lambda v: _is_valid_money(v) != "Hợp lệ")
        money_errors += int(invalid.sum())

    # --- Duplicate detection ---
    dup_mask = _check_duplicates(df, XCG_DUP_COLS)
    duplicate_rows = int(dup_mask.sum())

    if date_errors > 0:
        warnings.append(f"⚠️ {date_errors} dòng có ngày không hợp lệ.")
    if money_errors > 0:
        warnings.append(f"💰 {money_errors} dòng có số tiền không hợp lệ.")
    if duplicate_rows > 0:
        warnings.append(f"🔁 {duplicate_rows} dòng trùng lặp.")

    return {
        "ok": True,  # warnings, not fatal errors
        "errors": errors,
        "stats": {
            "total_rows": total_rows,
            "date_errors": date_errors,
            "money_errors": money_errors,
            "duplicate_rows": duplicate_rows,
        },
        "warnings": warnings,
    }


def _validate_general(df: pd.DataFrame, group_code: str) -> Dict[str, Any]:
    """
    Mirrors the 'else' branch in 1.check_input.R for general LT/ST forms.
    Steps:
      1. Find real header row (look for 'Ngày' in col 12)
      2. Rename columns using schema
      3. Validate dates (_Ngay / _Thang / _Nam triplets)
      4. Validate money (_So_tien columns)
      5. Detect duplicates
    """
    errors = []
    warnings = []

    # Determine expected column schema
    schema_key = _get_schema_key(group_code)
    expected_cols: List[str] = _schemas.get(schema_key, _schemas.get("Eng_LT_Pre", []))

    if not expected_cols:
        warnings.append(f"Không tìm thấy schema cho '{group_code}', dùng Eng_LT_Pre mặc định.")
        expected_cols = _schemas.get("Eng_LT_Pre", [])

    # --- Find true header row ---
    header_row_idx = _find_header_row(df)
    if header_row_idx is None:
        return _err(
            "Không tìm thấy dòng header hợp lệ (tìm 'Ngày' ở cột 12). Form không đúng định dạng."
        )

    # Slice from the row AFTER the header marker
    data = df.iloc[header_row_idx + 1 :].reset_index(drop=True)

    # --- Pad / trim columns to match schema ---
    n_data = data.shape[1]
    n_expected = len(expected_cols)
    if n_data < n_expected:
        warnings.append(f"Dữ liệu có ÍT cột hơn chuẩn ({n_data} < {n_expected}). Sẽ thêm NA.")
        for i in range(n_expected - n_data):
            data[f"_pad_{i}"] = None
    elif n_data > n_expected:
        warnings.append(f"Dữ liệu có NHIỀU cột hơn chuẩn ({n_data} > {n_expected}). Sẽ cắt bớt.")
        data = data.iloc[:, :n_expected]

    data.columns = expected_cols

    # --- Drop junk rows (too many NAs, or contains "Tổng") ---
    data = data[data.isnull().sum(axis=1) <= (data.shape[1] - 4)]
    data = data[~data.iloc[:, 0].astype(str).str.contains("Tổng|Tong", case=False, na=False)]
    data = data.reset_index(drop=True)

    total_rows = len(data)
    if total_rows == 0:
        return _err("File không có dữ liệu hợp lệ sau khi làm sạch.")

    # --- Date validation ---
    date_errors = 0
    ngay_cols = [c for c in data.columns if c.endswith("_Ngay")]
    if ngay_cols:
        row_has_date_error = pd.Series([False] * len(data), index=data.index)
        for ngay_col in ngay_cols:
            prefix = ngay_col[: -len("_Ngay")]
            thang_col = f"{prefix}_Thang"
            nam_col = f"{prefix}_Nam"
            if thang_col in data.columns and nam_col in data.columns:
                invalid = data.apply(
                    lambda row, nc=ngay_col, tc=thang_col, yc=nam_col: not _is_valid_date(
                        row[nc], row[tc], row[yc]
                    ),
                    axis=1,
                )
                row_has_date_error = row_has_date_error | invalid
        date_errors = int(row_has_date_error.sum())

    # --- Money validation ---
    money_errors = 0
    tien_cols = [c for c in data.columns if "_So_tien" in c]
    for col in tien_cols:
        invalid = data[col].apply(lambda v: _is_valid_money(v) != "Hợp lệ")
        money_errors += int(invalid.sum())

    # --- Duplicate detection ---
    dup_mask = _check_duplicates(data, GENERAL_DUP_COLS)
    duplicate_rows = int(dup_mask.sum())

    if date_errors > 0:
        warnings.append(f"⚠️ {date_errors} dòng có ngày không hợp lệ.")
    if money_errors > 0:
        warnings.append(f"💰 {money_errors} dòng có số tiền không hợp lệ.")
    if duplicate_rows > 0:
        warnings.append(f"🔁 {duplicate_rows} dòng trùng lặp.")

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
    }


def _get_schema_key(group_code: str) -> str:
    """Map a group_code to the corresponding schemas.json key."""
    gc = group_code or ""
    # Direct match first
    for key in _schemas:
        if key.lower() == gc.lower():
            return key

    # Fuzzy: strip trailing _LT/_ST and try with suffixes
    for suffix in ("_LT_Pre", "_ST_Pre", "_Pre"):
        candidate = gc + suffix
        if candidate in _schemas:
            return candidate

    # Marine uses 7_term
    if re.search(r"Marine", gc, re.IGNORECASE):
        return "7_term"

    return "Eng_LT_Pre"  # safe fallback
