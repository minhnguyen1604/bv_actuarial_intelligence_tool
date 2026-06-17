import os
import re
import math
import sqlite3
import pandas as pd
import numpy as np

# Root path for historical database
DB_PATH = r"d:\bv_intelligence_tool\python_code\backend\data\vas_analysis.db"

# Mapping table equivalent to line_map in start.R
LINE_MAP = {
    "Motor Vehicles": {"nghiep_vu": "Xe cơ giới", "nhom_nv": "Nhóm XCG, YT, CN"},
    "Healthcare + Travel": {"nghiep_vu": "Y tế + Du lịch", "nhom_nv": "Nhóm XCG, YT, CN"},
    "Travel": {"nghiep_vu": "Du lịch", "nhom_nv": "Nhóm XCG, YT, CN"},
    "Healthcare": {"nghiep_vu": "Y tế", "nhom_nv": "Nhóm XCG, YT, CN"},
    "Personal Accident": {"nghiep_vu": "Con người", "nhom_nv": "Nhóm XCG, YT, CN"},
    "HC & PA & Travel": {"nghiep_vu": "YT & CN & DL", "nhom_nv": "Nhóm XCG, YT, CN"},
    "Cargo in transit": {"nghiep_vu": "Hàng hóa", "nhom_nv": "Nhóm Tàu, hàng"},
    "Hull & PI": {"nghiep_vu": "Thân tàu", "nhom_nv": "Nhóm Tàu, hàng"},
    "Aviation & Oil": {"nghiep_vu": "DK & HK", "nhom_nv": "Nhóm DKKH, NN"},
    "Fire and Misc.": {"nghiep_vu": "Tài sản", "nhom_nv": "Nhóm TS, KT, TN"},
    "Engineering": {"nghiep_vu": "Kỹ thuật", "nhom_nv": "Nhóm TS, KT, TN"},
    "General Liability": {"nghiep_vu": "Trách nhiệm", "nhom_nv": "Nhóm TS, KT, TN"},
    "Agriculture": {"nghiep_vu": "Nông nghiệp", "nhom_nv": "Nhóm DKKH, NN"},
    "Total": {"nghiep_vu": "Tổng", "nhom_nv": "Tổng"}
}

def normalize_line(line: str) -> str:
    if pd.isna(line):
        return ""
    l_lower = str(line).strip().lower()
    
    if re.search(r"^pa$|personal", l_lower):
        return "Personal Accident"
    elif re.search(r"pa.*health", l_lower):
        return "HC & PA & Travel"
    elif "health" in l_lower:
        return "Healthcare"
    elif "motor" in l_lower:
        return "Motor Vehicles"
    elif re.search(r"aviation|oil and gas", l_lower):
        return "Aviation & Oil"
    elif "agriculture" in l_lower:
        return "Agriculture"
    elif "cargo" in l_lower:
        return "Cargo in transit"
    elif "engineering" in l_lower:
        return "Engineering"
    elif "fire" in l_lower:
        return "Fire and Misc."
    elif re.search(r"liability|miscell", l_lower):
        return "General Liability"
    elif "hull" in l_lower:
        return "Hull & PI"
    elif "travel" in l_lower:
        return "Travel"
    elif "total" in l_lower:
        return "Total"
    else:
        return str(line).strip().title()

def to_numeric(val) -> float:
    if pd.isna(val) or val is None:
        return 0.0
    val_str = re.sub(r"[^0-9.-]", "", str(val))
    if not val_str or val_str == "-" or val_str == ".":
        return 0.0
    try:
        return float(val_str)
    except ValueError:
        return 0.0

def prev_quarter(quarter_str: str) -> str:
    # Expects format like "2025Q1"
    if len(quarter_str) < 6 or "Q" not in quarter_str:
        return quarter_str
    try:
        year = int(quarter_str[:4])
        quarter = int(quarter_str[5])
        if quarter == 1:
            year -= 1
            quarter = 4
        else:
            quarter -= 1
        return f"{year}Q{quarter}"
    except Exception:
        return quarter_str

def get_last_4_quarters(quarter_str: str) -> list:
    res = [quarter_str]
    q = quarter_str
    for _ in range(3):
        q = prev_quarter(q)
        res.append(q)
    return res

def check_form(df: pd.DataFrame) -> bool:
    """Checks if sheet1 has the required structure (Line, UPR, OSC, IBNR, CAT, Direct, Inward, etc.)"""
    # 1. Check if "Line" exists in headers or in the first column
    col0_str = df.iloc[:, 0].fillna("").astype(str).tolist()
    has_line = any(re.search("Line", col, re.IGNORECASE) for col in df.columns) or \
               any(re.search("Line", val, re.IGNORECASE) for val in col0_str)
    if not has_line:
        return False

    # 2. Check if types exist
    col_str_all = " ".join(df.columns.astype(str).tolist())
    for t in ["Direct", "Inward", "Recovery", "Retrocession", "Net"]:
        if not re.search(t, col_str_all, re.IGNORECASE) and not any(re.search(t, str(x), re.IGNORECASE) for x in df.values.flatten()):
            return False

    # 3. Check for core indicators
    found_upr = any(re.search("UPR", val, re.IGNORECASE) for val in col0_str)
    found_osc = any(re.search("OSC", val, re.IGNORECASE) for val in col0_str)
    found_ibnr = any(re.search("IBNR", val, re.IGNORECASE) for val in col0_str)
    found_cat = any(re.search("CAT", val, re.IGNORECASE) for val in col0_str)
    
    return found_upr and found_osc and found_ibnr and found_cat

def slice_line_to_total(df: pd.DataFrame, start_pat=r"\bLine\b", end_pat=r"\bTotal\b", col_index=0) -> pd.DataFrame:
    col1 = df.iloc[:, col_index].fillna("").astype(str).tolist()
    
    start_pos = -1
    for idx, val in enumerate(col1):
        if re.search(start_pat, val, re.IGNORECASE):
            start_pos = idx
            break
            
    if start_pos == -1:
        return pd.DataFrame()
        
    end_pos = -1
    for idx in range(start_pos, len(col1)):
        if re.search(end_pat, col1[idx], re.IGNORECASE):
            end_pos = idx
            break
            
    if end_pos == -1:
        end_pos = len(df) - 1
        
    headers = [str(x).strip() for x in df.iloc[start_pos].tolist()]
    sliced_df = df.iloc[start_pos + 1 : end_pos + 1].copy()
    sliced_df.columns = headers
    
    if len(sliced_df) > 0:
        sliced_df = sliced_df[sliced_df.iloc[:, 0].fillna("").astype(str).str.strip() != ""]
        
    return sliced_df

def process_block(df: pd.DataFrame, var: str) -> pd.DataFrame:
    sliced = slice_line_to_total(df)
    if sliced.empty:
        return pd.DataFrame(columns=["Line", "Type", var])
        
    first_col = sliced.columns[0]
    sliced = sliced.rename(columns={first_col: "Line"})
    sliced["Line"] = sliced["Line"].apply(normalize_line)
    
    type_cols = []
    for col in sliced.columns:
        c_upper = str(col).upper()
        if c_upper in ["DIRECT", "INWARD", "RECOVERY", "RETROCESSION"] or "NET" in c_upper:
            type_cols.append(col)
            
    melted = sliced.melt(id_vars=["Line"], value_vars=type_cols, var_name="Type", value_name=var)
    
    def clean_type(t):
        t_upper = str(t).upper()
        if "NET" in t_upper:
            return "NET"
        return t_upper
        
    melted["Type"] = melted["Type"].apply(clean_type)
    melted[var] = melted[var].apply(to_numeric)
    
    return melted

def process_block_cat(df: pd.DataFrame, var: str) -> pd.DataFrame:
    sliced = slice_line_to_total(df)
    if sliced.empty:
        return pd.DataFrame(columns=["Line", "Type", var])
        
    sliced = sliced.iloc[:, 0:2].copy()
    first_col = sliced.columns[0]
    second_col = sliced.columns[1]
    
    sliced = sliced.rename(columns={first_col: "Line", second_col: var})
    sliced["Line"] = sliced["Line"].apply(normalize_line)
    sliced["Type"] = "NET"
    sliced[var] = sliced[var].apply(to_numeric)
    
    return sliced[["Line", "Type", var]]

def parse_result_sheet(df: pd.DataFrame) -> pd.DataFrame:
    """Parses sheet1 (Result) and returns reserves: reserve_upr, reserve_osc, reserve_ibnr, reserve_cat"""
    col0 = df.iloc[:, 0].fillna("").astype(str).tolist()
    
    line_rows = [idx for idx, val in enumerate(col0) if re.search(r"Line", val, re.IGNORECASE)]
    if not line_rows:
        raise ValueError("Missing 'Line' column in sheet1.")
    header_row_idx = line_rows[0]
    
    header_row_vals = df.iloc[header_row_idx].fillna("").astype(str).tolist()
    type_cols_indices = []
    for idx, val in enumerate(header_row_vals):
        if idx == 0:
            continue
        if re.search(r"Direct|Inward|Recovery|Retro|Net", val, re.IGNORECASE):
            type_cols_indices.append(idx)
            
    df_subset = df.iloc[:, [0] + type_cols_indices[:5]].copy()
    
    col0_subset = df_subset.iloc[:, 0].fillna("").astype(str).tolist()
    starts = {}
    for key in ["upr", "osc", "ibnr", "cat"]:
        idx_list = [i for i, val in enumerate(col0_subset) if re.search(key, val, re.IGNORECASE)]
        if idx_list:
            starts[key] = idx_list[0]
            
    if len(starts) < 4:
        raise ValueError(f"Missing core reserve indicators in sheet1: {list(starts.keys())}")
        
    sorted_starts = sorted(starts.items(), key=lambda x: x[1])
    
    blocks = {}
    for idx, (key, start_row) in enumerate(sorted_starts):
        if idx < len(sorted_starts) - 1:
            end_row = sorted_starts[idx + 1][1] - 1
        else:
            end_row = len(df_subset) - 1
        blocks[key] = df_subset.iloc[start_row : end_row + 1].copy()
        
    upr_long = process_block(blocks["upr"], "reserve_upr")
    osc_long = process_block(blocks["osc"], "reserve_osc")
    ibnr_long = process_block(blocks["ibnr"], "reserve_ibnr")
    cat_long = process_block_cat(blocks["cat"], "reserve_cat")
    
    upr_df = upr_long.groupby(["Line", "Type"])["reserve_upr"].sum().reset_index()
    osc_df = osc_long.groupby(["Line", "Type"])["reserve_osc"].sum().reset_index()
    ibnr_df = ibnr_long.groupby(["Line", "Type"])["reserve_ibnr"].sum().reset_index()
    cat_df = cat_long.groupby(["Line", "Type"])["reserve_cat"].sum().reset_index()
    
    merged = pd.merge(upr_df, osc_df, on=["Line", "Type"], how="outer")
    merged = pd.merge(merged, ibnr_df, on=["Line", "Type"], how="outer")
    merged = pd.merge(merged, cat_df, on=["Line", "Type"], how="outer")
    merged = merged.fillna(0.0)
    
    # Handle missing "Travel" line
    unique_lines = merged["Line"].unique()
    if "Travel" not in unique_lines:
        types = merged["Type"].unique()
        travel_rows = []
        for t in types:
            travel_rows.append({
                "Line": "Travel",
                "Type": t,
                "reserve_upr": 0.0,
                "reserve_osc": 0.0,
                "reserve_ibnr": 0.0,
                "reserve_cat": 0.0
            })
        merged = pd.concat([merged, pd.DataFrame(travel_rows)], ignore_index=True)
        
    # Group "Healthcare" and "Travel" to make "Healthcare + Travel"
    hc_travel_rows = merged[merged["Line"].isin(["Healthcare", "Travel"])]
    hc_travel_sum = hc_travel_rows.groupby("Type").agg({
        "reserve_upr": "sum",
        "reserve_osc": "sum",
        "reserve_ibnr": "sum",
        "reserve_cat": "sum"
    }).reset_index()
    hc_travel_sum["Line"] = "Healthcare + Travel"
    
    merged = pd.concat([merged, hc_travel_sum], ignore_index=True)
    
    # Group again
    merged = merged.groupby(["Line", "Type"]).agg({
        "reserve_upr": "sum",
        "reserve_osc": "sum",
        "reserve_ibnr": "sum",
        "reserve_cat": "sum"
    }).reset_index()
    
    # Convert to Millions
    for col in ["reserve_upr", "reserve_osc", "reserve_ibnr", "reserve_cat"]:
        merged[col] = merged[col] / 1e6
        
    return merged

def clean_dtbt(df_block: pd.DataFrame) -> pd.DataFrame:
    keys = ["CODE", "PHIGOC", "NHAN", "NHUONG CUA GOC", "NHUONG CUA NHAN", "HOANPHI", "GIAMPHI", "PHI_GLAI"]
    existing_cols = [c for c in keys if c in df_block.columns]
    df_block = df_block[existing_cols].copy()
    
    new_names = ["Line", "DIRECT", "INWARD", "RECOVERY", "RETROCESSION", "HOANPHI", "GIAMPHI", "NET"]
    rename_dict = {col: new_names[i] for i, col in enumerate(existing_cols)}
    df_block = df_block.rename(columns=rename_dict)
    
    df_block = df_block.iloc[1:].copy()
    df_block = df_block[df_block["Line"].fillna("").astype(str).str.strip() != ""]
    
    col1_list = df_block["Line"].fillna("").astype(str).str.strip().tolist()
    vt = -1
    for idx, val in enumerate(col1_list):
        if val == "Z":
            vt = idx
            break
            
    if vt == 11:
        hehe = ["Cargo in transit", "Hull & PI", "Aviation & Oil", "Aviation & Oil",
                "Engineering", "Fire and Misc.", "General Liability", "Motor Vehicles",
                "Personal Accident", "Healthcare", "Agriculture"]
        df_block = df_block.iloc[0:11].copy()
        df_block["Line"] = hehe
        
        travel_row = {
            "Line": "Travel",
            "DIRECT": 0.0, "INWARD": 0.0, "RECOVERY": 0.0, "RETROCESSION": 0.0,
            "HOANPHI": 0.0, "GIAMPHI": 0.0, "NET": 0.0
        }
        df_block = pd.concat([df_block, pd.DataFrame([travel_row])], ignore_index=True)
    else:
        hehe = ["Cargo in transit", "Hull & PI", "Aviation & Oil", "Aviation & Oil",
                "Engineering", "Fire and Misc.", "General Liability", "Motor Vehicles",
                "Personal Accident", "Travel", "Healthcare", "Agriculture"]
        df_block = df_block.iloc[0:12].copy()
        df_block["Line"] = hehe
        
    metric_cols = ["DIRECT", "INWARD", "RECOVERY", "RETROCESSION", "HOANPHI", "GIAMPHI", "NET"]
    for col in metric_cols:
        if col in df_block.columns:
            df_block[col] = df_block[col].apply(to_numeric)
            
    df_block["DIRECT"] = df_block["DIRECT"] - df_block["HOANPHI"] - df_block["GIAMPHI"]
    
    final_cols = ["Line", "DIRECT", "INWARD", "RECOVERY", "RETROCESSION", "NET"]
    return df_block[final_cols]

def process_dtbt(df_clean: pd.DataFrame, ten: str) -> pd.DataFrame:
    for col in df_clean.columns:
        if col != "Line":
            df_clean[col] = df_clean[col].apply(to_numeric)
            
    groups = {
        "Healthcare + Travel": ["Healthcare", "Travel"],
        "HC & PA & Travel": ["Travel", "Healthcare", "Personal Accident"],
        "Total": df_clean["Line"].unique().tolist()
    }
    
    group_rows = []
    for group_name, lines in groups.items():
        sub_df = df_clean[df_clean["Line"].isin(lines)]
        sum_row = sub_df.iloc[:, 1:].sum()
        row_dict = sum_row.to_dict()
        row_dict["Line"] = group_name
        group_rows.append(row_dict)
        
    df2 = pd.concat([df_clean, pd.DataFrame(group_rows)], ignore_index=True)
    
    val_cols = ["DIRECT", "INWARD", "RECOVERY", "RETROCESSION", "NET"]
    df2_grouped = df2.groupby("Line").agg({col: "sum" for col in val_cols}).reset_index()
    for col in val_cols:
        df2_grouped[col] = df2_grouped[col] / 1e6
        
    melted = df2_grouped.melt(id_vars=["Line"], value_vars=val_cols, var_name="Type", value_name=ten)
    melted["Type"] = melted["Type"].str.upper()
    return melted

def parse_dtbt_sheet(df: pd.DataFrame) -> tuple:
    """Parses sheet2 (Doanh thu bồi thường) and returns (Written_df, Paid_df)"""
    keys = ["CODE", "PHIGOC", "NHAN", "NHUONG CUA GOC", "NHUONG CUA NHAN", "HOANPHI", "GIAMPHI", "PHI_GLAI"]
    
    header_row_idx = -1
    for idx, row in df.iterrows():
        row_vals = [str(x).strip() for x in row.tolist()]
        if any(val in keys for val in row_vals):
            header_row_idx = idx
            break
            
    if header_row_idx == -1:
        raise ValueError("Missing header keywords in sheet2.")
        
    header_row = [str(x).strip() for x in df.iloc[header_row_idx].tolist()]
    dtbt_clean = df.copy()
    dtbt_clean.columns = header_row
    
    matched_cols = [col for col in dtbt_clean.columns if col in keys]
    dtbt_clean = dtbt_clean[matched_cols]
    
    col0_clean = dtbt_clean.iloc[:, 0].fillna("").astype(str).tolist()
    bt_row_idx = -1
    for idx, val in enumerate(col0_clean):
        if idx <= header_row_idx:
            continue
        if re.search(r"^BT$", val, re.IGNORECASE) or val.strip().upper() == "BT":
            bt_row_idx = idx
            break
            
    if bt_row_idx == -1:
        raise ValueError("Missing 'BT' separator row in sheet2.")
        
    dt = dtbt_clean.iloc[header_row_idx : bt_row_idx].copy()
    bt = dtbt_clean.iloc[bt_row_idx + 1 :].copy()
    
    dt_cleaned = clean_dtbt(dt)
    bt_cleaned = clean_dtbt(bt)
    
    dt_final = process_dtbt(dt_cleaned, "Written")
    bt_final = process_dtbt(bt_cleaned, "Paid")
    
    return dt_final, bt_final

def calculate_quarterly_changes(df_joined: pd.DataFrame, prev_quarter_df: pd.DataFrame, is_q1: bool) -> pd.DataFrame:
    """Calculates UPR, OsC, Written, Paid changes for 'Theo quý' (Quarterly) report"""
    df_joined["Line"] = df_joined["Line"].str.strip()
    df_joined["Type"] = df_joined["Type"].str.strip()
    
    # 1. Reserves UPR/OsC changes
    if prev_quarter_df is not None and not prev_quarter_df.empty:
        prev_quarter_df["Line"] = prev_quarter_df["Line"].str.strip()
        prev_quarter_df["Type"] = prev_quarter_df["Type"].str.strip()
        
        prev_reserves = prev_quarter_df[["Line", "Type", "reserve_upr", "reserve_osc"]].copy()
        prev_reserves = prev_reserves.rename(columns={"reserve_upr": "reserve_upr_prev", "reserve_osc": "reserve_osc_prev"})
        
        merged = pd.merge(df_joined, prev_reserves, on=["Line", "Type"], how="left").fillna(0.0)
        merged["upr"] = merged["reserve_upr"] - merged["reserve_upr_prev"]
        merged["osc"] = merged["reserve_osc"] - merged["reserve_osc_prev"]
        # Drop columns
        merged = merged.drop(columns=["reserve_upr_prev", "reserve_osc_prev"])
    else:
        merged = df_joined.copy()
        merged["upr"] = merged["reserve_upr"]
        merged["osc"] = merged["reserve_osc"]
        
    # 2. Written/Paid changes
    if is_q1:
        # For Q1, quarterly values are exactly cumulative values
        pass
    else:
        # Subtract previous cumulative values
        if prev_quarter_df is not None and not prev_quarter_df.empty:
            prev_cum = prev_quarter_df[["Line", "Type", "written", "paid"]].copy()
            prev_cum = prev_cum.rename(columns={"written": "written_prev", "paid": "paid_prev"})
            
            merged = pd.merge(merged, prev_cum, on=["Line", "Type"], how="left").fillna(0.0)
            merged["written"] = merged["written"] - merged["written_prev"]
            merged["paid"] = merged["paid"] - merged["paid_prev"]
            # Drop columns
            merged = merged.drop(columns=["written_prev", "paid_prev"])
            
    merged["quy_luy_ke"] = "Theo quý"
    return merged

def calculate_cumulative_changes(df_compare_q: pd.DataFrame, prev_quarter_cum_df: pd.DataFrame, is_q1: bool) -> pd.DataFrame:
    """Calculates reserves change and joins cumulative premiums for 'Lũy kế' (Cumulative) report"""
    if is_q1:
        # For Q1, Cumulative = Quarterly
        merged = df_compare_q.copy()
        merged["quy_luy_ke"] = "Lũy kế"
        return merged
        
    # Q2-Q4
    df_compare_q["Line"] = df_compare_q["Line"].str.strip()
    df_compare_q["Type"] = df_compare_q["Type"].str.strip()
    
    # 1. reserves changes: UPR_cum = UPR_q + UPR_prev_cum
    if prev_quarter_cum_df is not None and not prev_quarter_cum_df.empty:
        prev_quarter_cum_df["Line"] = prev_quarter_cum_df["Line"].str.strip()
        prev_quarter_cum_df["Type"] = prev_quarter_cum_df["Type"].str.strip()
        
        prev_res = prev_quarter_cum_df[["Line", "Type", "upr", "osc"]].copy()
        prev_res = prev_res.rename(columns={"upr": "upr_prev", "osc": "osc_prev"})
        
        merged = pd.merge(df_compare_q, prev_res, on=["Line", "Type"], how="left").fillna(0.0)
        merged["upr"] = merged["upr"] + merged["upr_prev"]
        merged["osc"] = merged["osc"] + merged["osc_prev"]
        merged = merged.drop(columns=["upr_prev", "osc_prev"])
    else:
        merged = df_compare_q.copy()
        
    merged["quy_luy_ke"] = "Lũy kế"
    return merged

def calculate_trailing_12m(df_compare_q: pd.DataFrame, history_q_df: pd.DataFrame, lag4_reserves_df: pd.DataFrame) -> pd.DataFrame:
    """Calculates Trailing 12 Months ('4Q') report values"""
    # 1. Written/Paid (4Q): sum over last 4 quarters (inclusive)
    if not history_q_df.empty:
        df_4q_sum = history_q_df.groupby(["line", "type"]).agg({
            "written": "sum",
            "paid": "sum"
        }).reset_index()
        df_4q_sum = df_4q_sum.rename(columns={"line": "Line", "type": "Type"})
    else:
        df_4q_sum = df_compare_q[["Line", "Type", "written", "paid"]].copy()
        
    # 2. reserves (4Q): change since 4 quarters ago (reserve_curr - reserve_lag4)
    df_compare_q["Line"] = df_compare_q["Line"].str.strip()
    df_compare_q["Type"] = df_compare_q["Type"].str.strip()
    
    if lag4_reserves_df is not None and not lag4_reserves_df.empty:
        lag4_reserves_df["Line"] = lag4_reserves_df["Line"].str.strip()
        lag4_reserves_df["Type"] = lag4_reserves_df["Type"].str.strip()
        
        lag4_res = lag4_reserves_df[["Line", "Type", "reserve_upr", "reserve_osc"]].copy()
        lag4_res = lag4_res.rename(columns={"reserve_upr": "reserve_upr_lag4", "reserve_osc": "reserve_osc_lag4"})
        
        merged = pd.merge(df_compare_q, lag4_res, on=["Line", "Type"], how="left").fillna(0.0)
        merged["upr"] = merged["reserve_upr"] - merged["reserve_upr_lag4"]
        merged["osc"] = merged["reserve_osc"] - merged["reserve_osc_lag4"]
        merged = merged.drop(columns=["reserve_upr_lag4", "reserve_osc_lag4"])
    else:
        merged = df_compare_q.copy()
        merged["upr"] = merged["reserve_upr"]
        merged["osc"] = merged["reserve_osc"]
        
    # 3. Join sum values of written/paid
    merged = merged.drop(columns=["written", "paid"])
    merged = pd.merge(merged, df_4q_sum, on=["Line", "Type"], how="left").fillna(0.0)
    
    merged["quy_luy_ke"] = "4Q"
    return merged

def compute_financial_ratios(df: pd.DataFrame) -> pd.DataFrame:
    df["sub_total_incurred"] = df["paid"] + df["osc"]
    df["sub_total_earned"] = df["written"] - df["upr"]
    
    df["paid_written"] = np.where(df["written"] == 0.0, 0.0, df["paid"] / df["written"])
    df["incurred_earned"] = np.where(df["sub_total_earned"] == 0.0, 0.0, df["sub_total_incurred"] / df["sub_total_earned"])
    
    # Map back to Vietnamese names
    nhom_nv_list = []
    nghiep_vu_list = []
    for line in df["Line"].tolist():
        mapping = LINE_MAP.get(line, {"nghiep_vu": line, "nhom_nv": ""})
        nhom_nv_list.append(mapping["nhom_nv"])
        nghiep_vu_list.append(mapping["nghiep_vu"])
        
    df["nhom_nv"] = nhom_nv_list
    df["nghiep_vu"] = nghiep_vu_list
    
    return df

def get_history_by_quarter(quarter_id: str, quy_luy_ke: str) -> pd.DataFrame:
    """Helper to query database for records of a quarter and category"""
    if not os.path.exists(DB_PATH):
        return pd.DataFrame()
    conn = sqlite3.connect(DB_PATH)
    try:
        query = "SELECT * FROM vas_history WHERE nam = ? AND quy_luy_ke = ?"
        df = pd.read_sql_query(query, conn, params=(quarter_id, quy_luy_ke))
        col_mapping = {
            "quy_luy_ke": "quy_luy_ke",
            "nam_lk": "nam_lk",
            "nhom_nv": "nhom_nv",
            "nghiep_vu": "nghiep_vu",
            "line": "Line",
            "nam": "nam",
            "osc": "osc",
            "paid": "paid",
            "sub_total_incurred": "sub_total_incurred",
            "written": "written",
            "upr": "upr",
            "sub_total_earned": "sub_total_earned",
            "paid_written": "paid_written",
            "incurred_earned": "incurred_earned",
            "reserve_osc": "reserve_osc",
            "reserve_upr": "reserve_upr",
            "quy": "quy",
            "type": "Type",
            "reserve_ibnr": "reserve_ibnr",
            "reserve_cat": "reserve_cat"
        }
        df = df.rename(columns=col_mapping)
        return df
    except Exception:
        return pd.DataFrame()
    finally:
        conn.close()

def save_to_history(df: pd.DataFrame, quarter_id: str):
    """Saves/updates calculation outputs into SQLite db"""
    os.makedirs(os.path.dirname(DB_PATH), exist_ok=True)
    conn = sqlite3.connect(DB_PATH)
    
    try:
        col_mapping = {
            "quy_luy_ke": "quy_luy_ke",
            "nam_lk": "nam_lk",
            "nhom_nv": "nhom_nv",
            "nghiep_vu": "nghiep_vu",
            "Line": "line",
            "nam": "nam",
            "osc": "osc",
            "paid": "paid",
            "sub_total_incurred": "sub_total_incurred",
            "written": "written",
            "upr": "upr",
            "sub_total_earned": "sub_total_earned",
            "paid_written": "paid_written",
            "incurred_earned": "incurred_earned",
            "reserve_osc": "reserve_osc",
            "reserve_upr": "reserve_upr",
            "quy": "quy",
            "Type": "type",
            "reserve_ibnr": "reserve_ibnr",
            "reserve_cat": "reserve_cat"
        }
        
        df_db = df.rename(columns=col_mapping)
        db_cols = ["quy_luy_ke", "nam_lk", "nhom_nv", "nghiep_vu", "line", "nam", "osc", "paid",
                   "sub_total_incurred", "written", "upr", "sub_total_earned", "paid_written",
                   "incurred_earned", "reserve_osc", "reserve_upr", "quy", "type", "reserve_ibnr", "reserve_cat"]
        df_db = df_db[db_cols]
        
        cursor = conn.cursor()
        cursor.execute("DELETE FROM vas_history WHERE nam = ?", (quarter_id,))
        conn.commit()
        
        df_db.to_sql("vas_history", conn, if_exists="append", index=False)
        print(f"Saved {len(df_db)} records for {quarter_id} to database.")
    finally:
        conn.close()

# ── Validation Rules & Anomaly Detection ─────────────────────────────────

def check_structure_rules(df_wide: pd.DataFrame) -> dict:
    """
    Checks Reinsurance Limits logic (Section 2, Tab COMPARE):
    - Recovery <= Direct (warns if Recovery > Direct)
    - Retrocession <= Inward (warns if Retrocession > Inward)
    """
    res = {
        "recovery_issue": [],
        "retro_issue": []
    }
    
    for idx, row in df_wide.iterrows():
        line = row.get("Line", "")
        direct = to_numeric(row.get("DIRECT"))
        inward = to_numeric(row.get("INWARD"))
        recovery = to_numeric(row.get("RECOVERY"))
        retro = to_numeric(row.get("RETROCESSION"))
        
        if not pd.isna(row.get("RECOVERY")) and not pd.isna(row.get("DIRECT")):
            if recovery > direct:
                res["recovery_issue"].append(line)
                
        if not pd.isna(row.get("RETROCESSION")) and not pd.isna(row.get("INWARD")):
            if retro > inward:
                res["retro_issue"].append(line)
                
    return res

def check_direct_recovery_consistency(df_tam: pd.DataFrame) -> dict:
    """
    Checks direct vs recovery consistency (Tab CHI TIẾT):
    - |Direct| > |Recovery|
    - Sign(Direct) == Sign(Recovery)
    - Written and Paid Recovery/Direct ratios
    """
    res = {
        "abs_violation": [],
        "sign_violation": [],
        "written_ratio": 0.0,
        "paid_ratio": 0.0
    }
    
    for idx, row in df_tam.iterrows():
        ct = row.get("Chỉ tiêu", "")
        direct = to_numeric(row.get("DIRECT"))
        recovery = to_numeric(row.get("RECOVERY"))
        
        if abs(direct) <= abs(recovery) and direct != 0.0:
            res["abs_violation"].append(ct)
            
        if np.sign(direct) != np.sign(recovery) and direct != 0.0 and recovery != 0.0:
            res["sign_violation"].append(ct)
            
    written_direct = 0.0
    written_recovery = 0.0
    paid_direct = 0.0
    paid_recovery = 0.0
    
    for idx, row in df_tam.iterrows():
        ct = row.get("Chỉ tiêu", "")
        if ct == "Written":
            written_direct = to_numeric(row.get("DIRECT"))
            written_recovery = to_numeric(row.get("RECOVERY"))
        elif ct == "Paid":
            paid_direct = to_numeric(row.get("DIRECT"))
            paid_recovery = to_numeric(row.get("RECOVERY"))
            
    if written_direct != 0.0:
        res["written_ratio"] = written_recovery / written_direct
    if paid_direct != 0.0:
        res["paid_ratio"] = paid_recovery / paid_direct
        
    return res

def detect_anomalies(df_pivoted: pd.DataFrame, threshold_yoy=0.3, threshold_z=3.0) -> dict:
    """
    Anomaly detection on the last quarter (Tab COMPARE):
    - YoY growth > 30%
    - Z-score > 3
    """
    res = {
        "anomalies": {},
        "last_col": ""
    }
    
    num_cols = [c for c in df_pivoted.columns if c != "Line"]
    if len(num_cols) < 5:
        return res
        
    last_col = num_cols[-1]
    res["last_col"] = last_col
    lag_col = num_cols[-5]
    
    for idx, row in df_pivoted.iterrows():
        line = row["Line"]
        last_val = to_numeric(row[last_col])
        lag_val = to_numeric(row[lag_col])
        
        if lag_val != 0.0:
            yoy = (last_val - lag_val) / lag_val
        else:
            yoy = 0.0
            
        vals = [to_numeric(row[col]) for col in num_cols]
        mean_val = np.mean(vals)
        sd_val = np.std(vals)
        
        if sd_val != 0.0:
            z_score = (last_val - mean_val) / sd_val
        else:
            z_score = 0.0
            
        if abs(yoy) > threshold_yoy or abs(z_score) > threshold_z:
            res["anomalies"][line] = yoy
            
    return res

def find_best_sheets(file_path: str) -> tuple:
    """
    Scans all sheets in the Excel workbook, matches column headers / content
    against keywords, and returns (recommended_result_sheet, recommended_dtbt_sheet).
    """
    import openpyxl
    try:
        wb = openpyxl.load_workbook(file_path, read_only=True)
        sheets = wb.sheetnames
        wb.close()
    except Exception:
        return None, None

    result_keywords = ["line", "upr", "osc", "ibnr", "cat", "direct", "inward", "recovery", "retro", "net"]
    dtbt_keywords = ["code", "phigoc", "nhan", "nhuong cua goc", "nhuong cua nhan", "hoanphi", "giamphi", "bt"]

    best_result_sheet = None
    best_result_score = -1
    best_dtbt_sheet = None
    best_dtbt_score = -1

    for sheet in sheets:
        try:
            # Read first 50 rows of this sheet
            df = pd.read_excel(file_path, sheet_name=sheet, nrows=50, header=None)
        except Exception:
            continue
        
        # Flatten all cell values to lowercased string
        vals_str = " ".join(df.fillna("").astype(str).values.flatten()).lower()
        
        # Calculate scores
        res_score = sum(1 for kw in result_keywords if kw in vals_str)
        dt_score = sum(1 for kw in dtbt_keywords if kw in vals_str)
        
        if res_score > best_result_score:
            best_result_score = res_score
            best_result_sheet = sheet
            
        if dt_score > best_dtbt_score:
            best_dtbt_score = dt_score
            best_dtbt_sheet = sheet

    # Only recommend if score exceeds a minimum threshold
    if best_result_score <= 1:
        best_result_sheet = None
    if best_dtbt_score <= 1:
        best_dtbt_sheet = None

    return best_result_sheet, best_dtbt_sheet

