import os
import re
import datetime
import openpyxl
from openpyxl.utils import get_column_letter
import pandas as pd
from sqlalchemy.orm import Session

import models
import pythoncom
import win32com.client

# Directory definitions
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_ROOT = os.path.join(BASE_DIR, "..", "data")
OUTPUT_EXCEL_ROOT = os.path.join(BASE_DIR, "..", "output_excel")

# Ensure folders exist
os.makedirs(OUTPUT_EXCEL_ROOT, exist_ok=True)

def int2col(col_idx: int) -> str:
    """Convert a 1-based column index to an Excel column letter."""
    result = ""
    while col_idx > 0:
        col_idx, remainder = divmod(col_idx - 1, 26)
        result = chr(65 + remainder) + result
    return result

def load_ty_gia() -> pd.DataFrame:
    """Load the exchange rates dataframe directly from the SQLite database."""
    from database import SessionLocal
    db = SessionLocal()
    try:
        db_rates = db.query(models.FXRate).all()
    finally:
        db.close()

    cols = [
        'Thoi_gian', 'USD', 'EUR', 'AUD', 'CHF', 'CAD', 'CNY', 'DKK', 'GBP', 
        'HKD', 'INR', 'JPY', 'KRW', 'KWD', 'MYR', 'NOK', 'RUB', 'SAR', 'SEK', 
        'SGD', 'THB'
    ]

    if not db_rates:
        return pd.DataFrame(columns=cols)

    # Group by quarter_id
    import collections
    rates_by_quarter = collections.defaultdict(dict)
    for r in db_rates:
        rates_by_quarter[r.quarter_id][r.currency] = r.rate

    # Build rows
    rows = []
    for q_id, cur_map in rates_by_quarter.items():
        try:
            q_part, y_part = q_id.split("_")
            q_num = int(q_part[1]) # "Q1" -> 1
            year = int(y_part)
        except Exception:
            q_num = 0
            year = 0
            
        row = {"Thoi_gian": q_id.replace("_", "/"), "_sort_key": (year, q_num)}
        for col in cols:
            if col != "Thoi_gian":
                row[col] = cur_map.get(col, None)
        rows.append(row)

    # Sort rows chronologically
    rows.sort(key=lambda x: x["_sort_key"])

    # Remove temporary sort key
    for r in rows:
        del r["_sort_key"]

    return pd.DataFrame(rows, columns=cols)

def recalculate_excel_file(file_path: str):
    """Force Excel to recalculate and save the file via COM."""
    pythoncom.CoInitialize()
    try:
        excel = win32com.client.DispatchEx("Excel.Application")
        excel.Visible = False
        excel.DisplayAlerts = False
        
        abs_path = os.path.abspath(file_path)
        wb = excel.Workbooks.Open(abs_path)
        
        # Force full recalculation
        excel.CalculateFull()
        
        wb.Save()
        wb.Close(SaveChanges=True)
        excel.Quit()
    except Exception as e:
        print(f"Error recalculating Excel file {file_path}: {e}")
        raise e
    finally:
        pythoncom.CoUninitialize()

def write_df_to_sheet(ws, df: pd.DataFrame, with_filter: bool = True):
    """Write pandas DataFrame to openpyxl worksheet."""
    # Write headers
    headers = list(df.columns)
    ws.append(headers)
    
    # Write rows
    for row in df.itertuples(index=False):
        row_vals = ["" if pd.isna(val) else val for val in row]
        ws.append(row_vals)
        
    if with_filter and df.shape[0] > 0:
        ws.auto_filter.ref = f"A1:{get_column_letter(df.shape[1])}{df.shape[0] + 1}"

def calculate_upr_for_file(quarter_id: str, file_name: str, group_code: str, dpnv_date: datetime.date) -> str:
    """
    Core UPR calculation implementation matching R's 4.1.ky_mau.R logic.
    Returns the path to the generated output Excel file.
    """
    parquet_path = os.path.join(DATA_ROOT, quarter_id, f"{group_code}.parquet")
    if not os.path.exists(parquet_path):
        dot_qid = quarter_id.replace("_", ".")
        alt_path = os.path.join(DATA_ROOT, dot_qid, f"{group_code}.parquet")
        if os.path.exists(alt_path):
            parquet_path = alt_path

    if not os.path.exists(parquet_path):
        raise FileNotFoundError(f"Merged parquet file not found at {parquet_path}")
        
    df = pd.read_parquet(parquet_path)
    
    # Load exchange rates
    ty_gia = load_ty_gia()
    
    # Determine type of UPR calculation
    is_vietjet = "vietjet" in file_name.lower()
    is_tttbvv = "tttbvv" in file_name.lower()
    is_lt = group_code.upper().endswith("_LT")
    
    # Create workbook
    wb = openpyxl.Workbook()
    if "Sheet" in wb.sheetnames:
        wb.remove(wb["Sheet"])
        
    # Write exchange rates sheet
    ws_tygia = wb.create_sheet("Tygia")
    write_df_to_sheet(ws_tygia, ty_gia, with_filter=False)
    
    # Create Result sheet (R creates it early)
    ws_result = wb.create_sheet("Result")
    
    # Get Ky_phi sheets available
    ky_available = []
    for col in df.columns:
        m = re.match(r"^Ky_phi_(\d+)_", col)
        if m:
            ky_available.append(int(m.group(1)))
    ky_available = sorted(list(set(ky_available)))
    
    dpnv_ngay = dpnv_date.day
    dpnv_thang = dpnv_date.month
    dpnv_nam = dpnv_date.year
    
    # Define VLOOKUP range & fallback cell
    # Note: Tygia starts at A1, USD in B, EUR in C. Header row is 1, data rows are 2 to len(ty_gia)+1
    num_rates = len(ty_gia)
    vlookup_range = f"Tygia!$A$1:$C${num_rates + 1}"
    fallback_cell = f"Tygia!$A${num_rates + 1}"
    
    # Extract four last quarters from exchange rates
    # R: four_last_quarters <- as.vector(ty_gia[(nrow(ty_gia) -3) :nrow(ty_gia),1])
    four_last_quarters = ty_gia.iloc[-4:, 0].tolist()
    
    # Output path
    quarter_out_dir = os.path.join(OUTPUT_EXCEL_ROOT, quarter_id)
    os.makedirs(quarter_out_dir, exist_ok=True)
    out_file_path = os.path.join(quarter_out_dir, f"{file_name}.xlsx")
    
    if is_vietjet:
        # --- Vietjet Branch ---
        columns_to_sum = [
            "Phi_bao_hiem_goc",
            "Phi_bao_hiem_giu_lai",
            "Giam_phi_bao_hiem_goc",
            "Giam_phi_bao_hiem_giu_lai",
            "Giam_phi_bao_hiem_tai"
        ]
        
        # Add formula columns to df
        sheet_data = df.copy()
        cols_to_add = ["Quy_phat_sinh_doanh_thu", "Quy_Nam", "Phi_bao_hiem_giu_lai"]
        for col in cols_to_add:
            if col not in sheet_data.columns:
                sheet_data[col] = None
                
        ws_wj = wb.create_sheet("Vietjet")
        write_df_to_sheet(ws_wj, sheet_data, with_filter=True)
        
        n = len(sheet_data)
        header = list(sheet_data.columns)
        
        def col_pos(col_name):
            return header.index(col_name) + 1
            
        col_thang = int2col(col_pos("Thang_phat_sinh_doanh_thu"))
        col_out_quy = col_pos("Quy_phat_sinh_doanh_thu")
        
        col_quy = int2col(col_pos("Quy_phat_sinh_doanh_thu"))
        col_nam = int2col(col_pos("Nam_phat_sinh_doanh_thu"))
        col_out_quynam = col_pos("Quy_Nam")
        
        goc = int2col(col_pos("Phi_bao_hiem_goc"))
        tai = int2col(col_pos("Phi_bao_hiem_tai"))
        col_out_giulai = col_pos("Phi_bao_hiem_giu_lai")
        
        # Write rows formulas
        for row in range(2, n + 2):
            ws_wj.cell(row=row, column=col_out_quy).value = f'=IF({col_thang}{row}="",0,IF(VALUE({col_thang}{row})=3,2, INT(({col_thang}{row}-1)/3)+1))'
            ws_wj.cell(row=row, column=col_out_quynam).value = f'=CONCATENATE("Q",{col_quy}{row},"/",{col_nam}{row})'
            ws_wj.cell(row=row, column=col_out_giulai).value = f'={goc}{row}-{tai}{row}'
            
        # Result Page Setup
        # result_data has columns: Quy, plus columns_to_sum
        num_result_rows = len(four_last_quarters)
        
        # Write SUBTOTAL row
        ws_result.cell(row=1, column=1).value = "SUBTOTAL"
        for j, col in enumerate(columns_to_sum):
            col_letter = int2col(j + 2) # Column A = Quy, Columns B..F = columns_to_sum
            ws_result.cell(row=1, column=j + 2).value = f"=SUBTOTAL(9,{col_letter}3:{col_letter}{num_result_rows + 2})"
            
        # Write Headers
        ws_result.cell(row=2, column=1).value = "Quy"
        for j, col in enumerate(columns_to_sum):
            ws_result.cell(row=2, column=j + 2).value = col
            
        # Write Rows
        for i, q in enumerate(four_last_quarters):
            row_idx = i + 3
            ws_result.cell(row=row_idx, column=1).value = q
            
            # Formulas for columns_to_sum
            # col 1-2 (Phi_bao_hiem_goc, Phi_bao_hiem_giu_lai)
            for j in range(2):
                col_name = columns_to_sum[j]
                c_range = f"Vietjet!{int2col(col_pos(col_name))}2:{int2col(col_pos(col_name))}{n+1}"
                q_range = f"Vietjet!{int2col(col_pos('Quy_Nam'))}2:{int2col(col_pos('Quy_Nam'))}{n+1}"
                ws_result.cell(row=row_idx, column=j + 2).value = f'=SUMIFS({c_range},{q_range},"{q}")'
                
            # col 3 (Giam_phi_bao_hiem_goc)
            goc_range = f"Vietjet!{int2col(col_pos('Phi_bao_hiem_goc'))}2:{int2col(col_pos('Phi_bao_hiem_goc'))}{n+1}"
            hieu_luc_range = f"Vietjet!{int2col(col_pos('hieu_luc'))}2:{int2col(col_pos('hieu_luc'))}{n+1}"
            q_range = f"Vietjet!{int2col(col_pos('Quy_Nam'))}2:{int2col(col_pos('Quy_Nam'))}{n+1}"
            ws_result.cell(row=row_idx, column=4).value = f'=SUMIFS({goc_range},{hieu_luc_range},"het",{q_range},"{q}")'
            
            # col 4 (Giam_phi_bao_hiem_giu_lai)
            giu_range = f"Vietjet!{int2col(col_pos('Phi_bao_hiem_giu_lai'))}2:{int2col(col_pos('Phi_bao_hiem_giu_lai'))}{n+1}"
            ws_result.cell(row=row_idx, column=5).value = f'=SUMIFS({giu_range},{hieu_luc_range},"het",{q_range},"{q}")'
            
            # col 5 (Giam_phi_bao_hiem_tai)
            ws_result.cell(row=row_idx, column=6).value = f"=D{row_idx}-E{row_idx}"
            
    elif is_tttbvv:
        # --- TTTBVV Branch ---
        columns_to_sum = [
            "Phi_bao_hiem_goc",
            "Phi_bao_hiem_giu_lai",
            "Du_phong_bao_hiem_goc",
            "Du_phong_bao_hiem_giu_lai",
            "Du_phong_bao_hiem_tai"
        ]
        
        if len(ky_available) == 0:
            raise ValueError(f"No installments found for TTTBVV file {file_name}")
            
        sheet_names = [f"Ky_phi{k}" for k in ky_available]
        
        # Process each installment
        for ky in ky_available:
            # Generate sheet data
            fee_cols = [f"Ky_phi_{ky}_So_tien_VND", f"Ky_phi_{ky}_So_tien_USD", f"Ky_phi_{ky}_So_tien_EUR",
                        f"Ky_phi_{ky}_Tu_Ngay", f"Ky_phi_{ky}_Tu_Thang", f"Ky_phi_{ky}_Tu_Nam",
                        f"Ky_phi_{ky}_Den_Ngay", f"Ky_phi_{ky}_Den_Thang", f"Ky_phi_{ky}_Den_Nam",
                        f"Ky_phi_{ky}_Ghi_Ngay", f"Ky_phi_{ky}_Ghi_Thang", f"Ky_phi_{ky}_Ghi_Nam"]
            fee_cols_exist = [c for c in fee_cols if c in df.columns]
            base_cols = list(df.columns[:min(24, len(df.columns))])
            sheet_data = df[base_cols + fee_cols_exist].copy()
            
            cols_to_add = [
                "Thoi_diem_tinh_DPNV_Ngay", "Thoi_diem_tinh_DPNV_Thang","Thoi_diem_tinh_DPNV_Nam",
                "Quy_ghi_doanh_thu", "Nam_ghi_doanh_thu", "Quy_Nam","Phi_bao_hiem_goc","Ty_le_giu_lai_BHBV",
                "Phi_bao_hiem_giu_lai","Tong_so_ngay","So_ngay_da_qua","So_ngay_con_lai","Du_phong_bao_hiem_goc","Du_phong_bao_hiem_giu_lai"
            ]
            for col in cols_to_add:
                if col not in sheet_data.columns:
                    sheet_data[col] = None
                    
            sheet_data["Thoi_diem_tinh_DPNV_Ngay"] = dpnv_ngay
            sheet_data["Thoi_diem_tinh_DPNV_Thang"] = dpnv_thang
            sheet_data["Thoi_diem_tinh_DPNV_Nam"] = dpnv_nam
            
            sheet_name = f"Ky_phi{ky}"
            ws_kp = wb.create_sheet(sheet_name)
            write_df_to_sheet(ws_kp, sheet_data, with_filter=True)
            
            n = len(sheet_data)
            header = list(sheet_data.columns)
            def col_pos(col_name):
                return header.index(col_name) + 1
                
            col_thang = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Thang"))
            col_nam = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Nam"))
            col_out_quy = col_pos("Quy_ghi_doanh_thu")
            col_out_nam = col_pos("Nam_ghi_doanh_thu")
            
            col_quy = int2col(col_pos("Quy_ghi_doanh_thu"))
            col_nam_ghi = int2col(col_pos("Nam_ghi_doanh_thu"))
            col_out_quynam = col_pos("Quy_Nam")
            
            col_vnd = int2col(col_pos(f"Ky_phi_{ky}_So_tien_VND"))
            col_usd = int2col(col_pos(f"Ky_phi_{ky}_So_tien_USD"))
            col_eur = int2col(col_pos(f"Ky_phi_{ky}_So_tien_EUR"))
            col_out_goc = col_pos("Phi_bao_hiem_goc")
            
            # Find Ty_le_giu_lai_cua_BHBV column name
            ret_col = "Ty_le_giu_lai_cua_BHBV" if "Ty_le_giu_lai_cua_BHBV" in sheet_data.columns else \
                      ("Ty_le_giu_lai_cua_BHBV_checked" if "Ty_le_giu_lai_cua_BHBV_checked" in sheet_data.columns else None)
            if not ret_col:
                raise ValueError("Could not find Ty_le_giu_lai_cua_BHBV column in TTTBVV sheet data")
                
            hi = int2col(col_pos(ret_col))
            col_out_tile = col_pos("Ty_le_giu_lai_BHBV")
            
            phi_goc_col = int2col(col_pos("Phi_bao_hiem_goc"))
            tile_col = int2col(col_pos("Ty_le_giu_lai_BHBV"))
            col_out_giulai = col_pos("Phi_bao_hiem_giu_lai")
            
            tu_ngay = int2col(col_pos(f"Ky_phi_{ky}_Tu_Ngay"))
            tu_thang = int2col(col_pos(f"Ky_phi_{ky}_Tu_Thang"))
            tu_nam = int2col(col_pos(f"Ky_phi_{ky}_Tu_Nam"))
            
            den_ngay = int2col(col_pos(f"Ky_phi_{ky}_Den_Ngay"))
            den_thang = int2col(col_pos(f"Ky_phi_{ky}_Den_Thang"))
            den_nam = int2col(col_pos(f"Ky_phi_{ky}_Den_Nam"))
            col_out_tongngay = col_pos("Tong_so_ngay")
            
            dpnv_ngay_col = int2col(col_pos("Thoi_diem_tinh_DPNV_Ngay"))
            dpnv_thang_col = int2col(col_pos("Thoi_diem_tinh_DPNV_Thang"))
            dpnv_nam_col = int2col(col_pos("Thoi_diem_tinh_DPNV_Nam"))
            col_out_ngaydaqua = col_pos("So_ngay_da_qua")
            
            a_col = int2col(col_pos("Tong_so_ngay"))
            b_col = int2col(col_pos("So_ngay_da_qua"))
            col_out_ngayconlai = col_pos("So_ngay_con_lai")
            
            b_col_con = int2col(col_pos("So_ngay_con_lai"))
            c_col_goc = int2col(col_pos("Phi_bao_hiem_goc"))
            col_out_dpgoc = col_pos("Du_phong_bao_hiem_goc")
            
            c_col_giu = int2col(col_pos("Phi_bao_hiem_giu_lai"))
            col_out_dpgiu = col_pos("Du_phong_bao_hiem_giu_lai")
            
            for row in range(2, n + 2):
                ws_kp.cell(row=row, column=col_out_quy).value = f'=IF({col_thang}{row}="",0,INT(({col_thang}{row}-1)/3)+1)'
                ws_kp.cell(row=row, column=col_out_nam).value = f'=VALUE({col_nam}{row})'
                ws_kp.cell(row=row, column=col_out_quynam).value = f'=CONCATENATE("Q",{col_quy}{row},"/",{col_nam_ghi}{row})'
                
                # Phi_bao_hiem_goc
                ws_kp.cell(row=row, column=col_out_goc).value = (
                    f'=VALUE({col_vnd}{row}) + VALUE({col_usd}{row}) * IFERROR(VALUE(VLOOKUP({col_out_quynam}{row}, {vlookup_range}, 2, 0)), '
                    f'VALUE(VLOOKUP({fallback_cell}, {vlookup_range}, 2, 0))) + VALUE({col_eur}{row}) * '
                    f'IFERROR(VALUE(VLOOKUP({col_out_quynam}{row}, {vlookup_range}, 3, 0)), VALUE(VLOOKUP({fallback_cell}, {vlookup_range}, 3, 0)))'
                )
                
                # Ty_le_giu_lai_BHBV
                ws_kp.cell(row=row, column=col_out_tile).value = f'=IF(OR({hi}{row}="",{hi}{row}=0), 1, IF(VALUE({hi}{row}) > 1, VALUE({hi}{row}) / 100, VALUE({hi}{row})))'
                
                # Phi_bao_hiem_giu_lai
                ws_kp.cell(row=row, column=col_out_giulai).value = f'=IF({phi_goc_col}{row}="", 0, VALUE({phi_goc_col}{row}) * {tile_col}{row})'
                
                # Tong_so_ngay
                ws_kp.cell(row=row, column=col_out_tongngay).value = f'=MAX(0,DATE({den_nam}{row},{den_thang}{row},{den_ngay}{row}) - DATE({tu_nam}{row},{tu_thang}{row},{tu_ngay}{row})+1)'
                
                # So_ngay_da_qua
                ws_kp.cell(row=row, column=col_out_ngaydaqua).value = f'=DATE({dpnv_nam_col}{row},{dpnv_thang_col}{row},{dpnv_ngay_col}{row}) - DATE({tu_nam}{row},{tu_thang}{row},{tu_ngay}{row})+1'
                
                # So_ngay_con_lai
                ws_kp.cell(row=row, column=col_out_ngayconlai).value = f'=IF({b_col}{row}<0,{a_col}{row},IF({b_col}{row}>{a_col}{row},0,{a_col}{row}-{b_col}{row}))'
                
                # Du_phong_bao_hiem_goc
                ws_kp.cell(row=row, column=col_out_dpgoc).value = f'={b_col_con}{row}/{a_col}{row}*{c_col_goc}{row}'
                
                # Du_phong_bao_hiem_giu_lai
                ws_kp.cell(row=row, column=col_out_dpgiu).value = f'={b_col_con}{row}/{a_col}{row}*{c_col_giu}{row}'
                
        # Result Page Setup
        # columns: Ky_phi, Quy, columns_to_sum (5 columns)
        num_result_rows = len(sheet_names) * len(four_last_quarters)
        
        # Write SUBTOTAL row
        ws_result.cell(row=1, column=1).value = "SUBTOTAL"
        for j, col in enumerate(columns_to_sum):
            col_letter = int2col(j + 3) # A = Ky_phi, B = Quy, Columns C..G = columns_to_sum
            ws_result.cell(row=1, column=j + 3).value = f"=SUBTOTAL(9,{col_letter}3:{col_letter}{num_result_rows + 2})"
            
        # Write Headers
        ws_result.cell(row=2, column=1).value = "Ky_phi"
        ws_result.cell(row=2, column=2).value = "Quy"
        for j, col in enumerate(columns_to_sum):
            ws_result.cell(row=2, column=j + 3).value = col
            
        # Write Rows
        row_idx = 3
        for sh in sheet_names:
            for q in four_last_quarters:
                ws_result.cell(row=row_idx, column=1).value = sh
                ws_result.cell(row=row_idx, column=2).value = q
                
                # Formulas for columns_to_sum (first 4 columns via SUMIFS)
                # Note: We need to look up column indexes in the Ky_phi sheet.
                # All Ky_phi sheets have the same schema, so we can use the last processed one.
                for j in range(4):
                    col_name = columns_to_sum[j]
                    c_range = f"{sh}!{int2col(col_pos(col_name))}2:{int2col(col_pos(col_name))}{n+1}"
                    q_range = f"{sh}!{int2col(col_pos('Quy_Nam'))}2:{int2col(col_pos('Quy_Nam'))}{n+1}"
                    ws_result.cell(row=row_idx, column=j + 3).value = f'=SUMIFS({c_range},{q_range},"{q}")'
                    
                # 5th column: Du_phong_bao_hiem_tai = Du_phong_goc - Du_phong_giu
                # Columns: A=Ky_phi, B=Quy, C=Phi_goc, D=Phi_giu, E=Du_phong_goc, F=Du_phong_giu, G=Du_phong_tai
                # So it is =E - F
                ws_result.cell(row=row_idx, column=7).value = f"=E{row_idx}-F{row_idx}"
                row_idx += 1
                
    elif is_lt:
        # --- Long Term (LT) Branch ---
        columns_to_sum = [
            "Phi_bao_hiem_sau_dong",
            "Phi_bao_hiem_giu_lai",
            "Phi_tai_bao_hiem",
            "Phi_bao_hiem_giu_lai_duoc_huong",
            "Phi_tai_bao_hiem_duoc_huong",
            "Phi_bao_hiem_giu_lai_chua_huong",
            "Phi_tai_bao_hiem_chua_huong"
        ]
        
        if len(ky_available) == 0:
            raise ValueError(f"No installments found for LT file {file_name}")
            
        sheet_names = [f"Ky_phi{k}" for k in ky_available]
        
        # Process each installment
        for ky in ky_available:
            fee_cols = [f"Ky_phi_{ky}_So_tien_VND", f"Ky_phi_{ky}_So_tien_USD", f"Ky_phi_{ky}_So_tien_EUR",
                        f"Ky_phi_{ky}_Tu_Ngay", f"Ky_phi_{ky}_Tu_Thang", f"Ky_phi_{ky}_Tu_Nam",
                        f"Ky_phi_{ky}_Den_Ngay", f"Ky_phi_{ky}_Den_Thang", f"Ky_phi_{ky}_Den_Nam",
                        f"Ky_phi_{ky}_Ghi_Ngay", f"Ky_phi_{ky}_Ghi_Thang", f"Ky_phi_{ky}_Ghi_Nam"]
            fee_cols_exist = [c for c in fee_cols if c in df.columns]
            base_cols = list(df.columns[:min(24, len(df.columns))])
            sheet_data = df[base_cols + fee_cols_exist].copy()
            
            cols_to_add = [
                "Quy_ghi_nhan_doanh_thu", "Quy_ghi_nhan_doanh_thu_2024", "Quy_ghi_nhan_doanh_thu_2025", 
                "Thoi_diem_ghi_nhan_doanh_thu", "Check_01", "Check_02", "Check_03", "Check_04", "Check_05", 
                "Check_06", "Check_07", "Tổng hợp các tiêu chí", 
                "Thoi_diem_tinh_DPNV_Ngay", "Thoi_diem_tinh_DPNV_Thang", "Thoi_diem_tinh_DPNV_Nam", 
                "Thu_tu_Quy_DPNV", "Mau_so", "Tu_so_huong_cu", "Tu_so_chua_huong", 
                "Tu_so_huong_sau_dieu_chinh", "Tu_so_chua_huong_dieu_chinh", 
                "TS_chua_huong_SĐC_final", "MS_SĐC_final", 
                "Phi_bao_hiem_sau_dong", "Phi_bao_hiem_giu_lai", "Phi_tai_bao_hiem", 
                "Phi_bao_hiem_giu_lai_duoc_huong", "Phi_tai_bao_hiem_duoc_huong", 
                "Phi_bao_hiem_giu_lai_chua_huong", "Phi_tai_bao_hiem_chua_huong", 
                "Check_Phi_bao_hiem_giu_lai_chua_huong", "Check_Phi_tai_bao_hiem_chua_huong"
            ]
            for col in cols_to_add:
                if col not in sheet_data.columns:
                    sheet_data[col] = None
                    
            sheet_data["Thoi_diem_tinh_DPNV_Ngay"] = dpnv_ngay
            sheet_data["Thoi_diem_tinh_DPNV_Thang"] = dpnv_thang
            sheet_data["Thoi_diem_tinh_DPNV_Nam"] = dpnv_nam
            
            sheet_name = f"Ky_phi{ky}"
            ws_kp = wb.create_sheet(sheet_name)
            write_df_to_sheet(ws_kp, sheet_data, with_filter=True)
            
            n = len(sheet_data)
            header = list(sheet_data.columns)
            def col_pos(col_name):
                return header.index(col_name) + 1
                
            col_thang = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Thang"))
            col_nam = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Nam"))
            col_out_quy = col_pos("Quy_ghi_nhan_doanh_thu")
            
            col_quy = int2col(col_pos("Quy_ghi_nhan_doanh_thu"))
            col_nam_ghi = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Nam"))
            col_out_quy_2024 = col_pos("Quy_ghi_nhan_doanh_thu_2024")
            col_out_quy_2025 = col_pos("Quy_ghi_nhan_doanh_thu_2025")
            col_out_quynam = col_pos("Thoi_diem_ghi_nhan_doanh_thu")
            
            col_den_ngay = int2col(col_pos(f"Ky_phi_{ky}_Den_Ngay"))
            col_den_thang = int2col(col_pos(f"Ky_phi_{ky}_Den_Thang"))
            col_den_nam = int2col(col_pos(f"Ky_phi_{ky}_Den_Nam"))
            
            col_dpnv_ngay = int2col(col_pos("Thoi_diem_tinh_DPNV_Ngay"))
            col_dpnv_thang = int2col(col_pos("Thoi_diem_tinh_DPNV_Thang"))
            col_dpnv_nam = int2col(col_pos("Thoi_diem_tinh_DPNV_Nam"))
            col_out_c1 = col_pos("Check_01")
            
            tu_ngay = int2col(col_pos("Thoi_han_bao_hiem_Tu_Ngay"))
            tu_thang = int2col(col_pos("Thoi_han_bao_hiem_Tu_Thang"))
            tu_nam = int2col(col_pos("Thoi_han_bao_hiem_Tu_Nam"))
            den_ngay = int2col(col_pos("Thoi_han_bao_hiem_Den_Ngay"))
            den_thang = int2col(col_pos("Thoi_han_bao_hiem_Den_Thang"))
            den_nam = int2col(col_pos("Thoi_han_bao_hiem_Den_Nam"))
            col_out_c2 = col_pos("Check_02")
            
            col_vnd = int2col(col_pos(f"Ky_phi_{ky}_So_tien_VND"))
            col_usd = int2col(col_pos(f"Ky_phi_{ky}_So_tien_USD"))
            col_eur = int2col(col_pos(f"Ky_phi_{ky}_So_tien_EUR"))
            col_out_c3 = col_pos("Check_03")
            
            tu_nam_kp = int2col(col_pos(f"Ky_phi_{ky}_Tu_Nam"))
            tu_thang_kp = int2col(col_pos(f"Ky_phi_{ky}_Tu_Thang"))
            tu_ngay_kp = int2col(col_pos(f"Ky_phi_{ky}_Tu_Ngay"))
            den_nam_kp = int2col(col_pos(f"Ky_phi_{ky}_Den_Nam"))
            den_thang_kp = int2col(col_pos(f"Ky_phi_{ky}_Den_Thang"))
            den_ngay_kp = int2col(col_pos(f"Ky_phi_{ky}_Den_Ngay"))
            col_out_c4 = col_pos("Check_04")
            
            col_ghi_thang = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Thang"))
            col_ghi_nam = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Nam"))
            col_out_c5 = col_pos("Check_05")
            
            col_tu_nam = int2col(col_pos("Thoi_han_bao_hiem_Tu_Nam"))
            col_tu_thang = int2col(col_pos("Thoi_han_bao_hiem_Tu_Thang"))
            col_tu_ngay = int2col(col_pos("Thoi_han_bao_hiem_Tu_Ngay"))
            col_out_c6 = col_pos("Check_06")
            
            col_ghi_ngay = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Ngay"))
            col_out_c7 = col_pos("Check_07")
            
            c1_c = int2col(col_pos("Check_01"))
            c2_c = int2col(col_pos("Check_02"))
            c3_c = int2col(col_pos("Check_03"))
            c4_c = int2col(col_pos("Check_04"))
            c5_c = int2col(col_pos("Check_05"))
            c6_c = int2col(col_pos("Check_06"))
            c7_c = int2col(col_pos("Check_07"))
            col_out_tonghop = col_pos("Tổng hợp các tiêu chí")
            
            col_tonghop = int2col(col_pos("Tổng hợp các tiêu chí"))
            col_out_thu_tu = col_pos("Thu_tu_Quy_DPNV")
            
            col_out_mau_so = col_pos("Mau_so")
            
            col_thu_tu = int2col(col_pos("Thu_tu_Quy_DPNV"))
            col_mau_so = int2col(col_pos("Mau_so"))
            col_out_huong_cu = col_pos("Tu_so_huong_cu")
            
            col_huong_cu = int2col(col_pos("Tu_so_huong_cu"))
            col_out_chua_huong = col_pos("Tu_so_chua_huong")
            
            col_chua_huong_dieu_chinh = int2col(col_pos("Tu_so_chua_huong_dieu_chinh"))
            col_out_huong_sdc = col_pos("Tu_so_huong_sau_dieu_chinh")
            
            col_chua_huong = int2col(col_pos("Tu_so_chua_huong"))
            col_out_chua_huong_dieu_chinh = col_pos("Tu_so_chua_huong_dieu_chinh")
            
            col_out_ts_final = col_pos("TS_chua_huong_SĐC_final")
            col_out_ms_final = col_pos("MS_SĐC_final")
            
            col_out_phi_sau_dong = col_pos("Phi_bao_hiem_sau_dong")
            
            ret_col = "Ty_le_giu_lai_cua_BHBV" if "Ty_le_giu_lai_cua_BHBV" in sheet_data.columns else \
                      ("Ty_le_giu_lai_cua_BHBV_checked" if "Ty_le_giu_lai_cua_BHBV_checked" in sheet_data.columns else None)
            if not ret_col:
                raise ValueError("Could not find Ty_le_giu_lai_cua_BHBV column in LT sheet data")
            hi = int2col(col_pos(ret_col))
            phi_col = int2col(col_pos("Phi_bao_hiem_sau_dong"))
            col_out_phi_giu_lai = col_pos("Phi_bao_hiem_giu_lai")
            
            phi_giu_lai = int2col(col_pos("Phi_bao_hiem_giu_lai"))
            col_out_phi_tai = col_pos("Phi_tai_bao_hiem")
            
            ms_final = int2col(col_pos("MS_SĐC_final"))
            ts_final = int2col(col_pos("TS_chua_huong_SĐC_final"))
            col_out_giu_chua_huong = col_pos("Phi_bao_hiem_giu_lai_chua_huong")
            
            phi_giu_chua_huong = int2col(col_pos("Phi_bao_hiem_giu_lai_chua_huong"))
            col_out_giu_duoc_huong = col_pos("Phi_bao_hiem_giu_lai_duoc_huong")
            
            phi_tai = int2col(col_pos("Phi_tai_bao_hiem"))
            phi_tai_chua_huong = int2col(col_pos("Phi_tai_bao_hiem_chua_huong"))
            col_out_tai_duoc_huong = col_pos("Phi_tai_bao_hiem_duoc_huong")
            col_out_tai_chua_huong = col_pos("Phi_tai_bao_hiem_chua_huong")
            
            ts_huong_sdc = int2col(col_pos("Tu_so_huong_sau_dieu_chinh"))
            phi_giu_duoc_huong = int2col(col_pos("Phi_bao_hiem_giu_lai_duoc_huong"))
            col_out_check_giu = col_pos("Check_Phi_bao_hiem_giu_lai_chua_huong")
            
            phi_tai_duoc_huong = int2col(col_pos("Phi_tai_bao_hiem_duoc_huong"))
            col_out_check_tai = col_pos("Check_Phi_tai_bao_hiem_chua_huong")
            
            for row in range(2, n + 2):
                ws_kp.cell(row=row, column=col_out_quy).value = f'=IF({col_thang}{row}="",0,INT(({col_thang}{row}-1)/3)+1)'
                ws_kp.cell(row=row, column=col_out_quy_2024).value = f'=IF(VALUE({col_nam}{row})=2024,{col_quy}{row},0)'
                ws_kp.cell(row=row, column=col_out_quy_2025).value = f'=IF(VALUE({col_nam}{row})=2025,{col_quy}{row},0)'
                ws_kp.cell(row=row, column=col_out_quynam).value = f'=CONCATENATE("Q",{col_quy}{row},"/",{col_nam_ghi}{row})'
                
                # Check_01
                ws_kp.cell(row=row, column=col_out_c1).value = f'=IFERROR(IF(DATE({col_dpnv_nam}{row},{col_dpnv_thang}{row},{col_dpnv_ngay}{row}) - DATE({col_den_nam}{row},{col_den_thang}{row},{col_den_ngay}{row}) >= 0, 0, 1), 0)'
                
                # Check_02
                ws_kp.cell(row=row, column=col_out_c2).value = f'=IF(OR({tu_ngay}{row}="",{tu_thang}{row}="",{tu_nam}{row}="",{den_ngay}{row}="",{den_thang}{row}="",{den_nam}{row}=""),0,IF(DATE({den_nam}{row},{den_thang}{row},{den_ngay}{row}) - DATE({tu_nam}{row},{tu_thang}{row},{tu_ngay}{row}) <= 365,0,1))'
                
                # Check_03
                ws_kp.cell(row=row, column=col_out_c3).value = f'=IF(AND({col_vnd}{row}="",{col_usd}{row}="",{col_eur}{row}=""),0,1)'
                
                # Check_04
                ws_kp.cell(row=row, column=col_out_c4).value = f'=IFERROR(IF(DATE({den_nam_kp}{row},{den_thang_kp}{row},{den_ngay_kp}{row})-DATE({tu_nam_kp}{row},{tu_thang_kp}{row},{tu_ngay_kp}{row})>0,1,0),0)'
                
                # Check_05
                ws_kp.cell(row=row, column=col_out_c5).value = f'=IF(AND({col_ghi_thang}{row}="",{col_ghi_nam}{row}=""), 0,1)'
                
                # Check_06
                ws_kp.cell(row=row, column=col_out_c6).value = f'=IFERROR(IF(DATE({col_dpnv_nam}{row},{col_dpnv_thang}{row},{col_dpnv_ngay}{row}) - DATE({col_tu_nam}{row},{col_tu_thang}{row},{col_tu_ngay}{row})>=0,1,0),0)'
                
                # Check_07
                ws_kp.cell(row=row, column=col_out_c7).value = f'=IFERROR(IF(DATE({col_ghi_nam}{row},{col_ghi_thang}{row},{col_ghi_ngay}{row}) - DATE({col_dpnv_nam}{row},{col_dpnv_thang}{row},{col_dpnv_ngay}{row}) > 0, 0, 1), 0)'
                
                # Tổng hợp các tiêu chí
                ws_kp.cell(row=row, column=col_out_tonghop).value = f'=IF(OR({c1_c}{row}=0,{c2_c}{row}=0,{c3_c}{row}=0,{c4_c}{row}=0,{c5_c}{row}=0,{c6_c}{row}=0,{c7_c}{row}=0), 0, 1)'
                
                # Thu_tu_Quy_DPNV
                ws_kp.cell(row=row, column=col_out_thu_tu).value = f'=IF({col_tonghop}{row}=0, 0, IFERROR(({col_dpnv_nam}{row} - {tu_nam_kp}{row}) * 4 + INT(({col_dpnv_thang}{row} - 1) / 3) + 1 - (INT(({tu_thang_kp}{row} - 1) / 3) + 1 )+1, 0))'
                
                # Mau_so
                ws_kp.cell(row=row, column=col_out_mau_so).value = f'=IFERROR(((DATE({den_nam_kp}{row},{den_thang_kp}{row},{den_ngay_kp}{row}) - DATE({tu_nam_kp}{row},{tu_thang_kp}{row},{tu_ngay_kp}{row}) + 1) / 365) * 8, 0)'
                
                # Tu_so_huong_cu
                ws_kp.cell(row=row, column=col_out_huong_cu).value = f'=IF({col_tonghop}{row}=0, {col_mau_so}{row}, IF({col_thu_tu}{row}<=0, 0, {col_thu_tu}{row}*2-1))'
                
                # Tu_so_chua_huong
                ws_kp.cell(row=row, column=col_out_chua_huong).value = f'={col_mau_so}{row} - {col_huong_cu}{row}'
                
                # Tu_so_chua_huong_dieu_chinh
                ws_kp.cell(row=row, column=col_out_chua_huong_dieu_chinh).value = f'=IFERROR(IF({col_chua_huong}{row}>=0, {col_chua_huong}{row}, ((DATE({den_nam_kp}{row},{den_thang_kp}{row},{den_ngay_kp}{row}) - DATE({col_dpnv_nam}{row},{col_dpnv_thang}{row},{col_dpnv_ngay}{row})) / 365) * 8),0)'
                
                # Tu_so_huong_sau_dieu_chinh
                ws_kp.cell(row=row, column=col_out_huong_sdc).value = f'={col_mau_so}{row} - {col_chua_huong_dieu_chinh}{row}'
                
                # TS_chua_huong_SĐC_final
                ws_kp.cell(row=row, column=col_out_ts_final).value = f'={col_chua_huong_dieu_chinh}{row}'
                
                # MS_SĐC_final
                ws_kp.cell(row=row, column=col_out_ms_final).value = f'={col_mau_so}{row}'
                
                # Phi_bao_hiem_sau_dong
                ws_kp.cell(row=row, column=col_out_phi_sau_dong).value = (
                    f'=VALUE({col_vnd}{row}) + VALUE({col_usd}{row}) * IFERROR(VALUE(VLOOKUP({col_out_quynam}{row}, {vlookup_range}, 2, 0)), '
                    f'VALUE(VLOOKUP({fallback_cell}, {vlookup_range}, 2, 0))) + VALUE({col_eur}{row}) * '
                    f'IFERROR(VALUE(VLOOKUP({col_out_quynam}{row}, {vlookup_range}, 3, 0)), VALUE(VLOOKUP({fallback_cell}, {vlookup_range}, 3, 0)))'
                )
                
                # Phi_bao_hiem_giu_lai
                ws_kp.cell(row=row, column=col_out_phi_giu_lai).value = f'=IF({phi_col}{row}="", 0, VALUE({phi_col}{row}) * IF(OR({hi}{row}="",{hi}{row}=0), 1, IF(VALUE({hi}{row}) > 1, VALUE({hi}{row}) / 100, VALUE({hi}{row}))))'
                
                # Phi_tai_bao_hiem
                ws_kp.cell(row=row, column=col_out_phi_tai).value = f'=IF({phi_col}{row}="", 0, VALUE({phi_col}{row}) - VALUE({phi_giu_lai}{row}))'
                
                # Phi_bao_hiem_giu_lai_chua_huong
                ws_kp.cell(row=row, column=col_out_giu_chua_huong).value = f'=IF(VALUE({ms_final}{row})=0, 0, VALUE({phi_giu_lai}{row}) * VALUE({ts_final}{row}) / VALUE({ms_final}{row}))'
                
                # Phi_bao_hiem_giu_lai_duoc_huong
                ws_kp.cell(row=row, column=col_out_giu_duoc_huong).value = f'=VALUE({phi_giu_lai}{row}) - VALUE({phi_giu_chua_huong}{row})'
                
                # Phi_tai_bao_hiem_chua_huong
                ws_kp.cell(row=row, column=col_out_tai_chua_huong).value = f'=IF(VALUE({ms_final}{row})=0, 0, VALUE({phi_tai}{row}) * VALUE({ts_final}{row}) / VALUE({ms_final}{row}))'
                
                # Phi_tai_bao_hiem_duoc_huong
                ws_kp.cell(row=row, column=col_out_tai_duoc_huong).value = f'=VALUE({phi_tai}{row}) - VALUE({phi_tai_chua_huong}{row})'
                
                # Check_Phi_bao_hiem_giu_lai_chua_huong
                ws_kp.cell(row=row, column=col_out_check_giu).value = f'=IF(VALUE({ms_final}{row}) = 0, 0, VALUE({phi_giu_lai}{row}) * (VALUE({ts_huong_sdc}{row}) / VALUE({ms_final}{row})) - VALUE({phi_giu_duoc_huong}{row}))'
                
                # Check_Phi_tai_bao_hiem_chua_huong
                ws_kp.cell(row=row, column=col_out_check_tai).value = f'=IF(VALUE({ms_final}{row}) = 0, 0, VALUE({phi_tai}{row}) * (VALUE({ts_huong_sdc}{row}) / VALUE({ms_final}{row})) - VALUE({phi_tai_duoc_huong}{row}))'
                
        # Result Page Setup
        # columns: Ky_phi, Quy, columns_to_sum (7 columns)
        # Note that R prefixes with "Số dùng để tính", and then the four quarters
        quarters_list = ["Số dùng để tính"] + four_last_quarters
        num_result_rows = len(sheet_names) * len(quarters_list)
        
        # Write SUBTOTAL row
        ws_result.cell(row=1, column=1).value = "SUBTOTAL"
        for j, col in enumerate(columns_to_sum):
            col_letter = int2col(j + 3) # Columns C..I = columns_to_sum
            ws_result.cell(row=1, column=j + 3).value = f"=SUBTOTAL(9,{col_letter}3:{col_letter}{num_result_rows + 2})"
            
        # Write Headers
        ws_result.cell(row=2, column=1).value = "Ky_phi"
        ws_result.cell(row=2, column=2).value = "Quy"
        for j, col in enumerate(columns_to_sum):
            ws_result.cell(row=2, column=j + 3).value = col
            
        # Write Rows
        row_idx = 3
        for sh in sheet_names:
            for q in quarters_list:
                ws_result.cell(row=row_idx, column=1).value = sh
                ws_result.cell(row=row_idx, column=2).value = q
                
                # SUMIFS formulas
                for j, col in enumerate(columns_to_sum):
                    c_range = f"{sh}!{int2col(col_pos(col))}2:{int2col(col_pos(col))}{n+1}"
                    criteria_range = f"{sh}!{int2col(col_pos('Tổng hợp các tiêu chí'))}2:{int2col(col_pos('Tổng hợp các tiêu chí'))}{n+1}"
                    q_range = f"{sh}!{int2col(col_pos('Thoi_diem_ghi_nhan_doanh_thu'))}2:{int2col(col_pos('Thoi_diem_ghi_nhan_doanh_thu'))}{n+1}"
                    
                    if q != "Số dùng để tính":
                        ws_result.cell(row=row_idx, column=j + 3).value = f'=SUMIFS({c_range},{criteria_range},"1",{q_range},"{q}")'
                    else:
                        ws_result.cell(row=row_idx, column=j + 3).value = f'=SUMIFS({c_range},{criteria_range},"1")'
                row_idx += 1
                
    else:
        # --- Short Term (ST) Branch ---
        columns_to_sum = [
            "Phi_bao_hiem_goc",
            "Phi_bao_hiem_giu_lai",
            "Giam_phi_bao_hiem_goc",
            "Giam_phi_bao_hiem_giu_lai",
            "Giam_phi_bao_hiem_tai"
        ]
        
        if len(ky_available) == 0:
            raise ValueError(f"No installments found for ST file {file_name}")
            
        sheet_names = [f"Ky_phi{k}" for k in ky_available]
        
        # Process each installment
        for ky in ky_available:
            fee_cols = [f"Ky_phi_{ky}_So_tien_VND", f"Ky_phi_{ky}_So_tien_USD", f"Ky_phi_{ky}_So_tien_EUR",
                        f"Ky_phi_{ky}_Tu_Ngay", f"Ky_phi_{ky}_Tu_Thang", f"Ky_phi_{ky}_Tu_Nam",
                        f"Ky_phi_{ky}_Den_Ngay", f"Ky_phi_{ky}_Den_Thang", f"Ky_phi_{ky}_Den_Nam",
                        f"Ky_phi_{ky}_Ghi_Ngay", f"Ky_phi_{ky}_Ghi_Thang", f"Ky_phi_{ky}_Ghi_Nam"]
            fee_cols_exist = [c for c in fee_cols if c in df.columns]
            base_cols = list(df.columns[:min(24, len(df.columns))])
            sheet_data = df[base_cols + fee_cols_exist].copy()
            
            cols_to_add = [
                "Thoi_diem_tinh_DPNV_Ngay", "Thoi_diem_tinh_DPNV_Thang","Thoi_diem_tinh_DPNV_Nam",
                "Quy_ghi_doanh_thu", "Nam_ghi_doanh_thu", "Quy_Nam","Phi_bao_hiem_goc","Ty_le_giu_lai_BHBV",
                "Phi_bao_hiem_giu_lai","Dem_ngay","Het_hieu_luc"
            ]
            for col in cols_to_add:
                if col not in sheet_data.columns:
                    sheet_data[col] = None
                    
            sheet_data["Thoi_diem_tinh_DPNV_Ngay"] = dpnv_ngay
            sheet_data["Thoi_diem_tinh_DPNV_Thang"] = dpnv_thang
            sheet_data["Thoi_diem_tinh_DPNV_Nam"] = dpnv_nam
            
            sheet_name = f"Ky_phi{ky}"
            ws_kp = wb.create_sheet(sheet_name)
            write_df_to_sheet(ws_kp, sheet_data, with_filter=True)
            
            n = len(sheet_data)
            header = list(sheet_data.columns)
            def col_pos(col_name):
                return header.index(col_name) + 1
                
            col_thang = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Thang"))
            col_nam = int2col(col_pos(f"Ky_phi_{ky}_Ghi_Nam"))
            col_out_quy = col_pos("Quy_ghi_doanh_thu")
            col_out_nam = col_pos("Nam_ghi_doanh_thu")
            
            col_quy = int2col(col_pos("Quy_ghi_doanh_thu"))
            col_nam_ghi = int2col(col_pos("Nam_ghi_doanh_thu"))
            col_out_quynam = col_pos("Quy_Nam")
            
            col_vnd = int2col(col_pos(f"Ky_phi_{ky}_So_tien_VND"))
            col_usd = int2col(col_pos(f"Ky_phi_{ky}_So_tien_USD"))
            col_eur = int2col(col_pos(f"Ky_phi_{ky}_So_tien_EUR"))
            col_out_goc = col_pos("Phi_bao_hiem_goc")
            
            ret_col = "Ty_le_giu_lai_cua_BHBV" if "Ty_le_giu_lai_cua_BHBV" in sheet_data.columns else \
                      ("Ty_le_giu_lai_cua_BHBV_checked" if "Ty_le_giu_lai_cua_BHBV_checked" in sheet_data.columns else None)
            if not ret_col:
                raise ValueError("Could not find Ty_le_giu_lai_cua_BHBV column in ST sheet data")
            hi = int2col(col_pos(ret_col))
            col_out_tile = col_pos("Ty_le_giu_lai_BHBV")
            
            phi_goc_col = int2col(col_pos("Phi_bao_hiem_goc"))
            tile_col = int2col(col_pos("Ty_le_giu_lai_BHBV"))
            col_out_giulai = col_pos("Phi_bao_hiem_giu_lai")
            
            tu_ngay = int2col(col_pos("Thoi_han_bao_hiem_Tu_Ngay"))
            tu_thang = int2col(col_pos("Thoi_han_bao_hiem_Tu_Thang"))
            tu_nam = int2col(col_pos("Thoi_han_bao_hiem_Tu_Nam"))
            den_ngay = int2col(col_pos("Thoi_han_bao_hiem_Den_Ngay"))
            den_thang = int2col(col_pos("Thoi_han_bao_hiem_Den_Thang"))
            den_nam = int2col(col_pos("Thoi_han_bao_hiem_Den_Nam"))
            col_out_demngay = col_pos("Dem_ngay")
            
            dpnv_ngay_col = int2col(col_pos("Thoi_diem_tinh_DPNV_Ngay"))
            dpnv_thang_col = int2col(col_pos("Thoi_diem_tinh_DPNV_Thang"))
            dpnv_nam_col = int2col(col_pos("Thoi_diem_tinh_DPNV_Nam"))
            col_out_hethieuluc = col_pos("Het_hieu_luc")
            
            for row in range(2, n + 2):
                ws_kp.cell(row=row, column=col_out_quy).value = f'=IF({col_thang}{row}="",0,INT(({col_thang}{row}-1)/3)+1)'
                ws_kp.cell(row=row, column=col_out_nam).value = f'=VALUE({col_nam}{row})'
                ws_kp.cell(row=row, column=col_out_quynam).value = f'=CONCATENATE("Q",{col_quy}{row},"/",{col_nam_ghi}{row})'
                
                # Phi_bao_hiem_goc
                ws_kp.cell(row=row, column=col_out_goc).value = (
                    f'=VALUE({col_vnd}{row}) + VALUE({col_usd}{row}) * IFERROR(VALUE(VLOOKUP({col_out_quynam}{row}, {vlookup_range}, 2, 0)), '
                    f'VALUE(VLOOKUP({fallback_cell}, {vlookup_range}, 2, 0))) + VALUE({col_eur}{row}) * '
                    f'IFERROR(VALUE(VLOOKUP({col_out_quynam}{row}, {vlookup_range}, 3, 0)), VALUE(VLOOKUP({fallback_cell}, {vlookup_range}, 3, 0)))'
                )
                
                # Ty_le_giu_lai_BHBV
                ws_kp.cell(row=row, column=col_out_tile).value = f'=IF(OR({hi}{row}="",{hi}{row}=0), 1, IF(VALUE({hi}{row}) > 1, VALUE({hi}{row}) / 100, VALUE({hi}{row})))'
                
                # Phi_bao_hiem_giu_lai
                ws_kp.cell(row=row, column=col_out_giulai).value = f'=IF({phi_goc_col}{row}="", 0, VALUE({phi_goc_col}{row}) * {tile_col}{row})'
                
                # Dem_ngay
                ws_kp.cell(row=row, column=col_out_demngay).value = f'=IF(OR({tu_ngay}{row}="",{tu_thang}{row}="",{tu_nam}{row}="",{den_ngay}{row}="",{den_thang}{row}="",{den_nam}{row}=""),0,IF(DATE({den_nam}{row},{den_thang}{row},{den_ngay}{row}) - DATE({tu_nam}{row},{tu_thang}{row},{tu_ngay}{row}) < 365,1,0))'
                
                # Het_hieu_luc
                ws_kp.cell(row=row, column=col_out_hethieuluc).value = f'=IF(OR({dpnv_ngay_col}{row}="",{dpnv_thang_col}{row}="",{dpnv_nam_col}{row}="",{den_ngay}{row}="",{den_thang}{row}="",{den_nam}{row}=""),0,IF(DATE({dpnv_nam_col}{row},{dpnv_thang_col}{row},{dpnv_nam_col}{row}) - DATE({den_nam}{row},{den_thang}{row},{den_ngay}{row}) >=0,1))'
                
        # Result Page Setup
        # columns: Ky_phi, Quy, columns_to_sum (5 columns)
        num_result_rows = len(sheet_names) * len(four_last_quarters)
        
        # Write SUBTOTAL row
        ws_result.cell(row=1, column=1).value = "SUBTOTAL"
        for j, col in enumerate(columns_to_sum):
            col_letter = int2col(j + 3) # Columns C..G = columns_to_sum
            ws_result.cell(row=1, column=j + 3).value = f"=SUBTOTAL(9,{col_letter}3:{col_letter}{num_result_rows + 2})"
            
        # Write Headers
        ws_result.cell(row=2, column=1).value = "Ky_phi"
        ws_result.cell(row=2, column=2).value = "Quy"
        for j, col in enumerate(columns_to_sum):
            ws_result.cell(row=2, column=j + 3).value = col
            
        # Write Rows
        row_idx = 3
        for sh in sheet_names:
            for q in four_last_quarters:
                ws_result.cell(row=row_idx, column=1).value = sh
                ws_result.cell(row=row_idx, column=2).value = q
                
                # Columns 1-2 (Phi_bao_hiem_goc, Phi_bao_hiem_giu_lai) via SUMIFS
                for j in range(2):
                    col_name = columns_to_sum[j]
                    c_range = f"{sh}!{int2col(col_pos(col_name))}2:{int2col(col_pos(col_name))}{n+1}"
                    q_range = f"{sh}!{int2col(col_pos('Quy_Nam'))}2:{int2col(col_pos('Quy_Nam'))}{n+1}"
                    ws_result.cell(row=row_idx, column=j + 3).value = f'=SUMIFS({c_range},{q_range},"{q}")'
                    
                # Column 3 (Giam_phi_bao_hiem_goc)
                goc_range = f"{sh}!{int2col(col_pos('Phi_bao_hiem_goc'))}2:{int2col(col_pos('Phi_bao_hiem_goc'))}{n+1}"
                dem_ngay_range = f"{sh}!{int2col(col_pos('Dem_ngay'))}2:{int2col(col_pos('Dem_ngay'))}{n+1}"
                hieu_luc_range = f"{sh}!{int2col(col_pos('Het_hieu_luc'))}2:{int2col(col_pos('Het_hieu_luc'))}{n+1}"
                q_range = f"{sh}!{int2col(col_pos('Quy_Nam'))}2:{int2col(col_pos('Quy_Nam'))}{n+1}"
                ws_result.cell(row=row_idx, column=5).value = f'=SUMIFS({goc_range},{dem_ngay_range},"1",{hieu_luc_range},"1",{q_range},"{q}")'
                
                # Column 4 (Giam_phi_bao_hiem_giu_lai)
                giu_range = f"{sh}!{int2col(col_pos('Phi_bao_hiem_giu_lai'))}2:{int2col(col_pos('Phi_bao_hiem_giu_lai'))}{n+1}"
                ws_result.cell(row=row_idx, column=6).value = f'=SUMIFS({giu_range},{dem_ngay_range},"1",{hieu_luc_range},"1",{q_range},"{q}")'
                
                # Column 5 (Giam_phi_bao_hiem_tai) = Giam_goc - Giam_giu
                # Columns: A=Ky_phi, B=Quy, C=Phi_goc, D=Phi_giu, E=Giam_goc, F=Giam_giu, G=Giam_tai
                # So it is =E - F
                ws_result.cell(row=row_idx, column=7).value = f"=E{row_idx}-F{row_idx}"
                row_idx += 1
                
    wb.save(out_file_path)
    
    # Recalculate using Excel COM
    recalculate_excel_file(out_file_path)
    
    return out_file_path

def calculate_upr_for_quarter(db: Session, quarter_id: str, file_ids: list[int] = None) -> dict:
    """
    Calculate UPR for a specific quarter.
    Processes selected file IDs or all files in status 'Merged'.
    """
    # Fetch calculations date
    param = db.query(models.AppParameter).filter(models.AppParameter.quarter_id == quarter_id).first()
    if param:
        dpnv_date = datetime.date(param.year, param.month, param.day)
    else:
        # Fallback date: end of quarter
        m = re.match(r"Q([1-4])_(\d{4})", quarter_id)
        if m:
            q, y = int(m.group(1)), int(m.group(2))
            if q == 1:
                dpnv_date = datetime.date(y, 3, 31)
            elif q == 2:
                dpnv_date = datetime.date(y, 6, 30)
            elif q == 3:
                dpnv_date = datetime.date(y, 9, 30)
            else:
                dpnv_date = datetime.date(y, 12, 31)
        else:
            dpnv_date = datetime.date.today()
            
    query = db.query(models.FileQueue).filter(models.FileQueue.quarter_id == quarter_id)
    if file_ids is not None:
        query = query.filter(models.FileQueue.id.in_(file_ids))
    else:
        query = query.filter(models.FileQueue.status == "Merged")
        
    files_to_calc = query.all()
    
    results = []
    for f in files_to_calc:
        f.status = "Calculating"
        db.commit()
        try:
            out_path = calculate_upr_for_file(
                quarter_id=f.quarter_id,
                file_name=f.file_name,
                group_code=f.group_code,
                dpnv_date=dpnv_date
            )
            f.status = "Calculated"
            db.commit()
            results.append({"file_id": f.id, "file_name": f.file_name, "status": "Success", "out_path": out_path})
        except Exception as e:
            f.status = "Error"
            db.commit()
            results.append({"file_id": f.id, "file_name": f.file_name, "status": "Error", "error": str(e)})
            
    return {"calculated": results}

def summarize_reports(db: Session, quarter_id: str, file_ids: list[int] = None) -> dict:
    """
    Consolidate calculated UPR Excel files into a summary workbook.
    """
    # Fetch calculated files in the quarter
    query = db.query(models.FileQueue).filter(
        models.FileQueue.quarter_id == quarter_id,
        models.FileQueue.status == "Calculated"
    )
    if file_ids is not None:
        query = query.filter(models.FileQueue.id.in_(file_ids))
        
    calculated_files = query.all()
    if not calculated_files:
        raise ValueError(f"No calculated files found in quarter {quarter_id} to summarize.")
        
    group_data = {
        "LT": [],
        "Travel": [],
        "TTTBVV": {},
        "ShortTerm": []
    }
    
    # Process each calculated file
    for f in calculated_files:
        excel_path = os.path.join(OUTPUT_EXCEL_ROOT, quarter_id, f"{f.file_name}.xlsx")
        if not os.path.exists(excel_path):
            continue
            
        # Re-activate via Excel COM first to ensure values are calculated
        recalculate_excel_file(excel_path)
        
        # Load Result sheet using pandas (we need the actual calculated values)
        # Note: pandas loads the values cached in the Excel sheet
        # Since we just called CalculateFull() and saved via win32com, values are guaranteed to exist.
        try:
            # We skip the first row (the SUBTOTAL row), and use the second row as header
            df_raw = pd.read_excel(excel_path, sheet_name="Result", header=None)
            if len(df_raw) < 2:
                continue
                
            headers = df_raw.iloc[1].tolist()
            data = df_raw.iloc[2:].copy()
            data.columns = headers
            
            # Remove Ky_phi if present
            if "Ky_phi" in data.columns:
                data = data.drop(columns=["Ky_phi"])
                
            # Convert numeric columns to float
            for col in data.columns:
                if col != "Quy":
                    data[col] = pd.to_numeric(data[col], errors="coerce").fillna(0.0)
                    
            # Group by Quy and sum
            df_sum = data.groupby("Quy", as_index=False).sum()
            
            # Determine group
            fn_lower = f.file_name.lower()
            if "lt" in fn_lower:
                df_sum.insert(0, "SourceFile", f.file_name)
                group_data["LT"].append(df_sum)
            elif "travel" in fn_lower or "vietjet" in fn_lower:
                df_sum.insert(0, "SourceFile", f.file_name)
                group_data["Travel"].append(df_sum)
            elif "tttbvv" in fn_lower:
                # TTTBVV is stored individually in its own sheet, named after the file
                group_data["TTTBVV"][f.file_name] = df_sum
            else:
                df_sum.insert(0, "SourceFile", f.file_name)
                group_data["ShortTerm"].append(df_sum)
                
        except Exception as e:
            print(f"Error reading result sheet from {f.file_name}: {e}")
            continue
            
    # Create final summarized workbook
    wb_new = openpyxl.Workbook()
    if "Sheet" in wb_new.sheetnames:
        wb_new.remove(wb_new["Sheet"])
        
    accounting_format = "#,##0"
    
    # Write aggregated sheets (LT, Travel, ShortTerm)
    for grp in ["LT", "Travel", "ShortTerm"]:
        list_dfs = group_data[grp]
        if list_dfs:
            merged = pd.concat(list_dfs, ignore_index=True)
            ws = wb_new.create_sheet(grp)
            write_df_to_sheet(ws, merged, with_filter=True)
            
            # Apply format to numeric columns
            n_rows = len(merged)
            for r in range(2, n_rows + 2):
                for c in range(1, len(merged.columns) + 1):
                    val = ws.cell(row=r, column=c).value
                    if isinstance(val, (int, float)):
                        cell = ws.cell(row=r, column=c)
                        cell.number_format = accounting_format
                        
            # Auto-fit columns
            for col in ws.columns:
                max_len = max(len(str(cell.value or "")) for cell in col)
                col_letter = get_column_letter(col[0].column)
                ws.column_dimensions[col_letter].width = max(max_len + 3, 12)
                
    # Write TTTBVV sheets individually
    for nm, df_sum in group_data["TTTBVV"].items():
        ws = wb_new.create_sheet(nm)
        write_df_to_sheet(ws, df_sum, with_filter=True)
        
        # Apply format to numeric columns
        n_rows = len(df_sum)
        for r in range(2, n_rows + 2):
            for c in range(1, len(df_sum.columns) + 1):
                val = ws.cell(row=r, column=c).value
                if isinstance(val, (int, float)):
                    cell = ws.cell(row=r, column=c)
                    cell.number_format = accounting_format
                    
        # Auto-fit columns
        for col in ws.columns:
            max_len = max(len(str(cell.value or "")) for cell in col)
            col_letter = get_column_letter(col[0].column)
            ws.column_dimensions[col_letter].width = max(max_len + 3, 12)
            
    # Save output file with timestamp
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    summary_filename = f"Tong_Hop_Result_{timestamp}.xlsx"
    summary_dir = os.path.join(OUTPUT_EXCEL_ROOT, quarter_id)
    os.makedirs(summary_dir, exist_ok=True)
    summary_path = os.path.join(summary_dir, summary_filename)
    
    wb_new.save(summary_path)
    
    return {"file_name": summary_filename, "file_path": summary_path}
