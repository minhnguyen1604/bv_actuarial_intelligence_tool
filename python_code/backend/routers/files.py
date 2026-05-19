# -*- coding: utf-8 -*-
import io
import os
import shutil
import pandas as pd
from fastapi import APIRouter, Depends, HTTPException, UploadFile, File, Form
from fastapi.responses import StreamingResponse
from sqlalchemy.orm import Session
from typing import List

import models
import schemas
from database import get_db
from services.excel_classifier import get_group_code
from services.form_validator import validate_form
from services.file_merger import merge_file

router = APIRouter(
    prefix="/api/files",
    tags=["files"],
)

UPLOAD_DIR = "uploads"

@router.post("/upload", response_model=schemas.FileQueue)
async def upload_file(
    quarter_id: str = Form(...),
    sheet_name: str = Form(...),
    file: UploadFile = File(...),
    db: Session = Depends(get_db)
):
    if not file.filename.lower().endswith(('.xls', '.xlsx', '.xlsb', '.xlsm')):
        raise HTTPException(status_code=400, detail="Only Excel files are allowed.")
        
    quarter_dir = os.path.join(UPLOAD_DIR, quarter_id)
    os.makedirs(quarter_dir, exist_ok=True)
    file_path = os.path.join(quarter_dir, file.filename)
    
    with open(file_path, "wb") as buffer:
        shutil.copyfileobj(file.file, buffer)
        
    group_code = get_group_code(file.filename, sheet_name)
    if not group_code:
        raise HTTPException(status_code=400, detail="Cannot determine Group Code from file name and sheet name. Ensure keywords exist.")

    term = "N/A"
    if group_code.endswith("_LT"):
        term = "Long Term"
    elif group_code.endswith("_ST"):
        term = "Short Term"
        
    existing_file = db.query(models.FileQueue).filter(
        models.FileQueue.quarter_id == quarter_id,
        models.FileQueue.file_name == file.filename,
        models.FileQueue.sheet_name == sheet_name
    ).first()
    
    if existing_file:
        existing_file.file_path = file_path
        existing_file.group_code = group_code
        existing_file.term = term
        existing_file.status = "Pending"
        db.commit()
        db.refresh(existing_file)
        return existing_file
    else:
        new_file = models.FileQueue(
            quarter_id=quarter_id,
            file_name=file.filename,
            sheet_name=sheet_name,
            file_path=file_path,
            group_code=group_code,
            term=term,
            status="Pending"
        )
        db.add(new_file)
        db.commit()
        db.refresh(new_file)
        return new_file

@router.get("/", response_model=List[schemas.FileQueue])
def list_files(quarter_id: str, db: Session = Depends(get_db)):
    files = db.query(models.FileQueue).filter(models.FileQueue.quarter_id == quarter_id).all()
    return files

@router.post("/{file_id}/check")
def check_file(file_id: int, db: Session = Depends(get_db)):
    file_record = db.query(models.FileQueue).filter(models.FileQueue.id == file_id).first()
    if not file_record:
        raise HTTPException(status_code=404, detail="File not found in queue.")

    res = validate_form(file_record.file_path, file_record.sheet_name, file_record.group_code)

    if res["ok"] and not res.get("errors"):
        # Only mark Validated when no fatal errors; warnings are allowed
        file_record.status = "Validated"
        db.commit()

    return res

@router.get("/{file_id}/check-report")
def download_check_report(file_id: int, db: Session = Depends(get_db)):
    """
    Re-run validation and return results as a downloadable Excel file.
    """
    import openpyxl
    from openpyxl.styles import PatternFill, Font, Alignment, Border, Side

    file_record = db.query(models.FileQueue).filter(models.FileQueue.id == file_id).first()
    if not file_record:
        raise HTTPException(status_code=404, detail="File not found.")

    res = validate_form(file_record.file_path, file_record.sheet_name, file_record.group_code)

    # Build Excel workbook
    wb = openpyxl.Workbook()
    ws = wb.active
    ws.title = "Validation Report"

    # Styles
    header_fill = PatternFill("solid", fgColor="1E3A5F")
    header_font = Font(color="FFFFFF", bold=True, size=11)
    ok_font    = Font(color="059669", bold=True)
    err_font   = Font(color="DC2626", bold=True)
    warn_fill  = PatternFill("solid", fgColor="FEF9C3")
    thin = Border(
        left=Side(style="thin", color="E5E7EB"),
        right=Side(style="thin", color="E5E7EB"),
        top=Side(style="thin", color="E5E7EB"),
        bottom=Side(style="thin", color="E5E7EB"),
    )
    center = Alignment(horizontal="center", vertical="center")

    # Title
    ws.merge_cells("A1:C1")
    ws["A1"] = "Validation Report - " + file_record.file_name
    ws["A1"].font = Font(bold=True, size=13)
    ws["A1"].alignment = center
    ws.row_dimensions[1].height = 28

    # Meta rows
    meta = [
        ("File", file_record.file_name),
        ("Sheet", file_record.sheet_name),
        ("Group Code", file_record.group_code),
        ("Status", "PASSED" if res["ok"] else "FAILED"),
    ]
    for i, (k, v) in enumerate(meta, start=2):
        ws.cell(row=i, column=1, value=k).font = Font(bold=True)
        ws.cell(row=i, column=2, value=v)

    # Stats header
    stat_start = len(meta) + 3
    ws.cell(row=stat_start, column=1, value="Metric").fill = header_fill
    ws.cell(row=stat_start, column=1).font = header_font
    ws.cell(row=stat_start, column=2, value="Value").fill = header_fill
    ws.cell(row=stat_start, column=2).font = header_font

    stats = res.get("stats", {})
    stat_rows = [
        ("Total rows",     stats.get("total_rows", "-")),
        ("Date errors",    stats.get("date_errors", 0)),
        ("Money errors",   stats.get("money_errors", 0)),
        ("Duplicate rows", stats.get("duplicate_rows", 0)),
    ]
    for j, (label, value) in enumerate(stat_rows, start=stat_start + 1):
        c_label = ws.cell(row=j, column=1, value=label)
        c_value = ws.cell(row=j, column=2, value=value)
        c_label.border = thin
        c_value.border = thin
        is_err = isinstance(value, int) and value > 0 and label != "Total rows"
        c_value.font = err_font if is_err else ok_font

    # Warnings
    warnings = res.get("warnings", [])
    if warnings:
        warn_start = stat_start + len(stat_rows) + 2
        ws.cell(row=warn_start, column=1, value="Warnings").font = Font(bold=True)
        for k, w in enumerate(warnings, start=warn_start + 1):
            cell = ws.cell(row=k, column=1, value=w)
            cell.fill = warn_fill
            ws.merge_cells(start_row=k, start_column=1, end_row=k, end_column=3)

    # Errors
    errors = res.get("errors", [])
    if errors:
        err_start = stat_start + len(stat_rows) + len(warnings) + 3
        ws.cell(row=err_start, column=1, value="Errors").font = Font(bold=True, color="DC2626")
        for k, e in enumerate(errors, start=err_start + 1):
            cell = ws.cell(row=k, column=1, value=e)
            cell.font = Font(color="DC2626")
            ws.merge_cells(start_row=k, start_column=1, end_row=k, end_column=3)

    ws.column_dimensions["A"].width = 28
    ws.column_dimensions["B"].width = 20
    ws.column_dimensions["C"].width = 40

    buf = io.BytesIO()
    wb.save(buf)
    buf.seek(0)

    from urllib.parse import quote
    import unicodedata
    safe_name = file_record.file_name.replace(".xlsx", "").replace(".xls", "")
    filename_xlsx = f"ValidationReport_{safe_name}.xlsx"
    # ASCII fallback for plain filename field (latin-1 safe)
    ascii_name = unicodedata.normalize("NFKD", filename_xlsx).encode("ascii", "ignore").decode("ascii")
    ascii_name = ascii_name.replace(" ", "_") or f"ValidationReport_{file_record.id}.xlsx"
    # RFC 5987 encoded name for full Unicode support
    encoded_name = quote(filename_xlsx, safe="")

    return StreamingResponse(
        buf,
        media_type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        headers={"Content-Disposition": f"attachment; filename=\"{ascii_name}\"; filename*=UTF-8''{encoded_name}"},
    )

@router.post("/{file_id}/merge")
def merge_file_endpoint(file_id: int, db: Session = Depends(get_db)):
    """
    Normalise a validated Excel file and save it as parquet in cur_data/.
    Also merges with the previous quarter's data if available.
    """
    file_record = db.query(models.FileQueue).filter(models.FileQueue.id == file_id).first()
    if not file_record:
        raise HTTPException(status_code=404, detail="File not found in queue.")

    if file_record.status not in ("Validated", "Pending"):
        raise HTTPException(status_code=400, detail="File must be Validated before merging.")

    # Get calculation date from parameters (for end-date filtering)
    from datetime import date as _date
    param = db.query(models.AppParameter).filter(
        models.AppParameter.quarter_id == file_record.quarter_id
    ).first()
    dpnv_date = None
    if param:
        try:
            dpnv_date = _date(param.year, param.month, param.day)
        except Exception:
            pass

    res = merge_file(
        file_path=file_record.file_path,
        sheet_name=file_record.sheet_name,
        group_code=file_record.group_code,
        quarter_id=file_record.quarter_id,
        dpnv_date=dpnv_date,
    )

    if res["ok"]:
        file_record.status = "Merged"
        db.commit()

    return res

@router.delete("/clear")
def clear_files(quarter_id: str, db: Session = Depends(get_db)):
    db.query(models.FileQueue).filter(models.FileQueue.quarter_id == quarter_id).delete()
    db.commit()
    
    quarter_dir = os.path.join(UPLOAD_DIR, quarter_id)
    if os.path.exists(quarter_dir):
        shutil.rmtree(quarter_dir, ignore_errors=True)
        
    return {"ok": True, "message": "All files cleared."}
