import os
import shutil
import pandas as pd
from fastapi import APIRouter, Depends, HTTPException, UploadFile, File, Form
from sqlalchemy.orm import Session
from typing import List

import models
import schemas
from database import get_db
from services.excel_classifier import get_group_code
from services.form_validator import validate_form

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
    
    if res["ok"]:
        file_record.status = "Validated"
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
