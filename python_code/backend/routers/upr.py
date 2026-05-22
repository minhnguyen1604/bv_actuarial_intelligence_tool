from fastapi import APIRouter, Depends, HTTPException
from fastapi.responses import FileResponse, StreamingResponse
from sqlalchemy.orm import Session
from pydantic import BaseModel
from typing import List, Optional
import os

from database import get_db
from services import upr_calculator

router = APIRouter(
    prefix="/api/upr",
    tags=["upr"],
)

class UPRCalculationRequest(BaseModel):
    quarter_id: str
    file_ids: Optional[List[int]] = None

class UPRSummaryRequest(BaseModel):
    quarter_id: str
    file_ids: Optional[List[int]] = None

@router.post("/calculate")
def calculate_upr(req: UPRCalculationRequest, db: Session = Depends(get_db)):
    try:
        res = upr_calculator.calculate_upr_for_quarter(
            db=db,
            quarter_id=req.quarter_id,
            file_ids=req.file_ids
        )
        return res
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/summarize")
def summarize_upr(req: UPRSummaryRequest, db: Session = Depends(get_db)):
    try:
        res = upr_calculator.summarize_reports(
            db=db,
            quarter_id=req.quarter_id,
            file_ids=req.file_ids
        )
        return res
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.get("/reports/{quarter_id}")
def list_reports(quarter_id: str):
    folder_path = os.path.join(upr_calculator.OUTPUT_EXCEL_ROOT, quarter_id)
    if not os.path.exists(folder_path):
        return []
    
    files = []
    for f in os.listdir(folder_path):
        if f.endswith(".xlsx"):
            full_path = os.path.join(folder_path, f)
            stat = os.stat(full_path)
            files.append({
                "file_name": f,
                "size_bytes": stat.st_size,
                "modified_at": stat.st_mtime
            })
    return files

@router.get("/download/{quarter_id}/{file_name}")
def download_report(quarter_id: str, file_name: str):
    file_path = os.path.join(upr_calculator.OUTPUT_EXCEL_ROOT, quarter_id, file_name)
    if not os.path.exists(file_path):
        raise HTTPException(status_code=404, detail="File not found")
    return FileResponse(
        path=file_path,
        filename=file_name,
        media_type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

@router.get("/download-all/{quarter_id}")
def download_all_reports(quarter_id: str):
    import zipfile
    import io
    folder_path = os.path.join(upr_calculator.OUTPUT_EXCEL_ROOT, quarter_id)
    if not os.path.exists(folder_path):
        raise HTTPException(status_code=404, detail="No reports found for this quarter")
    
    files = [f for f in os.listdir(folder_path) if f.endswith(".xlsx")]
    if not files:
        raise HTTPException(status_code=404, detail="No report files to zip")
    
    zip_buffer = io.BytesIO()
    with zipfile.ZipFile(zip_buffer, "w", zipfile.ZIP_DEFLATED) as zip_file:
        for f in files:
            file_path = os.path.join(folder_path, f)
            zip_file.write(file_path, f)
    
    zip_buffer.seek(0)
    
    from urllib.parse import quote
    encoded_name = quote(f"Reports_{quarter_id}.zip", safe="")
    
    return StreamingResponse(
        zip_buffer,
        media_type="application/x-zip-compressed",
        headers={"Content-Disposition": f"attachment; filename=\"Reports_{quarter_id}.zip\"; filename*=UTF-8''{encoded_name}"}
    )

@router.delete("/clear-all/{quarter_id}")
def clear_all_reports(quarter_id: str):
    folder_path = os.path.join(upr_calculator.OUTPUT_EXCEL_ROOT, quarter_id)
    if os.path.exists(folder_path):
        for f in os.listdir(folder_path):
            if f.endswith(".xlsx"):
                try:
                    os.remove(os.path.join(folder_path, f))
                except Exception:
                    pass
    return {"ok": True, "message": "All reports cleared."}

@router.delete("/delete/{quarter_id}/{file_name}")
def delete_report(quarter_id: str, file_name: str):
    file_path = os.path.join(upr_calculator.OUTPUT_EXCEL_ROOT, quarter_id, file_name)
    if not os.path.exists(file_path):
        raise HTTPException(status_code=404, detail="File not found")
    try:
        os.remove(file_path)
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
    return {"ok": True, "message": f"Deleted {file_name}"}
