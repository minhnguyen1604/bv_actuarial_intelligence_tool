from fastapi import APIRouter, Depends, HTTPException, BackgroundTasks
from fastapi.responses import FileResponse, StreamingResponse, JSONResponse
from sqlalchemy.orm import Session
from pydantic import BaseModel
from typing import List, Optional
import os
import uuid
import datetime
import re

import models
from database import get_db, SessionLocal
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

# numpy NaN/inf encoder helper
def sanitize_numpy(obj):
    import math
    if obj is None:
        return None
    type_name = type(obj).__name__
    if type_name in ("int64", "int32", "int16", "int8", "uint64", "uint32", "uint16", "uint8"):
        return int(obj)
    if type_name in ("float64", "float32", "float16"):
        if math.isnan(float(obj)) or math.isinf(float(obj)):
            return None
        return float(obj)
    if isinstance(obj, float):
        if math.isnan(obj) or math.isinf(obj):
            return None
        return obj
    if isinstance(obj, dict):
        return {str(k): sanitize_numpy(v) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [sanitize_numpy(i) for i in obj]
    return obj

import json
import multiprocessing

def get_task_file_path(task_id: str) -> str:
    tasks_dir = os.path.join(upr_calculator.OUTPUT_EXCEL_ROOT, "tasks")
    os.makedirs(tasks_dir, exist_ok=True)
    return os.path.join(tasks_dir, f"{task_id}.json")

def write_task_status(task_id: str, status: str, progress: int, message: str, error: str = None, result: dict = None):
    data = {
        "status": status,
        "progress": progress,
        "message": message,
        "error": error,
        "result": result
    }
    file_path = get_task_file_path(task_id)
    with open(file_path, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)

def read_task_status(task_id: str) -> dict:
    file_path = get_task_file_path(task_id)
    if not os.path.exists(file_path):
        return None
    with open(file_path, "r", encoding="utf-8") as f:
        try:
            return json.load(f)
        except Exception:
            return None

def async_calculate_upr(task_id: str, quarter_id: str, file_ids: list[int]):
    db = SessionLocal()
    try:
        write_task_status(task_id, "running", 10, "Khởi động tác vụ tính toán UPR...")
        
        # Fetch files to calculate
        query = db.query(models.FileQueue).filter(models.FileQueue.quarter_id == quarter_id)
        if file_ids is not None:
            query = query.filter(models.FileQueue.id.in_(file_ids))
        else:
            query = query.filter(models.FileQueue.status == "Merged")
            
        files_to_calc = query.all()
        total_files = len(files_to_calc)
        
        if total_files == 0:
            write_task_status(task_id, "success", 100, "Không có file nào cần tính toán.", result={"calculated": []})
            return

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

        results = []
        for idx, f in enumerate(files_to_calc):
            f.status = "Calculating"
            db.commit()
            
            progress = int(10 + (idx / total_files) * 80)
            write_task_status(task_id, "running", progress, f"Đang tính UPR cho nghiệp vụ {f.group_code} ({idx+1}/{total_files})...")
            
            try:
                out_path = upr_calculator.calculate_upr_for_file(
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

        write_task_status(task_id, "success", 100, "Tính toán hoàn thành!", result={"calculated": results})
    except Exception as e:
        import traceback
        traceback.print_exc()
        write_task_status(task_id, "failed", 100, f"Lỗi tính toán: {str(e)}", error=str(e))
    finally:
        db.close()

def async_summarize_upr(task_id: str, quarter_id: str, file_ids: list[int]):
    db = SessionLocal()
    try:
        write_task_status(task_id, "running", 20, "Khởi chạy tác vụ tổng hợp UPR...")
        
        res = upr_calculator.summarize_reports(
            db=db,
            quarter_id=quarter_id,
            file_ids=file_ids
        )
        
        write_task_status(task_id, "success", 100, "Tổng hợp hoàn thành!", result=res)
    except Exception as e:
        import traceback
        traceback.print_exc()
        write_task_status(task_id, "failed", 100, f"Lỗi tổng hợp: {str(e)}", error=str(e))
    finally:
        db.close()

@router.post("/calculate")
def calculate_upr(req: UPRCalculationRequest):
    task_id = str(uuid.uuid4())
    write_task_status(task_id, "running", 0, "Đang khởi chạy tác vụ tính toán UPR...")
    p = multiprocessing.Process(
        target=async_calculate_upr,
        args=(task_id, req.quarter_id, req.file_ids)
    )
    p.start()
    return {"task_id": task_id, "status": "running"}

@router.post("/summarize")
def summarize_upr(req: UPRSummaryRequest):
    task_id = str(uuid.uuid4())
    write_task_status(task_id, "running", 0, "Đang khởi chạy tác vụ tổng hợp UPR...")
    p = multiprocessing.Process(
        target=async_summarize_upr,
        args=(task_id, req.quarter_id, req.file_ids)
    )
    p.start()
    return {"task_id": task_id, "status": "running"}

@router.get("/task-status/{task_id}")
def get_task_status(task_id: str):
    task = read_task_status(task_id)
    if not task:
        raise HTTPException(status_code=404, detail="Task ID not found.")
    
    response_data = {
        "status": task["status"],
        "progress": task["progress"],
        "message": task["message"],
        "error": task["error"]
    }
    if task["status"] == "success":
        response_data["result"] = sanitize_numpy(task["result"])
    return JSONResponse(content=response_data)

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
