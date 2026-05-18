from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from typing import List

import models
import schemas
from database import get_db

router = APIRouter(
    prefix="/api/parameters",
    tags=["parameters"],
)

@router.post("/date", response_model=schemas.AppParameter)
def save_calculation_date(param: schemas.AppParameterCreate, db: Session = Depends(get_db)):
    db_param = db.query(models.AppParameter).filter(models.AppParameter.quarter_id == param.quarter_id).first()
    if db_param:
        db_param.day = param.day
        db_param.month = param.month
        db_param.year = param.year
    else:
        db_param = models.AppParameter(**param.dict())
        db.add(db_param)
    
    db.commit()
    db.refresh(db_param)
    return db_param

@router.get("/date/{quarter_id}", response_model=schemas.AppParameter)
def get_calculation_date(quarter_id: str, db: Session = Depends(get_db)):
    db_param = db.query(models.AppParameter).filter(models.AppParameter.quarter_id == quarter_id).first()
    if not db_param:
        raise HTTPException(status_code=404, detail="Parameters not found for this quarter")
    return db_param

@router.post("/fx-rates", response_model=List[schemas.FXRate])
def save_fx_rates(data: schemas.FXRatesListUpdate, db: Session = Depends(get_db)):
    # First, delete existing rates for this quarter to replace them
    db.query(models.FXRate).filter(models.FXRate.quarter_id == data.quarter_id).delete()
    
    saved_rates = []
    for rate_data in data.rates:
        db_rate = models.FXRate(**rate_data.dict())
        db.add(db_rate)
        saved_rates.append(db_rate)
        
    db.commit()
    for rate in saved_rates:
        db.refresh(rate)
    return saved_rates

@router.get("/fx-rates/{quarter_id}", response_model=List[schemas.FXRate])
def get_fx_rates(quarter_id: str, db: Session = Depends(get_db)):
    rates = db.query(models.FXRate).filter(models.FXRate.quarter_id == quarter_id).all()
    return rates
