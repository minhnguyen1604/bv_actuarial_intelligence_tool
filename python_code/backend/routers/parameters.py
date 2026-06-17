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

@router.get("/fx-rates", response_model=List[schemas.FXRate])
def get_all_fx_rates(db: Session = Depends(get_db)):
    rates = db.query(models.FXRate).all()
    return rates

@router.get("/fx-rates/{quarter_id}", response_model=List[schemas.FXRate])
def get_fx_rates(quarter_id: str, db: Session = Depends(get_db)):
    rates = db.query(models.FXRate).filter(models.FXRate.quarter_id == quarter_id).all()
    return rates

@router.post("/fx-rates/sync/{quarter_id}", response_model=List[schemas.FXRate])
def sync_fx_rates_from_rds(quarter_id: str, db: Session = Depends(get_db)):
    thoi_gian = quarter_id.replace("_", "/")
    
    try:
        import os
        import rdata
        import pandas as pd
        rds_path = "d:/bv_intelligence_tool/r_code_upr/ty_gia.rds"
        if not os.path.exists(rds_path):
            raise HTTPException(status_code=404, detail="RDS file not found")
        parsed = rdata.parser.parse_file(rds_path)
        converted = rdata.conversion.convert(parsed)
        if isinstance(converted, dict) and "ty_gia" in converted:
            df = converted["ty_gia"]
        else:
            df = converted
            
        df["Thoi_gian"] = df["Thoi_gian"].astype(str)
        row = df[df["Thoi_gian"] == thoi_gian]
        if row.empty:
            raise HTTPException(status_code=404, detail=f"No default exchange rates found for quarter {thoi_gian} in RDS")
            
        db.query(models.FXRate).filter(models.FXRate.quarter_id == quarter_id).delete()
        
        saved_rates = []
        currencies = [col for col in df.columns if col != "Thoi_gian"]
        
        for cur in currencies:
            val = row.iloc[0][cur]
            if pd.isna(val):
                continue
            db_rate = models.FXRate(
                quarter_id=quarter_id,
                currency=cur,
                rate=float(val)
            )
            db.add(db_rate)
            saved_rates.append(db_rate)
            
        db.commit()
        for rate in saved_rates:
            db.refresh(rate)
        return saved_rates
    except Exception as e:
        if isinstance(e, HTTPException):
            raise e
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/fx-rates/single", response_model=schemas.FXRate)
def save_single_fx_rate(data: schemas.FXRateCreate, db: Session = Depends(get_db)):
    db_rate = db.query(models.FXRate).filter(
        models.FXRate.quarter_id == data.quarter_id,
        models.FXRate.currency == data.currency
    ).first()
    
    if db_rate:
        db_rate.rate = data.rate
    else:
        db_rate = models.FXRate(
            quarter_id=data.quarter_id,
            currency=data.currency,
            rate=data.rate
        )
        db.add(db_rate)
        
    db.commit()
    db.refresh(db_rate)
    return db_rate

@router.delete("/fx-rates/single/{quarter_id}/{currency}")
def delete_single_fx_rate(quarter_id: str, currency: str, db: Session = Depends(get_db)):
    db.query(models.FXRate).filter(
        models.FXRate.quarter_id == quarter_id,
        models.FXRate.currency == currency
    ).delete()
    db.commit()
    return {"status": "deleted", "quarter_id": quarter_id, "currency": currency}

