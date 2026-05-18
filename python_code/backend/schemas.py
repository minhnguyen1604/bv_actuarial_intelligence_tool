from pydantic import BaseModel
from typing import List, Optional
import datetime

class AppParameterBase(BaseModel):
    quarter_id: str
    day: int
    month: int
    year: int

class AppParameterCreate(AppParameterBase):
    pass

class AppParameter(AppParameterBase):
    id: int
    class Config:
        orm_mode = True

class FXRateBase(BaseModel):
    quarter_id: str
    currency: str
    rate: float

class FXRateCreate(FXRateBase):
    pass

class FXRate(FXRateBase):
    id: int
    class Config:
        orm_mode = True
        
class FXRatesListUpdate(BaseModel):
    quarter_id: str
    rates: List[FXRateCreate]

class FileQueueBase(BaseModel):
    quarter_id: str
    file_name: str
    sheet_name: Optional[str] = None
    group_code: Optional[str] = None
    term: Optional[str] = None
    status: str = "Pending"

class FileQueue(FileQueueBase):
    id: int
    file_path: str
    uploaded_at: datetime.datetime
    class Config:
        orm_mode = True
