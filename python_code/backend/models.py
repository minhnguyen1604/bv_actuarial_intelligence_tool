from sqlalchemy import Column, Integer, String, Float, DateTime
from database import Base
import datetime

class AppParameter(Base):
    __tablename__ = "app_parameters"

    id = Column(Integer, primary_key=True, index=True)
    quarter_id = Column(String, unique=True, index=True) # e.g. "Q1_2026"
    day = Column(Integer)
    month = Column(Integer)
    year = Column(Integer)

class FXRate(Base):
    __tablename__ = "fx_rates"

    id = Column(Integer, primary_key=True, index=True)
    quarter_id = Column(String, index=True) # e.g. "Q1_2026"
    currency = Column(String, index=True) # e.g. "USD"
    rate = Column(Float)

class FileQueue(Base):
    __tablename__ = "file_queue"

    id = Column(Integer, primary_key=True, index=True)
    quarter_id = Column(String, index=True) # e.g. "Q1_2026"
    file_name = Column(String)
    sheet_name = Column(String, nullable=True) # Stores the specific sheet used
    file_path = Column(String)
    group_code = Column(String) # e.g. "Eng_LT", "Vietjet"
    term = Column(String) # "Long Term", "Short Term", or "N/A"
    status = Column(String, default="Pending")
    uploaded_at = Column(DateTime, default=datetime.datetime.utcnow)
