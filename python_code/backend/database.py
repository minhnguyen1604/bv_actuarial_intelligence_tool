import os
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy.ext.declarative import declarative_base

# Ensure the data directory exists in backend/data
backend_dir = os.path.dirname(os.path.abspath(__file__))
data_dir = os.path.join(backend_dir, "data")
os.makedirs(data_dir, exist_ok=True)

db_path = os.path.join(data_dir, "parameters.db").replace("\\", "/")
SQLALCHEMY_DATABASE_URL = f"sqlite:///{db_path}"

engine = create_engine(
    SQLALCHEMY_DATABASE_URL, connect_args={"check_same_thread": False}
)

SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

Base = declarative_base()

def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()
