from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from fastapi.responses import RedirectResponse

import models
from database import engine
from routers import parameters, files, upr, vas_analysis

models.Base.metadata.create_all(bind=engine)

app = FastAPI(title="BaoViet Actuarial Platform")

@app.get("/api/health")
async def health_check():
    return {"status": "ok", "message": "BaoViet Actuarial Platform is running"}

app.include_router(parameters.router)
app.include_router(files.router)
app.include_router(upr.router)
app.include_router(vas_analysis.router)

@app.get("/")
async def root():
    return RedirectResponse(url="/login.html")

# Serve the frontend directory at the root path
app.mount("/", StaticFiles(directory="../frontend", html=True), name="static")
