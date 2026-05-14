from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from fastapi.responses import RedirectResponse

app = FastAPI(title="BaoViet Actuarial Platform")

# Mount the frontend directory to serve static files
# We will use /api/ for our endpoints and serve frontend at the root
app.mount("/static", StaticFiles(directory="../frontend"), name="static")

@app.get("/")
async def root():
    return RedirectResponse(url="/static/login.html")

@app.get("/api/health")
async def health_check():
    return {"status": "ok", "message": "BaoViet Actuarial Platform is running"}
