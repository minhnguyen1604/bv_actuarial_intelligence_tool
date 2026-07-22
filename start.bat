@echo off
title BVGI Intelligence Tool Starter
echo ==========================================================
echo           Starting BVGI Intelligence Tool
echo ==========================================================
echo.

echo [1/3] Starting FastAPI backend server...
cd /d "%~dp0python_code"
start "BVGI Intelligence Tool Server" /min "%~dp0python_code\.venv\Scripts\python.exe" -m uvicorn backend.main:app --host 127.0.0.1 --port 8000

echo [2/3] Waiting for the server to spin up (3 seconds)...
timeout /t 3 /nobreak >nul

echo [3/3] Launching application in default web browser...
start http://127.0.0.1:8000/index_upr_calculation.html

echo.
echo ==========================================================
echo  FastAPI Server is now running in the background.
echo ==========================================================
echo.
echo Closing starter window in 3 seconds...
timeout /t 3 >nul
exit
