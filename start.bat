@echo off
title BVGI Intelligence Tool Starter
echo ==========================================================
echo           Starting BVGI Intelligence Tool
echo ==========================================================
echo(

echo [1/4] Checking and freeing port 8000 if occupied...
powershell -NoProfile -Command "Get-NetTCPConnection -LocalPort 8000 -ErrorAction SilentlyContinue | Select-Object -ExpandProperty OwningProcess | Stop-Process -Force -ErrorAction SilentlyContinue"

echo [2/4] Starting FastAPI backend server...
cd /d "%~dp0python_code"
start "BVGI Intelligence Tool Server" /d "%~dp0python_code" /min "%~dp0python_code\.venv\Scripts\python.exe" -m uvicorn backend.main:app --host 127.0.0.1 --port 8000

echo [3/4] Waiting for server initialization (4 seconds)...
ping 127.0.0.1 -n 5 >nul

echo [4/4] Launching application in default web browser...
start http://127.0.0.1:8000/index_upr_calculation.html

echo(
echo ==========================================================
echo  FastAPI Server has been freshly initialized on port 8000.
echo ==========================================================
echo(
echo Closing starter window in 3 seconds...
ping 127.0.0.1 -n 4 >nul
exit

