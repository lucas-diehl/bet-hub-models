@echo off
REM Launcher for the hands-off weekly feed update (called by Task Scheduler).
cd /d "C:\Users\ljdie\OneDrive\Documents\cfb-modeling"
echo ================ %DATE% %TIME% ================ >> weekly_update.log
"C:\Program Files\R\R-4.3.3\bin\Rscript.exe" weekly_update.R >> weekly_update.log 2>&1
echo exit code %ERRORLEVEL% >> weekly_update.log
