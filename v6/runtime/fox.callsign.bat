@echo off
:: fox-01.bat
set CALLSIGN=fox-01
if "%~1"=="" (
    echo [%CALLSIGN%] usage: %~nx0 "sync_status"
    exit /b
)
echo [%CALLSIGN%] sync: %~1
echo [%CALLSIGN%] %date% %time% : %~1 >> workflow.log