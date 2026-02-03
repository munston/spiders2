@echo off
:: vortex.bat
set tag=vortex-9#2b
if "%~1"=="" (
    echo [%tag%] usage: vortex.bat "logic_update"
    exit /b
)
echo [%tag%] logic: %~1
echo [%tag%] %date% %time%: %~1 >> minutes.log