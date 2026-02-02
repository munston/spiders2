@echo off
REM rtr-101_workflow.bat - safe parameter handling

setlocal enabledelayedexpansion

REM callsign
set CALLSIGN=rtr-101

REM default message if none provided
if "%~2"=="" (
    set MSG=status: ready
) else (
    set MSG=%~2
)

REM initialize log
set LOGFILE=workflow.log
if not exist %LOGFILE% (
    echo Workflow started at %date% %time% > %LOGFILE%
)

REM write entry for the given callsign
echo [%CALLSIGN%] !MSG! >> %LOGFILE%

REM notify
echo [%CALLSIGN%] message logged.
pause
