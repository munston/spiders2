@echo off
REM Send a ping to an agent and check if it responds
REM Usage: ping.bat <agent-name>

if "%~1"=="" (
    echo Usage: ping.bat ^<agent-name^>
    exit /b 1
)

set AGENT=%~1

if not exist "%AGENT%\inbox" (
    echo Agent not found: %AGENT%
    exit /b 1
)

if not exist "%AGENT%\currently-on.txt" (
    echo Agent %AGENT% is offline.
    exit /b 1
)

echo Pinging %AGENT%...
echo ping> "%AGENT%\inbox\ping-test.txt"
echo Sent ping to %AGENT%.
echo Check %AGENT%\outbox\ for response.
