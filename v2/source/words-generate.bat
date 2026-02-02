@echo off
REM Generate words using a words-mode agent
REM Usage: words-generate.bat <agent-name> [count]
REM Example: words-generate.bat agent-words 5

if "%~1"=="" (
    echo Usage: words-generate.bat ^<agent-name^> [count]
    echo Example: words-generate.bat agent-words 5
    exit /b 1
)

set AGENT=%~1
set COUNT=%~2
if "%COUNT%"=="" set COUNT=5

if not exist "%AGENT%\inbox" (
    echo Agent not found: %AGENT%
    exit /b 1
)

REM Generate timestamp for filename
for /f "tokens=1-4 delims=:.," %%a in ("%time%") do set TIMESTAMP=%%a%%b%%c%%d
set FILENAME=words-%TIMESTAMP%.txt

REM Send words generation request
echo words:%COUNT%> "%AGENT%\inbox\%FILENAME%"
echo Sent words request to %AGENT%
echo File: %AGENT%\inbox\%FILENAME%
