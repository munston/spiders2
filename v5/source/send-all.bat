@echo off
REM Send a message to all active agents' inboxes
REM Usage: send-all.bat <message>
REM Example: send-all.bat "What is quantum computing?"

if "%~1"=="" (
    echo Usage: send-all.bat ^<message^>
    echo Example: send-all.bat "What is quantum computing?"
    exit /b 1
)

set MSG=%~1
set COUNT=0

REM Generate a filename based on timestamp
for /f "tokens=1-4 delims=:.," %%a in ("%time%") do set TIMESTAMP=%%a%%b%%c%%d
set FILENAME=broadcast-%TIMESTAMP%.txt

REM Find all active postboxes (those with currently-on.txt)
for /d %%D in (*) do (
    if exist "%%D\currently-on.txt" (
        if exist "%%D\inbox" (
            echo %MSG%> "%%D\inbox\%FILENAME%"
            echo Sent to %%D
            set /a COUNT+=1
        )
    )
)

if %COUNT%==0 (
    echo No active postboxes found.
) else (
    echo Broadcast complete. Sent to %COUNT% agent(s).
)
