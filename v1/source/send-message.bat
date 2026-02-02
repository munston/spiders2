@echo off
REM Send a message to an agent's inbox
REM Usage: send-message.bat <agent-name> <message>
REM Example: send-message.bat agent-groq "What is the capital of France?"

if "%~1"=="" (
    echo Usage: send-message.bat ^<agent-name^> ^<message^>
    echo Example: send-message.bat agent-groq "What is quantum computing?"
    exit /b 1
)

if "%~2"=="" (
    echo Usage: send-message.bat ^<agent-name^> ^<message^>
    exit /b 1
)

set AGENT=%~1
set MSG=%~2

if not exist "%AGENT%\inbox" (
    echo Agent not found: %AGENT%
    echo Run 'list.bat' to see available postboxes.
    exit /b 1
)

REM Generate a filename based on timestamp
for /f "tokens=1-4 delims=:.," %%a in ("%time%") do set TIMESTAMP=%%a%%b%%c%%d
set FILENAME=query-%TIMESTAMP%.txt

echo %MSG%> "%AGENT%\inbox\%FILENAME%"
echo Sent to %AGENT%: %MSG%
echo File: %AGENT%\inbox\%FILENAME%
