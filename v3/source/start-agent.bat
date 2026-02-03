@echo off
REM Start an agent with the given name and model
REM Usage: start-agent.bat <name> <model>
REM Example: start-agent.bat agent-groq groq

if "%~1"=="" (
    echo Usage: start-agent.bat ^<name^> ^<model^>
    echo   model: groq ^| huggingface ^| hf
    echo.
    echo Example: start-agent.bat agent-groq groq
    exit /b 1
)

if "%~2"=="" (
    echo Usage: start-agent.bat ^<name^> ^<model^>
    echo   model: groq ^| huggingface ^| hf
    exit /b 1
)

set NAME=%~1
set MODEL=%~2

REM Create postbox directory if it doesn't exist
if not exist "%NAME%" mkdir "%NAME%"

REM Copy the spider executable if needed
set SPIDER_EXE=..\dist-newstyle\build\x86_64-windows\ghc-9.6.7\spider-app-0.1.0.0\x\spider\build\spider\spider.exe

REM Start the agent in its own directory
echo Starting %NAME% with model %MODEL%...
start "%NAME%" cmd /k "cd %NAME% && %SPIDER_EXE% %NAME% %MODEL%"
