@echo off
REM Send a compare request to an agent
REM Usage: compare.bat <agent-name> <msg1> <msg2> [msg3] ...
REM Example: compare.bat agent-groq "say hi" "say hello" "say greetings"
REM
REM The agent will process each message, score the responses, and return ranked results.
REM Messages are separated by ;;; in the request.

if "%~1"=="" (
    echo Usage: compare.bat ^<agent-name^> ^<msg1^> ^<msg2^> [msg3] ...
    echo Example: compare.bat agent-groq "say hi" "say hello"
    exit /b 1
)

if "%~2"=="" (
    echo Error: Need at least one message to compare
    exit /b 1
)

set AGENT=%~1
shift

REM Build message list separated by ;;;
set MSGS=
:loop
if "%~1"=="" goto done
if "%MSGS%"=="" (
    set MSGS=%~1
) else (
    set MSGS=%MSGS%;;;%~1
)
shift
goto loop

:done
if not exist "%AGENT%\inbox" (
    echo Agent not found: %AGENT%
    exit /b 1
)

REM Generate timestamp for filename
for /f "tokens=1-4 delims=:.," %%a in ("%time%") do set TIMESTAMP=%%a%%b%%c%%d
set FILENAME=compare-%TIMESTAMP%.txt

REM Send compare request with special prefix
echo compare:%MSGS%> "%AGENT%\inbox\%FILENAME%"
echo Sent compare request to %AGENT%
echo Messages: %MSGS%
echo File: %AGENT%\inbox\%FILENAME%
