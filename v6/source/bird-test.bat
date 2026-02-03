@echo off
REM Run bird tests on an agent
REM Usage: bird-test.bat <agent-name> [suite]
REM Example: bird-test.bat agent-test echo

if "%~1"=="" (
    echo Usage: bird-test.bat ^<agent-name^> [suite]
    echo Suites: echo, fly, llm-raw, llm-wiki, request, full
    echo Example: bird-test.bat agent-test echo
    exit /b 1
)

set AGENT=%~1
set SUITE=%~2

pushd %~dp0
if not exist "bird-context" mkdir bird-context
cd bird-context

if "%SUITE%"=="" (
    call "%~dp0..\dist-newstyle\build\x86_64-windows\ghc-9.6.7\bird-0.1.0.0\x\bird\build\bird\bird.exe" test %AGENT%
) else (
    call "%~dp0..\dist-newstyle\build\x86_64-windows\ghc-9.6.7\bird-0.1.0.0\x\bird\build\bird\bird.exe" test %AGENT% %SUITE%
)

popd
