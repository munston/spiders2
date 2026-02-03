@echo off
REM Quick ping test via bird
REM Usage: bird-ping.bat <agent-name>

if "%~1"=="" (
    echo Usage: bird-ping.bat ^<agent-name^>
    exit /b 1
)

pushd %~dp0
if not exist "bird-context" mkdir bird-context
cd bird-context

call "%~dp0..\dist-newstyle\build\x86_64-windows\ghc-9.6.7\bird-0.1.0.0\x\bird\build\bird\bird.exe" ping %~1

popd
