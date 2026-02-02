@echo off
REM Run bird CLI with any arguments
REM Usage: bird-run.bat <args...>
REM Example: bird-run.bat list
REM Example: bird-run.bat send agent-1 "echo:hello"
REM
REM Note: Bird uses fly which expects batch files in parent directory.
REM We create a temp subdirectory context to make the paths work.

pushd %~dp0
REM Create temp dir for bird context if needed
if not exist "bird-context" mkdir bird-context
cd bird-context

REM Run bird from this context (batch files are now in parent = source/)
call "%~dp0..\dist-newstyle\build\x86_64-windows\ghc-9.6.7\bird-0.1.0.0\x\bird\build\bird\bird.exe" %*

popd
