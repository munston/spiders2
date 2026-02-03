@echo off
REM Watch all outbox directories for new files
REM Usage: watch-outputs.bat

echo === Watching Outboxes ===
echo Press Ctrl+C to stop.
echo.

:loop
cls
echo === Outbox Contents === [%time%]
echo.

for /D %%d in (*) do (
    if exist "%%d\outbox" (
        echo --- %%d\outbox ---
        dir /b "%%d\outbox" 2>nul || echo   (empty)
        echo.
    )
)

timeout /t 5 /nobreak >nul
goto loop
