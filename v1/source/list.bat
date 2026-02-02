@echo off
echo === Discovered Postboxes ===
echo.

for /D %%d in (*) do (
    if exist "%%d\inbox" (
        echo   %%d
    )
)

echo.
