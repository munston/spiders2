@echo off
setlocal enabledelayedexpansion
echo === Killing All Postboxes ===
echo.

set count=0

for /D %%d in (*) do (
    if exist "%%d\inbox" (
        if exist "%%d\currently-on.txt" (
            echo Sending pkill to: %%d
            echo. > "%%d\inbox\pkill-killall.txt"
            set /a count+=1
        )
    )
)

if !count!==0 (
    echo No active postboxes found.
) else (
    echo.
    echo Sent pkill to !count! postbox(es^).
    echo Agents should shut down shortly.
)
endlocal
