@echo off
echo === Postbox Status ===
echo.

set online=0
set offline=0

for /D %%d in (*) do (
    if exist "%%d\inbox" (
        if exist "%%d\currently-on.txt" (
            echo [ONLINE]  %%d
            for /f "delims=" %%i in ('type "%%d\currently-on.txt"') do echo           %%i
            set /a online+=1
        ) else (
            echo [offline] %%d
            set /a offline+=1
        )
    )
)

echo.
echo Summary: %online% online, %offline% offline
echo.
echo Commands:
echo   start-agent.bat ^<name^> ^<model^>  - Start a new agent
echo   send-message.bat ^<name^> ^<msg^>   - Send message to agent
echo   kill-all.bat                     - Stop all agents
