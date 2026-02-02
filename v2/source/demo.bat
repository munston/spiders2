@echo off
REM Demo: Multi-agent postbox system
REM This demonstrates starting multiple agents and sending messages

echo ========================================
echo   Spider Multi-Agent Demo
echo ========================================
echo.

echo Step 1: Checking current status...
call status.bat
echo.
pause

echo Step 2: Starting Groq agent...
call start-agent.bat agent-groq groq
timeout /t 3 /nobreak >nul
echo.

echo Step 3: Starting HuggingFace agent...
call start-agent.bat agent-hf hf
timeout /t 3 /nobreak >nul
echo.

echo Step 4: Checking status again...
call status.bat
echo.
pause

echo Step 5: Sending test query to agent-groq...
call send-message.bat agent-groq "quantum computing"
echo.
pause

echo Step 6: Sending test query to agent-hf...
call send-message.bat agent-hf "machine learning"
echo.
pause

echo Step 7: Final status check...
call status.bat
echo.

echo ========================================
echo Demo complete!
echo.
echo Check agent-groq\outbox\ and agent-hf\outbox\ for results.
echo Use kill-all.bat to stop all agents.
echo ========================================
