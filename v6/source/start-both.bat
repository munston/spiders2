@echo off
REM Start both Groq and HuggingFace agents for testing

echo === Starting Both Agents ===
echo.

call start-agent.bat agent-groq groq
timeout /t 2 /nobreak >nul

call start-agent.bat agent-hf hf
timeout /t 2 /nobreak >nul

echo.
echo Both agents started. Use status.bat to check.
echo Use kill-all.bat to stop all agents.
