@echo off
set /p topic="Enter topic: "
echo %topic%> inbox\%topic%.txt
echo Posted: inbox\%topic%.txt
pause
