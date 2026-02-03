@echo off
echo running fox.callsign.bat
call fox.callsign.bat "sync starting"

echo running vortex.bat
call vortex.bat "status check"

echo running rtr-101_workflow.bat
call rtr-101_workflow.bat rtr-101 "monitoring sync"

echo running vortex.callsign-opperator.bat
call vortex.callsign-opperator.bat "operator ready"

echo all scripts executed. check workflow.log for entries
pause
