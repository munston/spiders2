@echo off
cd /d "%~dp0"
cd ..
cabal run spider
pause
