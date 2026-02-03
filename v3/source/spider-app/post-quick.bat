@echo off
set timestamp=%time:~0,2%%time:~3,2%%time:~6,2%
set filename=query_%timestamp%.txt
echo Rust programming language> inbox\%filename%
echo Posted: inbox\%filename%
timeout /t 2
