@echo off

REM Close all windows: there might be things to save!
call %~dp0close-all-windows.bat


REM Backup before shutting the computer down
wscript %~dp0backup\backup-dev.vbs %~dp0backup\idrive-backup-data.txt %~dp0backup\local-backup-data.txt


REM Shutdown
C:\Windows\System32\shutdown.exe -s -f -t 0