@echo off
REM %~dp0 is the directory where this very script is being runned

REM load key bindings on start
schtasks /create /f /tn "Keybindings" /tr "\"C:\Program Files\AutoHotkey\AutoHotkey.exe\" %~dp0autohotkey.ahk" /sc onlogon

REM Pomodoro on startup
schtasks /f /create /tn "Pomodoro" /tr "nw %~dp0\pomodoro\pomodoro.nw" /sc onlogon

REM schedule backup
REM schtasks /create /f /tn "Backup" /tr "%~dp0backup.bat" /sc HOURLY /mo 2
REM schtasks /create /f /tn "Full-Backup" /tr "%~dp0full-backup.bat" /sc WEEKLY /d SUN

pause