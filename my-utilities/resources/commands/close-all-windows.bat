:: from http://ysgitdiary.blogspot.fr/2011/03/how-to-close-all-active-windows-on.html
:: except I changed the /END command to cmdow which just kills processes to /CLS because it asks them to close
@echo off

:: find the Handle of the window of this batch file 
for /f %%l in ('%~dp0cmdow\cmdow @') do set c=%%l

:: find the Handle of the Pomodoro window
:: test beforehand the command succeeds
set d=""
%~dp0cmdow\cmdow "My Pomodoro" >nul 2>&1 && for /f %%l in ('%~dp0cmdow\cmdow "My Pomodoro"') do set d=%%l

:: find the handle for Firefox
:: do not want to kill streaming music!
:: test beforehand the command succeeds
set e=""
for /f "tokens=1,8" %%a in ('%~dp0cmdow\cmdow /T /B') do if %%b==firefox set e=%%a

:: find the handle for foobar2000, just in case
:: test beforehand the command succeeds
set f=""
for /f "tokens=1,8" %%a in ('%~dp0cmdow\cmdow /T /B') do if %%b==foobar20 set f=%%a

:: for each window, if it is not the one of this batch file
:: close it by killing its process
for /f %%i in ('%~dp0cmdow\cmdow /t /b') do if not %%i==%c% if not %%i==%d% if not %%i==%e% if not %%i==%f% %~dp0cmdow\cmdow %%i /CLS

:: activate the taskbar
::cmd /c explorer.exe