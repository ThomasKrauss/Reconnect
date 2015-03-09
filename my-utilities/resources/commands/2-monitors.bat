%~dp0multimonitortool-x64/MultiMonitorTool.exe /LoadConfig "%~dp0multimonitortool-x64\monitors-position.cfg"
%~dp0multimonitortool-x64/MultiMonitorTool.exe /disable \\.\DISPLAY3
%~dp0multimonitortool-x64/MultiMonitorTool.exe /enable \\.\DISPLAY2
%~dp0multimonitortool-x64/MultiMonitorTool.exe /setmax \\.\DISPLAY1 \\.\DISPLAY2