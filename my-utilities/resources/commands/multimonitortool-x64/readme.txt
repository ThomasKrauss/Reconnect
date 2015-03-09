


MultiMonitorTool v1.35
Copyright (c) 2012 - 2013 Nir Sofer
Web site: http://www.nirsoft.net



Description
===========

MultiMonitorTool is a small tool that allows you to do some actions
related to working with multiple monitors. With MultiMonitorTool, you can
disable/enable monitors, set the primary monitor, save and load the
configuration of all monitors, and move windows from one monitor to
another. You can do these actions from the user interface or from
command-line, without displaying user interface. MultiMonitorTool also
provides a preview window, which allows you to watch a preview of every
monitor on your system.



System Requirements and Limitations
===================================


* This utility works on any version of Windows, starting from Windows
  XP and up to Windows 8. Both 32-bit and x64 systems are supported.
* This utility is only useful if your desktop is extended to multiple
  monitors. If you have multiple monitors that display the same content,
  then MultiMonitorTool is useless for you.



Versions History
================


* Version 1.35:
  o Added option to choose another font (name and size) for using on
    the main window.

* Version 1.32:
  o Fixed bug: MultiMonitorTool failed to detect windows that their
    top-left and right-bottom points are not located in any valid
    monitor. MultiMonitorTool also failed to move these windows with
    /MoveWindow command-line option.

* Version 1.31:
  o When you run MultiMonitorTool, it now automatically selects the
    monitor that you selected in the previous time you used
    MultiMonitorTool.
  o Added /SelectedMonitior command-line option, which allows you to
    open MultiMonitorTool with the selected monitor you need. For example:
    MultiMonitorTool.exe /SelectedMonitior \\.\DISPLAY2

* Version 1.30:
  o Added 'Maximum Resolution' column.
  o Added 'Set Maximum Resolution' option.
  o Added /setmax command-line option.
  o Fixed the status-bar flickering.

* Version 1.26:
  o Added command-line support for moving windows from all monitors
    to the primary monitor:
    MultiMonitorTool.exe /MoveWindow Primary All
  o Added /SetNextPrimary command-line option.

* Version 1.25:
  o Added 'Start As Hidden' option. When this option and 'Put Icon On
    Tray' option are turned on, the main window of MultiMonitorTool will
    be invisible on start.
  o Added /StartAsHidden command-line option.
  o Added 'Monitor Preview Window' to the tray menu.

* Version 1.21:
  o Added 'Auto Size Columns+Headers' option, which allows you to
    automatically resize the columns according to the row values and
    column headers.
  o Fixed issue: The properties dialog-box and other windows opened
    in the wrong monitor, on multi-monitors system.
  o Added 'Mark Odd/Even Rows' option, under the View menu. When it's
    turned on, the odd and even rows are displayed in different color, to
    make it easier to read a single line.

* Version 1.20:
  o Added new command-line options: /MonitorPreviewWindow ,
    /PreviewMode , /DrawCursorPreview , and more...

* Version 1.15:
  o Added 'Load Recent Monitors Config' menu, which allows you to
    easily load the last 10 monitors configurations that you previously
    saved or loaded.

* Version 1.10:
  o Added 'Draw Cursor In Monitor Preview' option.
  o Fixed the /WindowLeft, /WindowTop, /WindowWidth, /WindowHeight
    command-line options to work even if the window remains in the same
    monitor.

* Version 1.05:
  o Added 'Orientation' column (Default, 90 Degrees, 180 Degrees, or
    270 Degrees)
  o The display orientation is now saved into the monitors
    configuration file, and the orientation of the monitor is updated
    when you load the monitors configuration file.
  o Added 'Next' option to the /MoveWindow command-line parameter,
    which allows you to move the window to the next monitor.
  o Added 4 new command-line parameters: /WindowLeft, /WindowTop,
    /WindowWidth, /WindowHeight - which allows you to set the desired
    window size/position when moving the window to another monitor with
    /MoveWindow command-line option

* Version 1.00 - First release.




Using MultiMonitorTool
======================

MultiMonitorTool doesn't require any installation process or additional
dll files. In order to start using it, simply run the executable file -
MultiMonitorTool.exe

The main window of MultiMonitorTool contains 2 panes: The upper pane
displays the list of all monitors detected on your system. When you
select a monitor in the upper pane, the lower pane displays the details
of all visible windows on the selected monitor.

You can select one or more monitors in the upper pane, and then use the
following options: Disable Selected Monitors (Ctrl+F6), Enable Selected
Monitors (Ctrl+F7), Disable/Enable Switch (Ctrl+F8), or Set As Primary
Monitor (Ctrl+F9)
You can also select one or more Windows in the lower pane, and then use
the 'Move Window To Next Monitor' and 'Move Window To Primary Monitor'
options in order to easily move Windows from one monitor to the other.



Save/Load Monitors Configuration
================================

MultiMonitorTool allows you to save the current configuration of all
monitors on your system, including the screen resolution, colors depth,
and monitor position of every monitor, by using the 'Save Monitors
Configuration' option (Ctrl+Shift+S).
You can restore back the saved monitors configuration by using the 'Load
Monitors Configuration' option (Ctrl+Shift+L).
You can also save/load the monitors configuration from command-line, by
using the /SaveConfig and /LoadConfig command-line options.



Preview Window
==============

The preview window shows you a preview of the monitor you select in the
upper pane. It might be useful if non-primary monitors are turned off and
you want to view the windows displayed in the other monitors. In order to
enable/disable the monitor preview window, simply press F2. You can also
resize the preview window to any size you like.



Command-Line Options
====================

You can use the following command-line options to change the monitor
configuration or to move windows automatically without displaying any
user interface.
The <Monitor> parameter in all command-line options may contain the
following values:
* Primary - Specifies that you want to do the action on the primary
  monitor.
* Monitor Name, as it appears in the 'Name' column, for example:
  \\.\DISPLAY1 , \\.\DISPLAY2, \\.\DISPLAY3
* Monitor Number - The number that appears in the monitor name. (1 for
  \\.\DISPLAY1, 2 for \\.\DISPLAY2, and so on...)



/disable <Monitors>
Disables the specified monitors. You can specify a single monitor or
multiple monitors.
Examples:
MultiMonitorTool.exe /disable 1
MultiMonitorTool.exe /disable \\.\DISPLAY3
MultiMonitorTool.exe /disable 1 2 3

/enable <Monitors>
Enables the specified monitors.
Examples:
MultiMonitorTool.exe /enable 3
MultiMonitorTool.exe /enable \\.\DISPLAY1
MultiMonitorTool.exe /enable 3 2

/switch <Monitors>
Switches the specified monitors between enabled and disabled state.
Examples:
MultiMonitorTool.exe /switch 3
MultiMonitorTool.exe /switch \\.\DISPLAY1
MultiMonitorTool.exe /switch \\.\DISPLAY2 \\.\DISPLAY3

/setmax <Monitors>
Set the maximum resolution on the specified monitors.
Examples:
MultiMonitorTool.exe /setmax 3
MultiMonitorTool.exe /setmax \\.\DISPLAY2 \\.\DISPLAY3

/SetPrimary <Monitor>
Set the primary monitor.
Examples:
MultiMonitorTool.exe /SetPrimary 2
MultiMonitorTool.exe /SetPrimary \\.\DISPLAY2

/SetNextPrimary
Set the next available monitor as the primary monitor. For example, if
you have a system with 3 monitors and the first monitor is the primary,
running this command will set the second monitor as the primary. running
this command again will set the third monitor as the primary. And...
running this command again will set (again) the first monitor as the
primary.

/SaveConfig <Filename>
Saves the current configuration of all your monitors into the specified
filename, including the screen resolution, colors depth, and monitor
position.
You can load the saved configuration back into the system by using the
/LoadConfig command-line option.
Example:
MultiMonitorTool.exe /SaveConfig "c:\temp\monitors1.cfg"

/LoadConfig <Filename>
Loads the monitors configuration that you previouly saved with /SaveConfig
Example:
MultiMonitorTool.exe /LoadConfig "c:\temp\monitors1.cfg"


/MoveWindow <To Monitor> Process <Process Name>
/MoveWindow <To Monitor> Title <Title Text>
/MoveWindow <To Monitor> Class <Window Class>
/MoveWindow <To Monitor> All <From Monitor>
This command-line allows you to move windows from one monitor to the
other, according to the specified conditions. You can also use this
command-line option with /WindowLeft, /WindowTop, /WindowWidth, and
/WindowHeight, in order to set the width/size of the moved window. (See
examples below)
Here's some example:
* Move all windows located on all monitors to the primary monitor: (The
  <From Monitor> parameter is omitted, so windows are moved from all
  monitors)
  MultiMonitorTool.exe /MoveWindow Primary All
* Move all windows of Internet Explorer to the primary monitor:
  MultiMonitorTool.exe /MoveWindow Primary Process "iexplore.exe"
* Move all windows of Firefox to the next monitor:
  MultiMonitorTool.exe /MoveWindow Next Process "firefox.exe"
* Move all windows that their title contains the 'Hello World' string
  to '\\.\DISPLAY2' monitor:
  MultiMonitorTool.exe /MoveWindow 2 Title "Hello World"
* Move all windows of Windows Explorer (with 'CabinetWClass' window
  class) to '\\.\DISPLAY3' monitor:
  MultiMonitorTool.exe /MoveWindow 3 Class "CabinetWClass"
* Move all windows from '\\.\DISPLAY3' monitor to '\\.\DISPLAY2'
  monitor:
  MultiMonitorTool.exe /MoveWindow 2 All 3
* Move all windows of Firefox to the next monitor, and set their
  position to (10, 10), relatively to the target monitor.
  MultiMonitorTool.exe /MoveWindow Next Process "firefox.exe" /WindowLeft
  10 /WindowTop 10
* Move all windows of Firefox to the next monitor, and set their width
  to (600, 400).
  MultiMonitorTool.exe /MoveWindow Next Process "firefox.exe"
  /WindowWidth 600 /WindowHeight 400


/MonitorPreviewWindow <0 | 1>
Specifies whether to display the monitor preview window. 0 = No, 1 = Yes.

/PreviewMode <1 | 2>
Specifies the preview mode. 1 = Fast and Low Quality, 2 = Slow and High
Quality.

/DrawCursorPreview <0 | 1>
Specifies whether to draw the cursor inside the preview window. 0 = No, 1
= Yes.

/HideInactiveMonitors <0 | 1>
Specifies whether to hide inactive monitors. 0 = No, 1 = Yes.

/TrayIcon <0 | 1>
Specifies whether to put an icon on the system tray. 0 = No, 1 = Yes.

/StartAsHidden <0 | 1>
Specifies whether to start the main window as hidden. 0 = No, 1 = Yes.
Ths command-line option works only when TrayIcon option is turned on.

/AutoRefresh <0 | 1>
Specifies whether to automatically refresh the monitors/windows
information. 0 = No, 1 = Yes.



Have fun with the preview window
================================

If you activate the monitor preview window and extend its size to a
little less than the size of the entire monitor, the preview window will
draw itself again and again and again... and the final result will be a
window inside window inside window, like you can see in the following
screenshot:




Translating MultiMonitorTool to other languages
===============================================

In order to translate MultiMonitorTool to other language, follow the
instructions below:
1. Run MultiMonitorTool with /savelangfile parameter:
   MultiMonitorTool.exe /savelangfile
   A file named MultiMonitorTool_lng.ini will be created in the folder of
   MultiMonitorTool utility.
2. Open the created language file in Notepad or in any other text
   editor.
3. Translate all string entries to the desired language. Optionally,
   you can also add your name and/or a link to your Web site.
   (TranslatorName and TranslatorURL values) If you add this information,
   it'll be used in the 'About' window.
4. After you finish the translation, Run MultiMonitorTool, and all
   translated strings will be loaded from the language file.
   If you want to run MultiMonitorTool without the translation, simply
   rename the language file, or move it to another folder.



License
=======

This utility is released as freeware. You are allowed to freely
distribute this utility via floppy disk, CD-ROM, Internet, or in any
other way, as long as you don't charge anything for this and you don't
sell it or distribute it as a part of commercial product. If you
distribute this utility, you must include all files in the distribution
package, without any modification !



Disclaimer
==========

The software is provided "AS IS" without any warranty, either expressed
or implied, including, but not limited to, the implied warranties of
merchantability and fitness for a particular purpose. The author will not
be liable for any special, incidental, consequential or indirect damages
due to loss of data or any other reason.



Feedback
========

If you have any problem, suggestion, comment, or you found a bug in my
utility, you can send a message to nirsofer@yahoo.com
