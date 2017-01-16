rem This batch file sets two environment variables
rem You can add the lines to your autoexec.bat file

rem This sets your graphics mode. You can try gw 800 gh 600, or more.
rem Use nc 16 to capture screens under Windows.

set grx20drv=vesa gw 640 gh 480 nc 256

rem This sets where is the LITT.CHR file
set tcbgi=.
