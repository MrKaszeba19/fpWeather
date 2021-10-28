@echo off
echo Starting...

set ver=3.0.4

echo Attempting to install from Lazarus FPC executable...
call C:\lazarus\fpc\%ver%\bin\x86_64-win64\fpc.exe main.pas -o"fpweather.exe"
if %ERRORLEVEL% == 0 goto :next

echo Failed. Attempting to install from a non-Lazarus FPC executable...
call C:\fpc\%ver%\bin\x86_64-win64\fpc.exe main.pas -o"fpweather.exe"
if %ERRORLEVEL% == 0 goto :next

echo Failed. Maybe there is FPC in Windows PATH...
fpc.exe main.pas -o"fpweather.exe"
if %ERRORLEVEL% == 0 goto :next
echo Failed. Could not find a FPC in the %ver% version.
goto :end

:next
    echo.
    echo.
    echo ================================================================ 
    echo Done.
    echo ================================================================

:end
    pause