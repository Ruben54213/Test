@echo off
setlocal

:: Check for administrative privileges
net session >nul 2>&1
if '%errorlevel%' neq '0' (
    echo Requesting elevation...
    powershell -Command "Start-Process '%~f0' -Verb RunAs"
    exit /b
)

:: Define the name of the script file
set "SCRIPT_NAME=squirk.py"

:: Initialize variables
set "INSTALL_DIR="

:: Find the directory containing the script
for /r "%~dp0" %%F in (%SCRIPT_NAME%) do (
    if exist "%%F" (
        set "INSTALL_DIR=%%~dpF"
    )
)

:: Check if INSTALL_DIR is set
if "%INSTALL_DIR%"=="" (
    echo Error: %SCRIPT_NAME% not found in any subdirectories.
    exit /b 1
)

:: Confirm the exact location of the script
for /d %%D in ("%INSTALL_DIR%*") do (
    if exist "%%D\%SCRIPT_NAME%" (
        set "INSTALL_DIR=%%D"
        goto :found
    )
)

:: If exact directory was not found, use the previously found directory
:found
echo Found %SCRIPT_NAME% in directory %INSTALL_DIR%

set "BATCH_DIR=C:\SQUIRK\bin"

:: Create batch directory if it does not exist
if not exist "%BATCH_DIR%" (
    echo Creating directory "%BATCH_DIR%"...
    mkdir "%BATCH_DIR%"
)

:: Create the batch file
echo Creating batch file "squirk.bat"...
(
    echo @echo off
    echo python "%INSTALL_DIR%\%SCRIPT_NAME%" %%*
) > "%BATCH_DIR%\squirk.bat"

:: Associate file extension
set "EXT=.sqrk"
set "DESCR=SQUIRK File"

assoc %EXT%=SQUIRKFile
ftype SQUIRKFile=notepad.exe "%%1"

echo Dateityp '%EXT%' was connected with notepad.exe.

:: Check if the batch directory is already in PATH
echo Checking if "%BATCH_DIR%" is in PATH...
echo %PATH% | findstr /i /c:"%BATCH_DIR%" >nul
if %ERRORLEVEL% == 0 (
    echo The directory is already in PATH.
    goto :update_complete
)

:: Add batch directory to PATH
echo Adding "%BATCH_DIR%" to PATH...
set "OLD_PATH=%PATH%"
set "NEW_PATH=%OLD_PATH%;%BATCH_DIR%"
setx PATH "%NEW_PATH%"

echo The PATH has been updated. You may need to restart your command prompt for the changes to take effect.
echo Please restart your computer, if you want changes to take place correctly.

:update_complete
echo Installation complete.
pause
