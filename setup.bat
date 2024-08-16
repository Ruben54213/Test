@echo off
setlocal

net session >nul 2>&1
if '%errorlevel%' neq '0' (
    echo Requesting elevation...
    powershell -Command "Start-Process '%~f0' -Verb RunAs"
    exit /b
)

set "INSTALL_DIR=C:\Users\felix\Documents\GitHub\SQUIRK\src"
set "SCRIPT_NAME=squirk.py"

set "BATCH_DIR=%ProgramFiles%\SQUIRK\bin"

if not exist "%BATCH_DIR%" (
    echo Creating directory "%BATCH_DIR%"...
    mkdir "%BATCH_DIR%"
)

echo Creating batch file "squirk.bat"...
(
    echo @echo off
    echo python "%INSTALL_DIR%\%SCRIPT_NAME%" %%*
) > "%BATCH_DIR%\squirk.bat"

echo Checking if "%BATCH_DIR%" is in PATH...
echo %PATH% | findstr /i /c:"%BATCH_DIR%" >nul
if %ERRORLEVEL% == 0 (
    echo The directory is already in PATH.
    goto :update_complete
)

echo Adding "%BATCH_DIR%" to PATH...
set "OLD_PATH=%PATH%"
set "NEW_PATH=%OLD_PATH%;%BATCH_DIR%"
setx PATH "%NEW_PATH%"

echo The PATH has been updated. You may need to restart your command prompt for the changes to take effect.

:update_complete
echo Installation complete.
pause
