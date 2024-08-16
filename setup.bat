@echo off
setlocal

REM Verzeichnis, in dem sich das Python-Skript befindet
set "INSTALL_DIR=C:\Users\felix\Documents\GitHub\SQUIRK\src"
set "SCRIPT_NAME=squirk.py"

REM Verzeichnis für die Batch-Datei erstellen (inklusive automatischer Verzeichnis-Erstellung)
set "BATCH_DIR=%ProgramFiles%\SQUIRK\bin"

REM Überprüfen, ob das Verzeichnis bereits existiert
if not exist "%BATCH_DIR%" (
    echo Creating directory "%BATCH_DIR%"...
    mkdir "%BATCH_DIR%"
)

REM Batch-Datei erstellen
echo Creating batch file "squirk.bat"...
(
    echo @echo off
    echo python "%INSTALL_DIR%\%SCRIPT_NAME%" %%*
) > "%BATCH_DIR%\squirk.bat"

REM Überprüfen, ob das Verzeichnis bereits im PATH ist
echo Checking if "%BATCH_DIR%" is in PATH...
echo %PATH% | findstr /i /c:"%BATCH_DIR%" >nul
if %ERRORLEVEL% == 0 (
    echo The directory is already in PATH.
    goto :update_complete
)

REM Verzeichnis zum PATH hinzufügen
echo Adding "%BATCH_DIR%" to PATH...
setx PATH "%PATH%;%BATCH_DIR%"

echo The PATH has been updated. You may need to restart your command prompt for the changes to take effect.

:update_complete
echo Installation complete.
pause
