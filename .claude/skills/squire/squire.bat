@echo off
setlocal
set "SQUIRE_SKILL_DIR=%~dp0"
for /f "delims=" %%I in ('where jsonschema 2^>nul') do if not defined SQUIRE_JSONSCHEMA_BIN set "SQUIRE_JSONSCHEMA_BIN=%%I"
if not defined TEMP set "TEMP=%SQUIRE_SKILL_DIR%"

:make_exit_file
set "SQUIRE_EXIT_FILE=%TEMP%\squire-exit-%RANDOM%-%RANDOM%-%RANDOM%.tmp"
if exist "%SQUIRE_EXIT_FILE%" goto make_exit_file
type nul > "%SQUIRE_EXIT_FILE%"
if not exist "%SQUIRE_EXIT_FILE%" (
  >&2 echo ERROR [launcher]: could not create Squire exit-code handoff
  exit /b 1
)

pushd "%SQUIRE_SKILL_DIR%"
call "%SQUIRE_SKILL_DIR%mill.bat" --no-server --ticker false squire.scala %*
set "SQUIRE_MILL_EXIT=%ERRORLEVEL%"
popd

set "SQUIRE_RECORDED_EXIT="
%SystemRoot%\System32\findstr.exe /r /x "[0-9][0-9]*" "%SQUIRE_EXIT_FILE%" >nul 2>&1
if not errorlevel 1 set /p "SQUIRE_RECORDED_EXIT="<"%SQUIRE_EXIT_FILE%"
del /q "%SQUIRE_EXIT_FILE%" >nul 2>&1
set "SQUIRE_EXIT_FILE="

if not defined SQUIRE_RECORDED_EXIT goto no_recorded_exit
set /a SQUIRE_RECORDED_EXIT=SQUIRE_RECORDED_EXIT 2>nul
if errorlevel 1 goto no_recorded_exit
if %SQUIRE_RECORDED_EXIT% GTR 255 goto no_recorded_exit
exit /b %SQUIRE_RECORDED_EXIT%

:no_recorded_exit
if not "%SQUIRE_MILL_EXIT%"=="0" exit /b %SQUIRE_MILL_EXIT%
>&2 echo ERROR [launcher]: Squire completed without a valid exit-code handoff
exit /b 1
