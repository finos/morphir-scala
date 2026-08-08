@echo off
setlocal
set "SQUIRE_SKILL_DIR=%~dp0"
for /f "delims=" %%I in ('where jsonschema 2^>nul') do if not defined SQUIRE_JSONSCHEMA_BIN set "SQUIRE_JSONSCHEMA_BIN=%%I"
pushd "%SQUIRE_SKILL_DIR%"
call "%SQUIRE_SKILL_DIR%mill.bat" --no-server --ticker false squire.scala %*
set "SQUIRE_EXIT=%ERRORLEVEL%"
popd
exit /b %SQUIRE_EXIT%
