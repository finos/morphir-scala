@echo off
setlocal
set "SQUIRE_SKILL_DIR=%~dp0"
pushd "%SQUIRE_SKILL_DIR%"
call "%SQUIRE_SKILL_DIR%mill.bat" --no-server --ticker false squire.scala %*
set "SQUIRE_EXIT=%ERRORLEVEL%"
popd
exit /b %SQUIRE_EXIT%
