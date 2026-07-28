@echo off
REM kb - knowledge base management for the Morphir knowledge base under kb\.
REM
REM Thin wrapper over Mill's single-file scripting. Mill resolves dependencies and compiles kb.scala on first run;
REM subsequent runs are incremental. Progress output goes to stderr, so --json on stdout stays clean and pipeable.
setlocal
set "KB_SKILL_DIR=%~dp0"
pushd "%KB_SKILL_DIR%"
call "%KB_SKILL_DIR%mill.bat" --ticker false kb.scala %*
set "KB_EXIT=%ERRORLEVEL%"
popd
exit /b %KB_EXIT%
