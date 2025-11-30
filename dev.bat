:: Temporarily gives access to  all scripts across the local dir
:: This can be ported to other workspaces that has .bat files stored in \bin\scripts
:: only place in ROOT folder
@echo off

:: Set bin\scripts as PATH
cd /d "%~dp0"
set "ScriptBIN=%~dp0bin\scripts"

set PATH=%ScriptBIN%;%PATH%
echo(
echo    Workspace PATH updated! You can now run:
    for %%F in ("%ScriptBIN%\*.bat") do (
        echo ^> %%~nF
        echo    ^> %%~fF
    )
call DebugPath.bat




