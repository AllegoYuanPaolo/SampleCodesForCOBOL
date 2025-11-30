@echo off

setlocal

set "file=src\%~n1.cbl"
set "module=%~1.dll"

echo    ^> Compiling %file%
cobc -m %file% -o bin\programs\%module%
    if errorlevel 1 (
        echo    ^> ERROR Compiling %file%
        exit /b
    ) else (
        echo    ^> Compiled %module%
    )

