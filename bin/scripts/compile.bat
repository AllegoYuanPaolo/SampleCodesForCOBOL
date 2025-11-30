@echo off

setlocal

set "file=src\%~n1.cbl"
set "exe=%~1.exe"

echo    ^> Compiling %file%
cobc -x %file% -o bin\programs\%exe%
    if errorlevel 1 (
        echo    ^> ERROR Compiling %file%
        exit /b
    ) else (
        echo    ^> Compiled %exe%
    )

