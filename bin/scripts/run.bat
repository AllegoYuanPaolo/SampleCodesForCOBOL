@echo off

setlocal

set "file=%~1.exe"

pushd bin\programs
    start "" cmd /c  "%file% & pause"
popd