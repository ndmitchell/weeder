@echo off
setlocal
cd %~dp0\..

for /r str %%i in (*.hs) do (
    mkdir dist\str\%%~ni
    ghc --make src/Paths.hs %%i Main -isrc -outputdir dist\str\%%~ni -o dist\str\%%~ni\weeder || goto die
    dist\str\%%~ni\weeder --test || goto die
)
for /r str %%i in (*.hs) do (
    echo %%~ni
    for /L %%j in (1,1,5) do (
        ptime dist\str\%%~ni\weeder %* | grep Execution
    )
)

exit /b 0

:die
exit /b 1














