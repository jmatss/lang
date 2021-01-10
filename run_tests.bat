@echo off
setlocal EnableDelayedExpansion 

rem echo usage: %0 ^[^<RANGE_START^> ^[^<RANGE_END^>^]^]

if "%~1" == "" (
    set RANGE_START=1
) else (
    set RANGE_START=%1
)
if "%~2" == "" (
    if "%~1" == "" (
        set RANGE_END=42
    ) else (
        set RANGE_END=%1
    )
) else (
    set RANGE_END=%2
)

set TEST_FAILURE=

for /l %%x in (%RANGE_START%, 1, %RANGE_END%) do (
    echo | set /p="Running test %%x..."
    %~dp0\target\debug\lang.exe -i test_data\test%%x.ren -o a.o >nul 2>&1
    if errorlevel 1 (
        echo compiling failed.
        set TEST_FAILURE=!TEST_FAILURE! %%x
    ) else (
        echo | set /p=" compiled..."
        clang a.o -o a.exe && a.exe >nul 2>&1
        if errorlevel 1 (
            echo running failed.
            set TEST_FAILURE=!TEST_FAILURE! %%x
        ) else (
            echo passed.
        )
    )
)

if "%TEST_FAILURE%" == "" (
    echo. && echo All tests passed.
    exit /b 0
) else (
    echo. && echo Test failures: %TEST_FAILURE%
    exit /b 1
)
