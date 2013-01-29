rem SScript Standard Library build script

goto :begin

:loop
	goto :loop

:fail
	echo --] compilation failed; building stopped
	goto :loop

:compiler_not_found
	echo Compiler not found!
	echo It should be reachable at `..\compiler.exe`
	goto :loop

:compile
	set outputfile=..\stdlib\%~1.ssm

	echo --] '%~1.ss' :: '%outputfile%'

	if exist %outputfile% del /Q %outputfile% > nul
	..\compiler %~1.ss -o %outputfile% -h ..\stdlib\%~1.ss -Clib -quiet -O1
	if not exist %outputfile% goto :fail

	goto :eof

:compile_bytecode
	set outputfile=..\stdlib\%~1.ssm

	echo --] '%~1.ssb' :: '%outputfile%'

	if exist %outputfile% del /Q %outputfile% > nul
	..\compiler %~1.ssb -o %outputfile% -Cbcode -quiet
	if not exist %outputfile% goto :fail

	goto :eof

:begin
@echo off
cls

echo -------------------------------------------
echo - SScript Standard Library; version: 0.1a -
echo - for compiler 2.0a                       -
echo -------------------------------------------
echo.

if not exist "..\compiler.exe" goto :compiler_not_found

echo Compiler test:
..\compiler -logo -quiet

echo.
echo -] init code
call :compile_bytecode "init"

echo.
echo -] STRING
call :compile "string"

echo.
echo -] MATH
call :compile "math"
call :compile "numbers"

echo.
echo -] TIME
call :compile "time"

echo.
echo -] STDIO
call :compile "stdio"

echo.
echo -] other units
call :compile "short_cast"

echo.
echo --------------------------------------------------
echo - SScript Standard Library has been compiled! :) -
echo - Headers and compiled files are in `..\stdlib\` -
echo - Have a good fun with SScript!                  -
echo -                                                -
echo - ~Patryk Wychowaniec                            -
echo --------------------------------------------------

goto :loop