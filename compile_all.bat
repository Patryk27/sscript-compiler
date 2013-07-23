@echo off

echo ---------- Compiler ----------
mkdir lib
ppc386 -B compiler.lpr -Fucompiler;compiler\syntax -Ficompiler;compiler\optimization;compiler\expression;compiler\expression\internal_functions;compiler\syntax;$(ProjOutDir) -FUlib\ -Mobjfpc -Sh -Sc -Sg -Si -g -gl -v0 -O1 -Or -Ou
echo.

echo ---------- Standard library ----------
cd stdlib_code
call compile_all.bat

echo.
echo ---------------------------------
echo - SScript has been compiled! :) -
echo - Have a good fun!              -
echo -                               -
echo - ~Patryk Wychowaniec           -
echo ---------------------------------