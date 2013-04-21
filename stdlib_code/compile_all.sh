#!/bin/bash

# SScript Standard Library build script
# by Patryk Wychowaniec

clear

fail()
{
	echo "--] compilation failed; building stopped"
	exit
}

compiler_not_found()
{
	echo "Compiler not found!"
	echo "It should be reachable at ../compiler.o"
	exit
}

compile()
{
	outputfile="../stdlib/$1.ssm"

	echo --] $1'.ss' :: $outputfile

	if [ -f $outputfile ]
	then
		rm $outputfile
	fi

	../compiler.o $1.ss -o $outputfile -h ../stdlib/$1.ss -Cm lib -O1

	if [ ! -f $outputfile ]
	then
		fail
	fi
}

echo "-------------------------------------------"
echo "- SScript Standard Library; version: 0.1c -"
echo "-------------------------------------------"
echo

if [ ! -f "../compiler.o" ]
then
	compiler_not_found
fi

echo "Compiler test:"
echo "-------------------------------------------"
../compiler.o -logo
echo "-------------------------------------------"

echo
echo "-] init code"
compile "init"

echo
echo "-] STRING"
compile "string"

echo
echo "-] MATH"
compile "math"
compile "numbers"
compile "float"

echo 
echo "-] TIME"
compile "time"

echo 
echo "-] STDIO"
compile "stdio"

echo 
echo "-] VM"
compile "vm"

echo 
echo "--------------------------------------------------"
echo "- SScript Standard Library has been compiled! :) -"
echo "- Headers and compiled files are in '../stdlib/' -"
echo "- Have a good fun with SScript!                  -"
echo "-                                                -"
echo "- ~Patryk Wychowaniec                            -"
echo "--------------------------------------------------"
