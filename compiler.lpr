(*
 SScript Compiler
 Copyright © by Patryk Wychowaniec, 2013

 -------------------------------------------------------------------------------
 SScript Compiler is free software; you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation; either version 2.1 of the License, or
 (at your option) any later version.

 SScript Compiler is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 GNU Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with SScript Compiler; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)
Program compiler;
Uses Windows, SysUtils, CompilerUnit, CTypes, Scanner, Compile1, ExpressionCompiler;
Var Input, Output: String;

    Options: TCompileOptions;

    I, Time: Cardinal;

    Frame : Integer;
    Frames: PPointer;
    Quiet : Boolean;

Begin
 DefaultFormatSettings.DecimalSeparator := '.';

 Time := GetTickCount;

 Options := [];

 { parse command line }
 For I := Low(OptionNames) To High(OptionNames) Do
  if (getBoolOption(OptionNames[I], False)) Then
   Include(Options, TCompileOption(I));

 Try
  if (ParamCount < 1) Then // too few parameters specified
  Begin
   Writeln('Usage:');
   Writeln('compiler.exe [input file] <options>');
   Writeln;
   Writeln('Available options:');
   Writeln('To enable a boolean switch use `name+` (or just `name`), to disable `name-`');
   Writeln;

   Writeln('name = description');
   Writeln;

   Writeln('-> Compiler');
   Writeln('-s <file>     save output verbal bytecode');
   Writeln('-o <file>     change output file name');
   Writeln('-ninit        do not include `init.sm` file into the program');
   Writeln('-includepath  include path for module including (see documentation for description)');

   Writeln;
   Writeln('-> Optimizations');
   Writeln('-Or      allocate variables in registers');
   Writeln('-Of      enable constant expression folding');
   Writeln('-Op      peephole bytecode optimizer');
   Writeln('-O1      optimization level 1: enables `-Or` `-Of` `-Op`');
   Writeln('-iconst  inline constants directly when building an expression (may affect to displayed errors)');

   Writeln;
   Writeln('-> Hints & warnings');

   Writeln;
   Writeln('-> Output file');
   Writeln('-dbg  generate debug data (applications only)'); // @TODO: debug data in libraries?

   Writeln;
   Writeln('-> Compile modes');
   Writeln('-Clib    compile file as a library');
   Writeln('-Cbcode  compile file as a bytecode');

   Writeln;
   Writeln('-> For libraries');
   Writeln('-h <file> generate header file for input source file');

   Writeln;
   Writeln('-> Other options');
   Writeln('-includepath  include path for modules');
   Writeln('-stacksize    change stack size (default: ', DEF_STACKSIZE, ')');
   Writeln('-wait         wait for `enter` when finished (-)');
   Writeln('-logo         if you set this as an input file name, the compiler will display only its version, but won''t compile anything');
   Writeln('-quiet        when enabled, the compiler displays only important messages (eg.code errors/warnings/hints)');
  End Else
  Begin
   Input  := ExpandFileName(ParamStr(1));
   Output := ExpandFileName(getStringOption('o', 'output.ssc'));
   Quiet  := getBoolOption('quiet', False);

   if (_O1 in Options) Then
   Begin
    Include(Options, _Or);
    Include(Options, _Of);
    Include(Options, _Op);
   End;

   if (ParamStr(1) = '-logo') Then
   Begin
    // @TODO: make a pretty, cool logo; like that FPC one ;)
    Writeln('SScript Compiler '+Version);
    Writeln('by Patryk Wychowaniec');
    raise Exception.Create('');
   End;

   if (not Quiet) Then
   Begin
    Writeln('SScript Compiler '+Version);
    Writeln('by Patryk Wychowaniec');
    Writeln;
    Writeln('Input file  : ', Input);
    Writeln('Output file : ', Output);
    Writeln('Command line: ', GetCommandLine);
    Writeln;
   End;

   if (_Clib in Options) Then // `init` code would be unusable in a library
    Include(Options, _NINIT);

   if (not FileExists(Input)) Then
    raise Exception.Create('Input file does not exist.');

   if (Input = Output) Then
    raise Exception.Create('Input file is output file.');

   if (not Quiet) Then
   Begin
    Writeln;
    Writeln('-- starting --');
    Writeln;
   End;

   CompileCode(Input, Output, Options);
  End;
 Except
  On E: Exception Do
   if (E.Message <> '') Then
   Begin
    Writeln('Exception raised:');
    Writeln(E.Message);
    Writeln;
    Writeln('Callstack:');
    Writeln(BackTraceStrFunc(ExceptAddr));
    Frames := ExceptFrames;
    For Frame := 0 To ExceptFrameCount-1 Do
     Writeln(BackTraceStrFunc(Frames[Frame]));
   End;
 End;

 Time := GetTickCount-Time; // total compilation time

 if (not Quiet) Then
 Begin
  Writeln;
  Writeln('-- done in ', Time, ' ms --');
 End;

 if (getBoolOption('wait', False)) Then
  Readln;
End.
