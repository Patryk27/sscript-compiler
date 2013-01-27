(*
 SScript Compiler 2.0b

 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 @TODO:
 1) one, global type table (created in the parent compiler)
 2) remove unused functions from the bytecode
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

 For I := Low(OptionNames) To High(OptionNames) Do
  if (getBoolOption(OptionNames[I], OptionDefault[I])) Then
   Include(Options, TCompileOption(I));

 Try
  if (ParamCount < 1) Then
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
   Writeln('-s            save output verbal bytecode; `-s file_name`');
   Writeln('-o            change output file name; `-o file_name`');
   Writeln('-ninit        do not include `init.sm` file into the program');
   Writeln('-includepath  include path for module including (see documentation)');

   Writeln;
   Writeln('-> Optimizations');
   Writeln('-Or  allocate variables in registers');
   Writeln('-Of  enable constant expression folding');
   Writeln('-Op  peephole bytecode optimizer');
   Writeln('-O1  optimization level 1: enables `-Or` `-Of` `-Op`');

   Writeln;
   Writeln('-> Hints & warnings');

   Writeln;
   Writeln('-> Output file');
   Writeln('-dbg  generate debug data'); // @TODO: debug data in modules?

   Writeln('-> For modules');
   Writeln('-module     compile file as module');
   Writeln('-h <output> generate header file for input source file');

   Writeln;
   Writeln('-> Other options');
   Writeln('-bytecode     compile file as a bytecode');
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

   if (_MODULE in Options) Then
   Begin
    Include(Options, _NINIT);
    if (not Quiet) Then
     Writeln('Compiling file as a module; `-ninit` added automatically.');
   End;

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

    {if (not getBoolOption('nodump', False)) and (getCompiler <> nil) Then
    Begin
     Writeln;
     Writeln('Compiler data dump (if it''ll crash now, run compiler again with option `-nodump`):');
     With TCompiler(getCompiler) do
     Begin
      Writeln('Function amount: ', Length(FunctionList));
      if (Length(FunctionList) > 0) Then
       Writeln('Last function name: ', FunctionList[High(FunctionList)].Name);
     End;
    End;}

    Writeln;
    Writeln('Callstack:');
    Writeln(BackTraceStrFunc(ExceptAddr));
    Frames := ExceptFrames;
    For Frame := 0 To ExceptFrameCount-1 Do
     Writeln(BackTraceStrFunc(Frames[Frame]));
   End;
 End;

 Time := GetTickCount-Time;

 if (not Quiet) Then
 Begin
  Writeln;
  Writeln('-- done in ', Time, ' ms --');
 End;

 if (getBoolOption('wait', False)) Then
  Readln;
End.
