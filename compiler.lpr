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
Uses Windows, SysUtils, TypInfo, CompilerUnit, CTypes, Scanner, Compile1, ExpressionCompiler;
Var Input, Output: String;

    Options   : TCompileOptions;
    _logo_only: Boolean=False;
    _wait     : Boolean=False;
    _time     : Boolean=False;

    Frame : Integer;
    Frames: PPointer;

    Time: Cardinal;

{ AddOption }
Procedure AddOption(const Option: TCommandLineOption; Value: Variant);
Begin
 SetLength(Options, Length(Options)+1);
 Options[High(Options)].Option := Option;
 Options[High(Options)].Value  := Value;
End;

{ ParseCommandLine }
Procedure ParseCommandLine;
Var Current        : String;
    Option         : TCommandLineOption;
    Pos, Tmp, OptID: Integer;
    Value          : Variant;
Begin
 if (Copy(ParamStr(1), 1, 1) = '-') Then
  Pos := 1 Else
  Pos := 2;

 SetLength(Options, 0);

 Repeat
  Current := ParamStr(Pos);

  if (Length(Current) = 0) Then // shouldn't happen (?)
  Begin
   Inc(Pos);
   Continue;
  End;

  { -O1 (optimize level 1) }
  if (Current = '-O1') Then
  Begin
   AddOption(opt__register_alloc, True);
   AddOption(opt__constant_folding, True);
   AddOption(opt__bytecode_optimize, True);
  End Else

  { -logo }
  if (Current = '-logo') Then
  Begin
   _logo_only := True;
  End Else

  { -time }
  if (Current = '-time') Then
  Begin
   _time := True;
  End Else

  { -wait }
  if (Current = '-wait') Then
  Begin
   _wait := True;
  End Else

  { -verbose / -v }
  if (Current = '-verbose') or (Current = '-v') Then
  Begin
   verbose_mode := True;
  End Else

  { another option }
  if (Current[1] = '-') Then
  Begin
   Tmp   := Length(Current);
   Value := not (Current[Tmp] = '-');

   if (System.Pos('=', Current) > 0) Then
   Begin
    Value := Copy(Current, System.Pos('=', Current)+1, Length(Current));
    Delete(Current, System.Pos('=', Current), Length(Current));
   End;

   // find this option
   OptID := -1;
   For Option := Low(CommandLineNames) To High(CommandLineNames) Do
    if (CommandLineNames[Option].Names[0] = Current) or (CommandLineNames[Option].Names[1] = Current) Then
     OptID := ord(Option);
   Option := TCommandLineOption(OptID);

   if (ord(Option) = -1) Then
    Writeln('Unknown command-line option: ', Current) Else
    AddOption(Option, Value);
  End Else

  { unexpected }
   Writeln('Unexpected command-line argument: ', Current);

  Inc(Pos);
 Until (Pos > ParamCount);
End;

{ program's body }
Begin
 Time := GetTickCount;

 DefaultFormatSettings.DecimalSeparator := '.';

 ParseCommandLine;

 Try
  if (ParamCount < 1) Then // too few parameters specified
  Begin
   Writeln('Usage:');
   Writeln('compiler.exe [input file] <options>');
   Writeln;
   Writeln('See `command-line.txt` for more information.');
  End Else
  Begin
   Input  := ExpandFileName(ParamStr(1));
   Output := ExpandFileName(getStringOption('o', 'output.ssc'));

   if (_logo_only) Then
    verbose_mode := True;

   Log('SScript Compiler '+Version+' [compiled '+{$I %DATE%}+']');
   Log('by Patryk Wychowaniec');

   {$IFDEF NIGHTLY}
    Log;
    Log('Warning: This is a nightly, untested and most likely unstable version - not intended for daily use!');
   {$ENDIF}

   if (_logo_only) Then
    raise Exception.Create('');

   Log; // newline
   Log('Input: '+Input);
   Log('Output: '+Output);
   Log;

   if (not FileExists(Input)) Then
    raise Exception.Create('Input file does not exist.'); // error: input file not found

   if (Input = Output) Then
    raise Exception.Create('Input file is the same as output file.'); // error: input file is the same as the output's

   CompileCode(Input, Output, Options);
  End;
 Except
  On E: Exception Do
   if (E.Message <> '') Then
   Begin
    Writeln;
    Writeln(E.Message);
    Writeln;
    Writeln('Callstack:');
    Writeln(BackTraceStrFunc(ExceptAddr));
    Frames := ExceptFrames;
    For Frame := 0 To ExceptFrameCount-1 Do
     Writeln(BackTraceStrFunc(Frames[Frame]));

    Writeln;
    if (getCompiler <> nil) Then
    Begin
     With TCompiler(getCompiler) do
     Begin
      Writeln('Additional compilation info:');

      Writeln;

      Writeln('Last namespace: ');
      if (getCurrentNamespace.Name <> 'self') Then
       Writeln('  -> ', getCurrentNamespace.Name) Else
       Writeln('  -> (default namespace: `self`)');

      Writeln;

      if (getCurrentFunctionPnt = nil) Then
       Writeln('Last function: <none>') Else
      Begin
       Writeln('Last function:');
       With getCurrentFunction do
       Begin
        Writeln('  -> ', Name);
        Writeln('  -> declared at line ', DeclToken.Line+1);
       End;
      End;
     End;
    End Else
     Writeln('No more info available.');
   End;
 End;

 { -time }
 if (_time) Then
 Begin
  Time := GetTickCount-Time;
  Writeln('Total time: ', Time, ' ms');
 End;

 { -wait }
 if (_wait) Then
 Begin
  Writeln('-- done --');
  Readln;
 End;
End.
