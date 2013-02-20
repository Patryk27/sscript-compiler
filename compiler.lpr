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

{$DEFINE NIGHTLY}

Program compiler;
Uses Windows, SysUtils, TypInfo, CompilerUnit, CTypes, Scanner, Compile1, ExpressionCompiler;
Var Input, Output: String;

    Options  : TCompileOptions;
    logo_only: Boolean=False;

    Frame : Integer;
    Frames: PPointer;

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
   logo_only := True;
   Exit;
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

   // find this option
   OptID := -1;
   For Option := Low(CommandLineNames) To High(CommandLineNames) Do
    if (CommandLineNames[Option].Names[0] = Current) or (CommandLineNames[Option].Names[1] = Current) Then
     OptID := ord(Option);
   Option := TCommandLineOption(OptID);

   if (ord(Option) = -1) Then
    Writeln('Unknown command-line option: ', Current) Else
    Begin
     if (CommandLineNames[Option].Typ <> pBool) Then
     Begin
      Inc(Pos);
      Value := ParamStr(Pos);
     End;

     AddOption(Option, Value);
    End;
  End Else

  { unexpected }
   Writeln('Unexpected command-line argument: ', Current);

  Inc(Pos);
 Until (Pos > ParamCount);
End;

{ program's body }
Begin
 DefaultFormatSettings.DecimalSeparator := '.';

 ParseCommandLine;

 Try
  if (ParamCount < 1) Then // too few parameters specified
  Begin
   Writeln('Usage:');
   Writeln('compiler.exe [input file] <options>');
   Writeln;
   Writeln('See `command-line.txt` for more informations.');
  End Else
  Begin
   Input  := ExpandFileName(ParamStr(1));
   Output := ExpandFileName(getStringOption('o', 'output.ssc'));

   if (logo_only) Then
    verbose_mode := True;

   Log('SScript Compiler, version '+Version+' ['+{$I %DATE%}+']');
   Log('by Patryk Wychowaniec');

   {$IFDEF NIGHTLY}
    Log;
    Log('Warning: This is a nightly, untested and most likely unstable version - not intended for daily use!');
   {$ENDIF}

   if (logo_only) Then
    raise Exception.Create('');

   Log; // newline

   if (not FileExists(Input)) Then
    raise Exception.Create('Input file does not exist.'); // error: input file not found

   if (Input = Output) Then
    raise Exception.Create('Input file is the same as output file.'); // error: input is output

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

 { -wait }
 if (getBoolOption('wait', False)) Then // @TODO
 Begin
  Writeln('-- done --');
  Readln;
 End;
End.
