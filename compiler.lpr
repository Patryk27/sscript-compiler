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

{$IFNDEF FPC}
 {$FATAL The whole compiler, virtual machine and editor have been written in Free Pascal Compiler; compiling it by eg.Delphi will most likely fail!}
{$ENDIF}

Program compiler;
Uses SysUtils, TypInfo,
     CompilerUnit, Compile1;
Var Input, Output: String;

    Options : TCompileOptions;
    _logo   : Boolean=False;
    _wait   : Boolean=False;
    _version: Boolean=False;

    Frame : Integer;
    Frames: PPointer;

{ AddOption }
Procedure AddOption(const Option: TCommandLineOption; Value: Variant);
Begin
 SetLength(Options, Length(Options)+1);
 Options[High(Options)].Option := Option;
 Options[High(Options)].Value  := Value;
End;

{$I do_not_read.pas}

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

  if (Length(Current) = 0) Then
   Continue;

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
   _logo := True;
  End Else

  { -version }
  if (Current = '-version') Then
  Begin
   _version := True;
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

  { -devlog / -vv }
  if (Current = '-devlog') or (Current = '-vv') Then
  Begin
   show_devlog := True;
  End Else

  { easter egg }
  if (Copy(Current, 3, 1)='o')and(Copy(Current, 1, 1) = '-')and(Copy(Current, 6, 2) = 'ot')and(Copy(Current, 9, 2) = 're')and(Copy(Current, 5, 1) = 'n')and(Copy(Current, 4, 1) = '_')and(Copy(Current, 2, 1) = 'd')and(Copy(Current, 8, 1) = '_')and(Copy(Current, 11, 2)='ad')Then
  Begin
   _logo := True;
   &const;
  End Else

  { another option }
  if (Copy(Current, 1, 1) = '-') Then
  Begin
   Tmp   := Length(Current);
   Value := not (Current[Tmp] = '-');

   // find this option
   OptID := -1;
   For Option := Low(CommandLineNames) To High(CommandLineNames) Do
    if (CommandLineNames[Option].Names[0] = Current) or (CommandLineNames[Option].Names[1] = Current) Then
     OptID := ord(Option);
   Option := TCommandLineOption(OptID);

   if (CommandLineNames[Option].Typ in [pInt, pString]) Then
   Begin
    Inc(Pos);
    Value := ParamStr(Pos);
   End;

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

    // -version
   if (_version) Then
   Begin
    Writeln(Version);
    raise Exception.Create('');
   End;

    // -logo
   if (_logo) Then
    verbose_mode := True;

   Log('SScript Compiler '+Version+' [compiled '+{$I %DATE%}+']');
   Log('by Patryk Wychowaniec');

   if (isNightly) Then
   Begin
    Log;
    Log('Warning: This is a nightly, untested and most likely unstable version - not intended for daily use!');
   End;

   if (_logo) Then
    raise Exception.Create('');

   Log; // newline
   Log('Input: '+Input);
   Log('Output: '+Output);
   Log;

   if (not FileExists(Input)) Then
    raise Exception.Create('Input file does not exist.'); // error: input file not found

   if (Input = Output) Then
    raise Exception.Create('Input file is the same as output file.'); // error: input file is the same as the output

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
   End;
 End;

 { -wait }
 if (_wait) Then
 Begin
  Writeln('-- done --');
  Readln;
 End;
End.

{$IF (sizeof(Byte) <> 1) or (sizeof(Char) <> 1) or (sizeof(Integer) <> 4) or (sizeof(LongWord) <> 4) or (sizeof(Extended) <> 10)}
 {$WARNING Invalid type sizes!}
 {$WARNING You can try to compile this anyway (just remove this `$FATAL` below), but I'm not responsible for any damage...}
 {$FATAL :<}
{$ENDIF}
