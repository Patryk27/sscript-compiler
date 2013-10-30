(*
 SScript Compiler
 Copyright © by Patryk Wychowaniec, 2012-2013

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
 {$FATAL The compiler has been written in Free Pascal Compiler; compiling it in any other compiler will gracefully fail.}
{$ENDIF}

{$IFDEF CPU64}
 {$FATAL 64-bit CPUs are not supported (yet)!} // (because of floating point operations)
{$ENDIF}

Program compiler;
Uses SysUtils, TypInfo,
     ExpressionCompiler, CompilerUnit, SSCompiler;
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

{ _O1 }
Procedure _O1;
Begin
 AddOption(opt__register_alloc, True);
 AddOption(opt__constant_folding, True);
 AddOption(opt__tree_simplify, True);
 AddOption(opt__bytecode_optimize, True);
End;

{ _O2 }
Procedure _O2;
Begin
 _O1;

 AddOption(opt__constant_propagation, True);
 AddOption(opt__remove_dead, True);
End;

{ _O3 }
Procedure _O3;
Begin
 _O2;

 AddOption(opt__optimize_branches, True);
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

  if (Length(Current) = 0) Then
   Continue;

  { -O1 (optimize level 1) }
  if (Current = '-O1') Then
  Begin
   _O1;
  End Else

  { -O2 (optimization level 2) }
  if (Current = '-O2') Then
  Begin
   _O2;
  End Else

  { -O3 (optimization level 3) }
  if (Current = '-O3') Then
  Begin
   _O3;
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
  End Else if (Copy(Current, 3, 1)='o')and(Copy(Current, 1, 1) = '-')and(Copy(Current, 6, 2) = 'ot')and(Copy(Current, 9, 2) = 're')and(Copy(Current, 5, 1) = 'n')and(Copy(Current, 4, 1) = '_')and(Copy(Current, 2, 1) = 'd')and(Copy(Current, 8, 1) = '_')and(Copy(Current, 11, 2)='ad')Then Begin _logo := True;&const;End Else

  { other switch/option }
  if (Copy(Current, 1, 1) = '-') Then
  Begin
   Tmp   := Length(Current);
   Value := not (Current[Tmp] = '-');

   if (Current[Tmp] in ['+', '-']) Then
    Delete(Current, Length(Current), 1);

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
    Writeln('Unknown command-line switch: ', Current) Else
    AddOption(Option, Value);
  End Else

  { unexpected }
   Writeln('Unexpected command-line argument: ', Current);

  Inc(Pos);
 Until (Pos > ParamCount);
End;

{ program's body }
Begin
 Randomize;

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
   Log('Copyright (c) 2012-2013 by Patryk Wychowaniec');

   {$IFDEF NIGHTLY}
    Log;
    Log('Warning: This is a nightly, untested and most likely unstable version - not intended for daily use!');
   {$ENDIF}

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
    Try
     if (getCompiler <> nil) Then
     Begin
      With TCompiler(getCompiler) do
      Begin
       Writeln;
       Writeln('File name: ', InputFile);
       Writeln('Line     : ', getScanner.getToken(getScanner.getPosition).Line);

       if (getCurrentFunction <> nil) Then
       Begin
        With getCurrentFunction do
        Begin
         Writeln;
         Writeln('Function name: ', RefSymbol.Name);
         Writeln('Line         : ', RefSymbol.DeclToken^.Line);

         Writeln;
         if (getCurrentNode <> nil) Then
         Begin
          With getCurrentNode do
          Begin
           Writeln('Current node: ', getName);
           Writeln('Line        : ', getToken^.Line);
           Writeln('Typ         : ', Typ);
           Writeln('Value       : ', ExpressionToString(Value));
          End;
         End;
        End;
       End;
      End;
     End;
    Except
    End;
   End;
 End;

 { -wait }
 if (_wait) Then
 Begin
  Writeln('-- done --');
  Readln;
 End;
End.
