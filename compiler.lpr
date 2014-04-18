(*
 SScript Compiler
 Copyright © by Patryk Wychowaniec, 2013-2014

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
 {$FATAL 64-bit CPUs are not supported (yet)!} // (because of floating point type which is a bit different on x86-64)
{$ENDIF}

Program compilerprog;
Uses SysUtils, TypInfo, Logging, CommandLine, SSCompiler, ExpressionParser, LibInfo, symdef;
Var InputFile, OutputFile: String;

    LibInfoMode: (limDisabled, limEnabled);

    Compiler: TCompiler;

    Frame : Integer;
    Frames: PPointer;

(* RunCompiler *)
Procedure RunCompiler;
Begin
 Compiler := TCompiler.Create;
 Compiler.CompileCode(InputFile, OutputFile);
End;

Begin
 Randomize;

 DefaultFormatSettings.DecimalSeparator := '.';

 Try
  CmdLine := TCommandLine.Create;
  CmdLine.ParseCommandLine; // parse command line

  if (ParamCount < 1) Then // too few parameters specified
  Begin
   Writeln('Usage:');
   Writeln('compiler.exe [input file] <options>');
   Writeln;
   Writeln('See `command-line.txt` for more information.');
  End Else
  Begin
   if (not CmdLine.getBoolSwitch(opt_silent)) Then
   Begin
    Writeln(Format('SScript Compiler %s (compiled %s)', [Version, {$I %DATE%}]));
    Writeln('Copyright (c) 2013-2014 by Patryk Wychowaniec');

    {$IFDEF NIGHTLY}
     Writeln;
     Writeln('Warning: this is a nightly, untested and most likely unstable version - not intended for daily use!');
    {$ENDIF}

    Writeln;
   End;

   if (Length(CmdLine.getInputFile) = 0) Then
    raise Exception.Create('');

   InputFile  := ExpandFileName(CmdLine.getInputFile); // get input file
   OutputFile := ExpandFileName(CmdLine.getStringSwitch(opt_output, 'output.ssc')); // get ouput file

   LibInfoMode := limDisabled;

   if (CmdLine.getStringSwitch(opt__compile_mode) = 'libinfo') Then
   Begin
    LibInfoMode := limEnabled;
    Log('LibInfo mode activated.');
    Log('Input library: %s', [InputFile]);
    Log;
   End Else

   Begin
    Log('Input file: %s', [InputFile]);
    Log('Output file: %s', [OutputFile]);
    Log;

    if (not FileExists(InputFile)) Then // error: input file not found
     raise Exception.Create('Input file does not exist.');

    if (InputFile = OutputFile) Then // error: input file is the same as the output
     raise Exception.Create('Input file is the same as output file.');
   End;

   Case LibInfoMode of
    limDisabled: RunCompiler();
    limEnabled : RunLibInfo(InputFile);
   End;
  End;
 Except
  On E: ECommandLineException Do
  Begin
   Writeln(E.Message);
  End;

  On E: Exception Do
  Begin
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
     if (Compiler <> nil) Then
     Begin
      With Compiler do
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
     Writeln('<unknown>');
    End;
   End;
  End;
 End;

 { -wait }
 if (CmdLine.getBoolSwitch(opt_wait)) Then
 Begin
  Writeln('-- done --');
  Readln;
 End;
End.
