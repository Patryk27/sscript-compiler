(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Logging;

 Interface

 { TDevlogVerbosity }
 Type TDevlogVerbosity = (dvInfo, dvWarning, dvError, dvFatal);

 Procedure Log(const Format: String; const Args: Array of Const);
 Procedure Log(const Message: String);
 Procedure Log;

 Procedure DevLog(const Verbosity: TDevlogVerbosity; const Format: String; const Args: Array of Const);
 Procedure DevLog(const Verbosity: TDevlogVerbosity; const Message: String);
 Procedure DevLog;

 Var VerboseEnabled: Boolean = False;
     DevlogEnabled : Boolean = False;

 Implementation
Uses SysUtils, LineInfo;

(* Log *)
Procedure Log(const Format: String; const Args: Array of const);
Begin
 if (VerboseEnabled) Then
  Writeln(SysUtils.Format(Format, Args));
End;

(* Log *)
Procedure Log(const Message: String);
Begin
 if (VerboseEnabled) Then
  Writeln(Message);
End;

(* Log *)
Procedure Log;
Begin
 Log('');
End;

(* DevLog *)
Procedure DevLog(const Verbosity: TDevlogVerbosity; const Format: String; const Args: Array of const);
Const VerbosityStr: Array[TDevlogVerbosity] of String = ('info', 'warning', 'error', 'fatal');
Var Frame, Address: Pointer;

    FunctionName, SourceFile: ShortString;
    FunctionLine            : Integer;

    Counter: uint8 = 0;
Begin
 if (DevlogEnabled) Then
 Begin
  Repeat
   FunctionName := 'unknown';
   SourceFile   := 'unknown';
   FunctionLine := -1;

   Frame   := get_frame;
   Address := get_caller_addr(Frame);

   GetLineInfo(uint32(Address), FunctionName, SourceFile, FunctionLine);

   Frame := get_caller_frame(Frame);
   Inc(Counter);
  Until (FunctionName <> 'DEVLOG') or (Counter > 3); // small trick, because it used not to return valid caller name but "DEVLOG" sometimes

  Writeln('[', VerbosityStr[Verbosity], '] ', FunctionName, '() -> ', SysUtils.Format(Format, Args));
 End;
End;

(* DevLog *)
Procedure DevLog(const Verbosity: TDevlogVerbosity; const Message: String);
Begin
 DevLog(Verbosity, Message, []);
End;

(* DevLog *)
Procedure DevLog;
Begin
 if (DevlogEnabled) Then
  Writeln;
End;
End.
