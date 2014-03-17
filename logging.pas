(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Logging;

 Interface

 { TDevlogVerbosity }
 Type TDevlogVerbosity = (dvInfo, dvWarning, dvError, dvFatal);

 Procedure Log(const Fmt: String; const Args: Array of Const);
 Procedure Log(const Msg: String);
 Procedure Log;

 Procedure DevLog(const Verbosity: TDevlogVerbosity; const FuncName, Text: String);
 Procedure DevLog;

 Var VerboseEnabled: Boolean = False;
     DevlogEnabled : Boolean = False;

 Implementation
Uses SysUtils;

(* Log *)
Procedure Log(const Fmt: String; const Args: Array of const);
Begin
 if (VerboseEnabled) Then
  Writeln(Format(Fmt, Args));
End;

(* Log *)
Procedure Log(const Msg: String);
Begin
 if (VerboseEnabled) Then
  Writeln(Msg);
End;

(* Log *)
Procedure Log;
Begin
 Log('');
End;

(* DevLog *)
Procedure DevLog(const Verbosity: TDevlogVerbosity; const FuncName, Text: String);
Const VerbosityStr: Array[TDevlogVerbosity] of String = ('info', 'warning', 'error', 'fatal');
Begin
 if (DevlogEnabled) Then
  Writeln('[', VerbosityStr[Verbosity], '] ', FuncName, '() -> ', Text);
End;

(* DevLog *)
Procedure DevLog;
Begin
 if (DevlogEnabled) Then
  Writeln;
End;
End.
