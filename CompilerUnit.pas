(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit CompilerUnit;

 Interface

 (* -- types -- *)

 { TCommandLineOption }
 Type TCommandLineOption =
 (
  opt_bytecode, opt_output, opt_includepath,
  opt_Cm, opt_internal_const,
  opt__register_alloc, opt__constant_folding, opt__tree_simplify, opt__bytecode_optimize, opt__remove_dead, opt__short_circuit,
  opt__constant_propagation,
  opt__optimize_branches,
  opt__strip_debug, opt__dump_cfg,
  opt_logo, opt_version, opt_wait, opt_verbose
 );

 { TCommandLineName }
 Type TCommandLineName =
      Record
       Names: Array[0..1] of String;
       Typ  : (pBool, pInt, pString);
      End;

 { CommandLineNames }
 Const CommandLineNames: Array[TCommandLineOption] of TCommandLineName =
 (
  (Names: ('-bytecode',       '-b'); Typ: pString),
  (Names: ('-output',         '-o'); Typ: pString),
  (Names: ('-includepath',    '');   Typ: pString),

  (Names: ('-Cm', '');                    Typ: pString),
  (Names: ('-internal-const', '-Cconst'); Typ: pBool),

  (Names: ('--register-alloc', '-Or');       Typ: pBool),
  (Names: ('--constant-folding', '-Of');     Typ: pBool),
  (Names: ('--tree-simplify', '-Ot');        Typ: pBool),
  (Names: ('--bytecode-optimize', '-Op');    Typ: pBool),
  (Names: ('--remove-dead', '-Ou');          Typ: pBool),
  (Names: ('--short-circuit', '-Os');        Typ: pBool),
  (Names: ('--constant-propagation', '-Oc'); Typ: pBool),
  (Names: ('--optimize-branches', '-Ob');    Typ: pBool),

  (Names: ('--strip-debug', '-Sd'); Typ: pBool),
  (Names: ('--dump-cfg', '');       Typ: pBool),

  (Names: ('-logo', '');      Typ: pBool),
  (Names: ('-version', '');   Typ: pBool),
  (Names: ('-wait', '');      Typ: pBool),
  (Names: ('-verbose', '-v'); Typ: pBool)
 );

 { TCompileOption }
 Type TCompileOption = Record
                        Option: TCommandLineOption;
                        Value : Variant;
                       End;
 Type TCompileOptions = Array of TCompileOption;

 { TDevlogVerbosity }
 Type TDevlogVerbosity = (dvInfo, dvWarning, dvError, dvFatal);

 (* -- functions -- *)
 Procedure Log(const Text: String);
 Procedure Log;

 Procedure DevLog(const Verbosity: TDevlogVerbosity; const FuncName, Text: String);
 Procedure DevLog;

 Function getCompiler: Pointer;

 Function getBoolOption(O: String; Default: Boolean): Boolean;
 Function getStringOption(O: String; Default: String): String;
 Function getIntOption(O: String; Default: Integer): Integer;

 Procedure CompileCode(Input, Output: String; Options: TCompileOptions);

 Var verbose_mode: Boolean = False;
     show_devlog : Boolean = False;

 Implementation
Uses SSCompiler, SysUtils;
Var Compiler: TCompiler = nil;

(* Log *)
Procedure Log(const Text: String);
Begin
 if (verbose_mode) Then
  Writeln(Text);
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
 if (show_devlog) Then
  Writeln('[', VerbosityStr[Verbosity], '] ', FuncName, '() -> ', Text);
End;

(* DevLog *)
Procedure DevLog;
Begin
 if (show_devlog) Then
  Writeln;
End;

(* getCompiler *)
{
 Returns current compiler instance (Compile1.TCompiler).
}
Function getCompiler: Pointer;
Begin
 Result := Compiler;
End;

(* getBoolOption *)
{
 Gets a boolean-typed option from the command line
}
Function getBoolOption(O: String; Default: Boolean): Boolean;
Var I: Integer;
Begin
 O := '-'+O;
 
 Result := Default;
 For I := 1 To ParamCount Do
  if (ParamStr(I) = O+'+') or (ParamStr(I) = O) Then
   Exit(True) Else
  if (ParamStr(I) = O+'-') Then
   Exit(False);
End;

(* getStringOption *)
{
 Gets a string-typed option from the command line
}
Function getStringOption(O: String; Default: String): String;
Var I  : Integer;
    Str: String;
Begin
 O := '-'+O;

 Result := Default;
 For I := 1 To ParamCount-1 Do
 Begin
  Str := ParamStr(I);
  if (Str = O) Then
  Begin
   Result := ParamStr(I+1);
   Exit;
  End;
 End;
End;

(* getIntOption *)
{
 Gets an integer-typed option from the command line
}
Function getIntOption(O: String; Default: Integer): Integer;
Begin
 Try
  Result := StrToInt(getStringOption(O, IntToStr(Default)));
 Except
  Result := Default;
 End;
End;

(* CompileCode *)
{
 Compiles code
}
Procedure CompileCode(Input, Output: String; Options: TCompileOptions);
Begin                                            
 Compiler := TCompiler.Create;
 Compiler.CompileCode(Input, Output, Options);
 Compiler.Free; // lack of try..finally construction here is purposeful
End;
End.
