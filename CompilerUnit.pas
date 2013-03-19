(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit CompilerUnit;

 Interface

 { types }
 // TCommandLineOption
 Type TCommandLineOption =
 (
  opt_bytecode, opt_output, opt_initcode, opt_includepath,
  opt_Cm, opt_internal_const,
  opt__register_alloc, opt__constant_folding, opt__bytecode_optimize, opt_O1,
  opt_header,
  opt_logo, opt_wait, opt_verbose
 );

 Type TCommandLineName = Record
                          Names: Array[0..1] of String;
                          Typ  : (pBool, pString, pInt);
                         End;

 Const CommandLineNames: Array[TCommandLineOption] of TCommandLineName =
 (
  (Names: ('-bytecode',       '-b'); Typ: pString),
  (Names: ('-output',         '-o'); Typ: pString),
  (Names: ('-initcode',       '');   Typ: pBool),
  (Names: ('-includepath',    '');   Typ: pString),

  (Names: ('-Cm', '');                    Typ: pString),
  (Names: ('-internal-const', '-Cconst'); Typ: pBool),

  (Names: ('--register-alloc', '-Or');    Typ: pBool),
  (Names: ('--constant-folding', '-Of');  Typ: pBool),
  (Names: ('--bytecode-optimize', '-Op'); Typ: pBool),
  (Names: ('-O1', '');                    Typ: pBool),

  (Names: ('-header', '-h'); Typ: pString),

  (Names: ('-logo', '');      Typ: pBool),
  (Names: ('-wait', '');      Typ: pBool),
  (Names: ('-verbose', '-v'); Typ: pBool)
 );

 // TCompileOption
 Type TCompileOption = Record
                        Option: TCommandLineOption;
                        Value : Variant;
                       End;
 Type TCompileOptions = Array of TCompileOption;

 { functions }
 Procedure Log(const Text: String);
 Procedure Log;

 Procedure DevLog(const Text: String);
 Procedure DevLog;

 Function getCompiler: Pointer;

 Function getBoolOption(O: String; Default: Boolean): Boolean;
 Function getStringOption(O: String; Default: String): String;
 Function getIntOption(O: String; Default: Integer): Integer;

 Procedure CompileCode(Input, Output: String; Options: TCompileOptions);

 Var verbose_mode: Boolean = False;
     show_devlog : Boolean = False;

 Implementation
Uses Compile1, SysUtils;
Var Compiler: TCompiler = nil;

{ Log }
Procedure Log(const Text: String);
Begin
 if (verbose_mode) Then
  Writeln(Text);
End;

{ Log }
Procedure Log;
Begin
 Log('');
End;

{ DevLog }
Procedure DevLog(const Text: String);
Begin
 if (show_devlog) Then
  Writeln(Text);
End;

{ DevLog }
Procedure DevLog;
Begin
 DevLog('');
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
 Gets a boolean-typed option from a command line
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
 Gets a string-typed option from a command line
}
Function getStringOption(O: String; Default: String): String;
Var I  : Integer;
    Str: String;
Begin
 O := '-'+O;

 Result := Default;
 For I := 1 To ParamCount Do
 Begin
  Str := ParamStr(I);
  if (Copy(Str, 1, Pos('=', Str)-1) = O) Then
  Begin
   Result := Copy(Str, Pos('=', Str)+1, Length(Str));
   Exit;
  End;
 End;
End;

(* getIntOption *)
{
 Gets a integer-typed option from a command line
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
 Compiler.Free;
End;
End.
