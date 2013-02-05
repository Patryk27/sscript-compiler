(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit CompilerUnit;

 Interface

 Type TCompileOption =
 (
  _NINIT, _Or, _Of, _Op, _O1, _DBG, _Clib, _Cbcode, _iconst, _sconst
 );
 
 Type TCompileOptions = Set of TCompileOption;

 Const OptionNames: Array[0..9] of String =
 (
  'ninit', 'Or', 'Of', 'Op', 'O1', 'dbg', 'Clib', 'Cbcode', 'iconst', 'sconst'
 );

 Function getCompiler: Pointer;

 Function getBoolOption(O: String; Default: Boolean): Boolean;
 Function getStringOption(O: String; Default: String): String;
 Function getIntOption(O: String; Default: Integer): Integer;

 Procedure CompileCode(Input, Output: String; Options: TCompileOptions);

 Implementation
Uses Compile1, SysUtils;
Var Compiler: TCompiler = nil;

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
 For I := 0 To ParamCount Do
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
Var I: Integer;
Begin
 O := '-'+O;

 Result := Default;
 For I := 0 To ParamCount-1 Do
  if (ParamStr(I) = O) Then
  Begin
   Result := ParamStr(I+1);
   Exit;
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
