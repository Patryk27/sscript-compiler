Procedure ParseCall(const isMethodCall: Boolean);
Var Symbol: Pointer;

// ReverseParamList
Procedure ReverseParamList;
Var Tmp: Array of PExpression;
    I  : Integer;
Begin
 SetLength(Tmp, Length(Expr^.ParamList));
 For I := 0 To High(Expr^.ParamList) Do
  Tmp[I] := Expr^.ParamList[High(Expr^.ParamList)-I];
 Expr^.ParamList := Tmp;
End;

// RequiredParamCount
Function RequiredParamCount(const ParamList: TParamList): Integer;
Var I: Integer;
Begin
 Result := 0;

 For I := Low(ParamList) To High(ParamList) Do
  if (ParamList[I].DefaultValue <> nil) and (not ParamList[I].isVar) Then
   Break Else
   Inc(Result);
End;

// ParseParamList
Procedure ParseParamList(const FuncName: String; const ParamList: TParamList);
Var ParamID: Integer;
    Param  : PExpression;
    TypeID : TType;
Begin
 With Compiler do
 Begin
  For ParamID := High(ParamList) Downto Low(ParamList) Do
  Begin
   if (ParamID > High(Expr^.ParamList)) Then
   Begin
    Parse(ParamList[ParamID].DefaultValue);
    Continue;
   End;

   Param := Expr^.ParamList[ParamID];

   if (ParamList[ParamID].isVar) Then // is variable required?
   Begin
    if (not (Param^.Typ in [mtVariable, mtArrayElement])) Then // error: expected variable
     Compiler.CompileError(Expr^.ParamList[ParamID]^.Token, eLValueExpected, []);
   End;

   if (ParamList[ParamID].isVar) and (ParamList[ParamID].DefaultValue <> nil) Then // special case: var-ParamID and default value
    TypeID := Parse(ParamList[ParamID].DefaultValue) Else
    TypeID := Parse(Expr^.ParamList[ParamID]);

   With Compiler do
    if (not TypeID.CanBeAssignedTo(ParamList[ParamID].Typ)) Then
     Error(Expr^.ParamList[ParamID]^.Token, eWrongTypeInCall, [FuncName, ParamID+1, TypeID.asString, ParamList[ParamID].Typ.asString]);
  End;
 End;
End;

// CleanAfterCall
Procedure CleanAfterCall(const ParamList: TParamList);
Var Param: Integer;
    rVar : TRVariable;
Begin
 For Param := Low(ParamList) To High(ParamList) Do
  if (ParamList[Param].isVar) Then
  Begin
   rVar := getVariable(Expr^.ParamList[Param]);

   if (rVar.getArray > 0) Then
   Begin
    Compiler.PutOpcode(o_mov, ['e'+rVar.RegChar+'1', '['+IntToStr(-Param)+']']);
    __variable_setvalue_array_reg(rVar, 1, rVar.RegChar, Expr^.ParamList[Param]);
   End Else
    Compiler.PutOpcode(o_mov, [rVar.PosStr, '['+IntToStr(-Param)+']']);
  End;

 Compiler.PutOpcode(o_sub, ['stp', Length(ParamList)]);
 Dec(PushedValues, Length(ParamList))
End;

{ CastCall }
Function CastCall: TType;
Var TypeID       : TType;
    Param        : Integer;
    FuncParamList: TParamList;
Begin
 TypeID := Parse(Left, 1, 'r');
 RePop(Left, TypeID, 1);

 With Compiler do
  if (not TypeID.isFunctionPointer) Then
  Begin
   Error(eWrongType, [TypeID.asString, 'function pointer']);
   Exit;
  End;

 FuncParamList := TypeID.FuncParams;
 Result        := TypeID.FuncReturn;

 if (not TypeID.isUnspecialized) Then
 Begin
  // check param count
  if not (Length(Expr^.ParamList) in [RequiredParamCount(FuncParamList)..Length(FuncParamList)]) Then
  Begin
   Error(eWrongParamCount, ['<anonymouse cast-function>', Length(FuncParamList), Length(Expr^.ParamList)]);
   Exit;
  End;
 End;

 // push parameters onto the stack
 if (TypeID.isUnspecialized) Then
 Begin
  For Param := High(Expr^.ParamList) Downto Low(Expr^.ParamList) Do
   Parse(Expr^.ParamList[Param]);
 End Else
  ParseParamList('<anonymouse cast-function>', FuncParamList);

 // call function-pointer
 Compiler.PutOpcode(o_acall, ['er1']);

 // clean after call
 CleanAfterCall(FuncParamList);
End;

{ LocalVarCall }
Function LocalVarCall: TType;
Var TypeID       : TType;
    Param        : Integer;
    Variable     : TRVariable;
    FuncParamList: TParamList;
Begin
 Variable := getVariable(Left);

 if (Variable.Symbol = nil) Then
  Exit;

 TypeID := __variable_getvalue_reg(Variable, 1, 'r');

 With Compiler do
  if (not TypeID.isFunctionPointer) Then // a function pointer is required
  Begin
   Error(eWrongType, [TypeID.asString, 'function pointer']);
   Exit;
  End;

 FuncParamList := TypeID.FuncParams;
 Result        := TypeID.FuncReturn;

 if (not TypeID.isUnspecialized) Then
 Begin
  // check param count
  if not (Length(Expr^.ParamList) in [RequiredParamCount(FuncParamList)..Length(FuncParamList)]) Then
  Begin
   Error(eWrongParamCount, [Variable.Name, Length(FuncParamList), Length(Expr^.ParamList)]);
   Exit;
  End;
 End;

 // push parameters onto the stack
 if (TypeID.isUnspecialized) Then
 Begin
  For Param := High(Expr^.ParamList) Downto Low(Expr^.ParamList) Do
   Parse(Expr^.ParamList[Param]);
 End Else
  ParseParamList(Variable.Name, FuncParamList);

 // call function-pointer
 Compiler.PutOpcode(o_acall, ['er1']);

 // clean after call
 CleanAfterCall(FuncParamList);
End;

{ GlobalFuncCall }
Procedure GlobalFuncCall;
Begin
 With TGlobalSymbol(Symbol), mFunction do
 Begin
  // check param count
  if not (Length(Expr^.ParamList) in [RequiredParamCount(ParamList)..Length(ParamList)]) Then
  Begin
   Error(eWrongParamCount, [Name, Length(ParamList), Length(Expr^.ParamList)]);
   Exit;
  End;

  // push parameters onto the stack
  ParseParamList(Name, ParamList);

  // call function
  Compiler.PutOpcode(o_call, [':'+MangledName]);

  // clean after call
  CleanAfterCall(ParamList);

  Result := Return;
 End;
End;

{ MethodCall }
Procedure MethodCall; // pseudo-OOP for arrays :P

 // magic
 Function magic(Expr: PExpression): TType;
 Var method, param: TType;
     name         : String;

 {$I array_length.pas}

 Begin
  Result := nil;
  method := Parse(Expr^.Left);
  name   := Expr^.Right^.IdentName;

  RePop(Expr^.Left, method, 1);

  if (not method.isObject) Then // is it an "object"?
  Begin
   Error(eNonObjectMethodCall, [name, method.asString]);
   Exit;
  End;

  if (method.isArray(False)) and (Name = 'length') Then
  Begin
   __array_length;
  End Else
  Begin
   Error(eMethodNotFound, [name, method.asString]);
   Exit;
  End;

  Result := TYPE_INT;
 End;

Begin
 Result := magic(Expr);
End;

Begin
 ReverseParamList;

 Symbol := Expr^.Symbol;

 Try
  // calling a method?
  if (isMethodCall) Then
  Begin
   MethodCall;
   Exit;
  End;

  if (Expr^.Symbol = nil) Then // function not found or cast-call
  Begin
   if (VarToStr(Expr^.Value) = 'cast-call') Then // cast-call
   Begin
    Result := CastCall;
    Exit;
   End;

   // is it any of internal functions?
   Case VarToStr(Left^.Value) of
    { @Note: when changing internal functions, modify also TInterpreter.MakeTree->CreateNodeFromStack and Parse_FUNCTION }
    '':
   End;

   Exit; // error message had been already shown when building the expression tree
  End;

  { local variable call }
  if (Expr^.isLocal) Then
   Result := LocalVarCall Else

  { global function call }
  if (TGlobalSymbol(Symbol).Typ = gsFunction) Then
   GlobalFuncCall Else

  { global variable call (and, as there's no global variables for now, there's no glob-var-call) }
   Error(eInternalError, ['@TODO']);
 Finally
  ReverseParamList;
 End;
End;
