Procedure ParseCall(const isMethodCall: Boolean);
Var IdentID, Namespace: Integer;

// CleanAfterCall
Procedure CleanAfterCall(const ParamList: TParamList);
Var Param: Integer;
Begin
 For Param := Low(ParamList) To High(ParamList) Do
  if (ParamList[High(ParamList)-Param].isVar) Then
   Compiler.PutOpcode(o_mov, [getVariable(Expr^.ParamList[Param]).PosStr, '['+IntToStr(-(High(ParamList)-Param))+']']);

 Compiler.PutOpcode(o_sub, ['stp', Length(ParamList)]);
 Dec(PushedValues, Length(ParamList))
End;

{ CastCall }
Function CastCall: TType;
Var TypeID       : TType;
    Param        : Integer;
    fParamList   : TParamList;
    Unspecialized: Boolean;
Begin
 TypeID := Parse(Left, 1, 'r');
 RePop(Left, TypeID, 1);

 With Compiler do
  if (not TypeID.isFunctionPointer) Then
  Begin
   Error(eWrongType, [TypeID.asString, 'function pointer']);
   Exit;
  End;

 fParamList    := TypeID.FuncParams;
 Result        := TypeID.FuncReturn;
 Unspecialized := TypeID.isUnspecialized;

 if (not Unspecialized) Then
 Begin
  // check param count
  if (Length(Expr^.ParamList) <> Length(fParamList)) Then
  Begin
   Error(eWrongParamCount, ['anonymous cast-function', Length(fParamList), Length(Expr^.ParamList)]);
   Exit;
  End;
 End Else
  SetLength(fParamList, Length(Expr^.ParamList));

 // push parameters onto the stack
 For Param := Low(fParamList) To High(fParamList) Do
 Begin
  if (fParamList[High(fParamList)-Param].isVar) Then
   if (Expr^.ParamList[Param]^.Typ <> mtVariable) Then // error: expected variable
    Compiler.CompileError(Expr^.ParamList[Param]^.Token, eLValueExpected, []);

  TypeID := Parse(Expr^.ParamList[Param]);

  if (not Unspecialized) Then
   With Compiler do
    if (not TypeID.CanBeAssignedTo(fParamList[High(fParamList)-Param].Typ)) Then
     Error(Expr^.ParamList[Param]^.Token, eWrongTypeInCall, ['anonymous cast-function', High(fParamList)-Param+1, TypeID.asString, fParamList[High(fParamList)-Param].Typ.asString]);
 End;

 // call function-pointer
 Compiler.PutOpcode(o_acall, ['er1']);

 // clean after call
 CleanAfterCall(fParamList);
End;

{ LocalVarCall }
Function LocalVarCall: TType;
Var TypeID       : TType;
    Param        : Integer;
    Variable     : TRVariable;
    fParamList   : TParamList;
    Unspecialized: Boolean;
Begin
 Variable := getVariable(Left);

 if (Variable.ID = -1) Then
  Exit;

 TypeID := __variable_getvalue_reg(Variable, 1, 'r');

 With Compiler do
  if (not TypeID.isFunctionPointer) Then
  Begin
   Error(eWrongType, [TypeID.asString, 'function pointer']);
   Exit;
  End;

 fParamList    := TypeID.FuncParams;
 Result        := TypeID.FuncReturn;
 Unspecialized := TypeID.isUnspecialized;

 if (not Unspecialized) Then
 Begin
  // check param count
  if (Length(Expr^.ParamList) <> Length(fParamList)) Then
  Begin
   Error(eWrongParamCount, [Variable.Name, Length(fParamList), Length(Expr^.ParamList)]);
   Exit;
  End;
 End Else
  SetLength(fParamList, Length(Expr^.ParamList));

 // push parameters onto the stack
 For Param := Low(fParamList) To High(fParamList) Do
 Begin
  if (fParamList[High(fParamList)-Param].isVar) Then
   if (Expr^.ParamList[Param]^.Typ <> mtVariable) Then // error: expected variable
    Compiler.CompileError(Expr^.ParamList[Param]^.Token, eLValueExpected, []);

  TypeID := Parse(Expr^.ParamList[Param]);

  if (not Unspecialized) Then
   With Compiler do
    if (not TypeID.CanBeAssignedTo(fParamList[High(fParamList)-Param].Typ)) Then
     Error(Expr^.ParamList[Param]^.Token, eWrongTypeInCall, [Variable.Name, High(fParamList)-Param+1, TypeID.asString, fParamList[High(fParamList)-Param].Typ.asString]);
 End;

 // call function-pointer
 Compiler.PutOpcode(o_acall, ['er1']);

 // clean after call
 CleanAfterCall(fParamList);
End;

{ GlobalFuncCall }
Procedure GlobalFuncCall;
Var TypeID: TType;
    Param : Integer;
Begin
 With Compiler.NamespaceList[Namespace].SymbolList[IdentID], mFunction do
 Begin
  // check param count
  if (Length(Expr^.ParamList) <> Length(ParamList)) Then
  Begin
   Error(eWrongParamCount, [Name, Length(ParamList), Length(Expr^.ParamList)]);
   Exit;
  End;

  // push parameters onto the stack
  For Param := Low(ParamList) To High(ParamList) Do
  Begin
   if (ParamList[High(ParamList)-Param].isVar) Then
    if (Expr^.ParamList[Param]^.Typ <> mtVariable) Then // error: expected variable
     Compiler.CompileError(Expr^.ParamList[Param]^.Token, eLValueExpected, []);

   TypeID := Parse(Expr^.ParamList[Param]);

   With Compiler do
    if (not TypeID.CanBeAssignedTo(ParamList[High(ParamList)-Param].Typ)) Then
     Error(Expr^.ParamList[Param]^.Token, eWrongTypeInCall, [Name, High(ParamList)-Param+1, TypeID.asString, ParamList[High(ParamList)-Param].Typ.asString]);
  End;

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
 Function magic(Expr: PMExpression): TType;
 Var method, param: TType;
     name         : String;

 {$I array_length.pas}

 Begin
  Result := nil;
  method := Parse(Expr^.Left);
  name   := VarToStr(Expr^.Right^.Value);

  RePop(Expr^.Left, method, 1);

  if (not method.isObject) Then // is it an object?
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
 IdentID    := Expr^.IdentID;
 Namespace  := Expr^.IdentNamespace;

 // calling a method?
 if (isMethodCall) Then
 Begin
  MethodCall;
  Exit;
 End;

 if (Expr^.IdentID = -1) Then // function not found or cast-call
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
 if (Compiler.NamespaceList[Namespace].SymbolList[IdentID].Typ = gsFunction) Then
  GlobalFuncCall Else

 { global variable call (and, as there's no global variables for now, there's no glob-var-call) }
  Error(eInternalError, ['@TODO']);
End;
