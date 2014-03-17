Procedure ParseCall(const isMethodCall: Boolean);
Var Symbol: Pointer;

{ RequiredParamCount }
Function RequiredParamCount(const ParamList: TFunctionParamList): Integer;
Var I: Integer;
Begin
 Result := 0;

 For I := Low(ParamList) To High(ParamList) Do
  if (ParamList[I].DefaultValue <> nil) and (not ParamList[I].isVar) Then
   Break Else
   Inc(Result);
End;

{ ParseParamList }
Procedure ParseParamList(const FuncName: String; const ParamList: TFunctionParamList);
Var ParamID: Integer;
    Param  : PExpressionNode;
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
    if (not (Param^.Typ in [mtIdentifier, mtArrayElement])) Then // error: expected variable
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

{ CleanAfterCall }
Procedure CleanAfterCall(const ParamList: TFunctionParamList);
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

// -------------------------------------------------------------------------- //
{ CastCall }
Function CastCall: TType;
Var TypeID       : TType;
    Param        : Integer;
    FuncParamList: TFunctionParamList;
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

{ VarCall }
Function VarCall: TType;
Var TypeID       : TType;
    Param        : Integer;
    FuncParamList: TFunctionParamList;
    TmpVar       : TRVariable;
    IdentName    : String;
Begin
 if (Left^.Typ = mtArrayElement) Then
 Begin
  TypeID    := Parse(Left);
  IdentName := '<anonymouse array-call>';

  RePop(Left, TypeID, 1);
 End Else
 Begin
  TmpVar := getVariable(Left);

  if (TmpVar.Symbol = nil) Then
   Exit;

  IdentName := TmpVar.Name;
  TypeID    := __variable_getvalue_reg(TmpVar, 1, 'r');
 End;

 if (not TypeID.isSimple) Then // error: complex type
 Begin
  Error(eCannotBeUsedAsFunction, [TypeID.asString]);
  Exit;
 End;

 if (not TypeID.isFunctionPointer) Then // error: not a callable function pointer
 Begin
  Error(eWrongType, [TypeID.asString, 'function pointer']);
  Exit;
 End;

 FuncParamList := TypeID.FuncParams;
 Result        := TypeID.FuncReturn;

 if (not TypeID.isUnspecialized) Then
 Begin
  // check param count
  if (not (Length(Expr^.ParamList) in [RequiredParamCount(FuncParamList)..Length(FuncParamList)])) Then
  Begin
   Error(eWrongParamCount, [IdentName, Length(FuncParamList), Length(Expr^.ParamList)]);
   Exit;
  End;
 End;

 // push parameters onto the stack
 if (TypeID.isUnspecialized) Then
 Begin
  For Param := High(Expr^.ParamList) Downto Low(Expr^.ParamList) Do
   Parse(Expr^.ParamList[Param]);
 End Else
  ParseParamList(IdentName, FuncParamList);

 // call function-pointer
 Compiler.PutOpcode(o_acall, ['er1']);

 // clean after call
 CleanAfterCall(FuncParamList);
End;

{ GlobalFuncCall }
Procedure GlobalFuncCall;
Begin
 With TSymbol(Symbol), mFunction do
 Begin
  // check param count
  if (not (Length(Expr^.ParamList) in [RequiredParamCount(ParamList)..Length(ParamList)])) Then
  Begin
   Error(eWrongParamCount, [RefSymbol.Name, Length(ParamList), Length(Expr^.ParamList)]);
   Exit;
  End;

  // push parameters onto the stack
  ParseParamList(RefSymbol.Name, ParamList);

  // call function
  Compiler.PutOpcode(o_call, [':$function.'+IntToStr(uint64(Pointer(mFunction)))]);

  // clean after call
  CleanAfterCall(ParamList);

  Result := Return;
 End;
End;

{ MethodCall }
Function MethodCall: TType; // pseudo-OOP for arrays :P

 // magic
 Function magic(const Expr: PExpressionNode): TType;
 Var mObject   : TType;
     MethodName: String;

 {$I array_length.pas}

 Begin
  Result     := nil;
  mObject    := Parse(Expr^.Left);
  MethodName := Expr^.Right^.IdentName;

  if (not mObject.isObject) Then // is it an object?
  Begin
   Error(eNonObjectMethodCall, [MethodName, mObject.asString]);
   Exit;
  End;

  RePop(Expr^.Left, mObject, 1); // re-pop object address

  if (mObject.isArray(False)) and (MethodName = 'length') Then
  Begin
   __array_length;
  End Else
  Begin
   Error(eMethodNotFound, [MethodName, mObject.asString]);
   Exit;
  End;

  Result := TYPE_INT;
 End;

Begin
 Result := magic(Expr);
End;

Begin
 Symbol := Expr^.Symbol;

 // calling a method?
 if (isMethodCall) Then
 Begin
  Result := MethodCall();
  Exit;
 End;

 if (Expr^.Symbol = nil) Then // function not found, array-call or cast-call
 Begin
  if (VarToStr(Expr^.Value) = 'array-call') Then // array-call -> array[10]();
  Begin
   Result := VarCall();
   Exit;
  End;

  if (VarToStr(Expr^.Value) = 'cast-call') Then // cast-call -> cast<function>(expression)()
  Begin
   Result := CastCall();
   Exit;
  End;

  // is it any of the internal functions?
  {Case VarToStr(Left^.Value) of
   { @Note: when changing internal functions, modify also TInterpreter.MakeTree->CreateNodeFromStack and TCompiler.RedeclarationCheck }
   '':
  End;}

  Compiler.CompileError(Expr^.Token, eUnknownFunction, [Expr^.Value]);

  Result := TYPE_VOID;
  Exit;
 End;

 { variable call }
 if (TSymbol(Symbol).Typ = stVariable) Then
  Result := VarCall Else

 { function call }
 if (TSymbol(Symbol).Typ = stFunction) Then
  GlobalFuncCall Else

  Compiler.CompileError(eInternalError, ['call.pas -> don''t know what to do!']);
End;
