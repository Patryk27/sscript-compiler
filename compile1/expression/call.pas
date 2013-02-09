Procedure ParseCall;
Var FuncID, Param, TypeID: Integer;

 { array_length (array type) }
 Procedure __array_length;
 Var Param: TVType;
     Ch   : Char;
 Begin
  Error(eInternalError, ['Sorry, but it doesn''t work now - check in future updates ;<']);

  // parameter check
  if (Length(Expr^.ParamList) <> 1) Then
  Begin
   Error(eWrongParamCount, [Expr^.Value, Length(Expr^.ParamList), 1]);
   Exit;
  End;

  // parse parameter and check types
  Param := Parse(Expr^.ParamList[0], 1);
  RePop(Expr^.ParamList[0], Param, 1);
  if (not Compiler.isTypeArray(Param)) Then
  Begin
   Error(eWrongTypeInCall, ['array_length', 1, Compiler.getTypeName(Param), 'array']);
   Exit;
  End;

  Ch := Compiler.getTypePrefix(Param);
  Compiler.PutOpcode(o_arlen, ['e'+Ch+'1']);

  Result := TYPE_INT;
  Exit;
 End;

Label NotAnInternalFunction;
Begin
 FuncID := Compiler.findFunction(Expr^.Value); // get function ID

 if (FuncID = -1) Then // function not found
 Begin
  // is it any of internal functions?
  Case VarToStr(Expr^.Value) of
   'array_length': __array_length;
   else goto NotAnInternalFunction;
  End;
  Exit;

 NotAnInternalFunction:
  FuncID := Compiler.findTypeByName(Expr^.Value); // so, is it a type-casting?

  if (FuncID = -1) Then // No, it's not ;<
  Begin
   Error(eUnknownFunction, [Expr^.Value]);
   Exit;
  End;

  // Yes, it is - so do casting
  if (Length(Expr^.ParamList) <> 1) Then
  Begin
   Error(eWrongParamCount, [Expr^.Value, Length(Expr^.ParamList), 1]);
   Exit;
  End;

  // load a value to cast onto the register
  Expr^.ResultOnStack := False;
  TypeID := Parse(Expr^.ParamList[0], 1, Compiler.getTypePrefix(FuncID));

  Result := FuncID;

  // ... but fail on `void`-casting (from `void` or to `void`)
  if (Compiler.isTypeVoid(Result) or Compiler.isTypeVoid(TypeID)) Then
   Error(eVoidCasting, []);

  Exit;
 End;

 With Compiler.FunctionList[FuncID] do
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
   TypeID := Parse(Expr^.ParamList[Param]);

   With Compiler do
    if (not CompareTypes(ParamList[High(ParamList)-Param].Typ, TypeID)) Then
     Error(Expr^.ParamList[Param]^.Token, eWrongTypeInCall, [Expr^.Value, High(ParamList)-Param+1, getTypeName(TypeID), getTypeName(ParamList[High(ParamList)-Param].Typ)]);
  End;

  Dec(PushedValues, Length(ParamList));

  // call function
  Compiler.PutOpcode(o_call, [':'+MName]);

  Result := Return;
 End;
End; 
