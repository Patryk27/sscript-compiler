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
