 { array_length (array type) }
 Procedure __array_length;
 Var Variable: TRVariable;
     TmpType : PMType;
     TmpExpr : PMExpression;
     Typ     : TMType;
     Ch      : Char;
 Begin
  // parameter check
  if (Length(Expr^.ParamList) <> 1) Then
  Begin
   Error(eWrongParamCount, ['array_length', Length(Expr^.ParamList), 1]);
   Exit;
  End;

  // parse parameter and check type
  if not (Expr^.ParamList[0]^.Typ in [mtVariable, mtArrayElement]) Then
  Begin
   Error(eInvalidExpression, []);
   Exit;
  End;

  Variable := getVariable(Expr^.ParamList[0]);
  TmpExpr  := Expr^.ParamList[0];
  Typ      := Variable.Typ^;

  if (TmpExpr^.Right <> nil) Then
  Begin
   Repeat
    TmpType := Parse(TmpExpr^.Right);
    With Compiler do // array subscript must be an integer value
     if (not isTypeInt(TmpType)) or (Typ.ArrayDimCount = 0) Then
     Begin
      Error(TmpExpr^.Right^.Token, eInvalidArraySubscript, [getTypeDeclaration(Typ), getTypeDeclaration(TmpType)]);
      Exit;
     End;

    TmpExpr := TmpExpr^.Left;

    Dec(Typ.ArrayDimCount);
   Until not (TmpExpr^.Typ in [mtArrayElement]);
  End;

  if (Typ.ArrayDimCount = 0) Then
  Begin
   Error(eWrongTypeInCall, ['array_length', 1, Compiler.getTypeDeclaration(Typ), 'array']);
   Exit;
  End;

  // put opcode
  Ch := Compiler.getTypePrefix(Variable.Typ);
  Compiler.PutOpcode(o_arlen, ['e'+Ch+'1', Variable.getArray, 'ei1']);

  Result := TypeInstance(TYPE_INT);
  Exit;
 End;
