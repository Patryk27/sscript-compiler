Procedure ParseLogicalNOT;
Begin
 // load value into the first register
 Result := Parse(Left, 1);
 RePop(Left, Result, 1);

 if (not Compiler.isTypeInt(Result)) and (not Compiler.isTypeBool(Result)) Then
 Begin
  Error(eUnsupportedUOperator, [getDisplay(Expr), Compiler.getTypeDeclaration(Result)]);
  Exit;
 End;

 if (Compiler.isTypeInt(Result)) Then // implicit cast: int->bool
  Compiler.PutOpcode(o_mov, ['eb1', 'ei1']);

 Compiler.PutOpcode(o_not, ['eb1']);

 Result := TypeInstance(TYPE_BOOL);
End;
