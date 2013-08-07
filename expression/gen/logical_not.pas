Procedure ParseLogicalNOT;
Begin
 // load value into the first register
 Result := Parse(Left, 1);
 RePop(Left, Result, 1);

 if (not Result.isInt) and (not Result.isBool) Then
 Begin
  Error(eUnsupportedUOperator, [getDisplay(Expr), Result.asString]);
  Exit;
 End;

 if (Result.isInt) Then // implicit cast: int->bool
  Compiler.PutOpcode(o_mov, ['eb1', 'ei1']);

 Compiler.PutOpcode(o_not, ['eb1']);

 Result := TYPE_BOOL;
End;
