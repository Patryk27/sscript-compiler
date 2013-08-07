Procedure ParseBitwiseNOT;
Begin
 // load value into the first register
 Result := Parse(Left, 1);
 RePop(Left, Result, 1);

 if (not Result.isInt) Then
 Begin
  Error(eUnsupportedUOperator, [getDisplay(Expr), Result.asString]);
  Exit;
 End;

 Compiler.PutOpcode(o_not, ['ei1']);
End;
