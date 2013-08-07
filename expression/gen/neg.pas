Procedure ParseNEG;
Begin
 // load value into the first register
 Result := Parse(Left, 1);
 RePop(Left, Result, 1);

 if (not Result.isNumerical) Then // numerical types only
 Begin
  Error(eUnsupportedUOperator, [getDisplay(Expr), Result.asString]);
  Exit;
 End;

 Compiler.PutOpcode(o_neg, ['e'+Result.RegPrefix+'1']);
End;
