Procedure ParseNEG;
Begin
 // load value into the first register
 Result := Parse(Left, 1);
 RePop(Left, Result, 1);

 if (not Compiler.isTypeNumerical(Result)) Then // numerical types only
 Begin
  Error(eUnsupportedUOperator, [getDisplay(Expr), Compiler.getTypeName(Result)]);
  Exit;
 End;

 Compiler.PutOpcode(o_neg, ['e'+Compiler.getTypePrefix(Result)+'1']);
End;
