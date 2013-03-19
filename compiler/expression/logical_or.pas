Procedure ParseLogicalOR;
Var TypeLeft, TypeRight: TType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
  if (not Result.isBool) Then
   Error(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]) Else
   PutOpcode(o_or, ['eb1', 'eb2']);
End;
