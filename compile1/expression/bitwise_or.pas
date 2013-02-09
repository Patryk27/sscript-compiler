Procedure ParseBitwiseOR;
Var TypeLeft, TypeRight: TVType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
  if (not isTypeInt(Result)) Then
   CompileError(eUnsupportedOperator, [getTypeName(TypeLeft), getDisplay(Expr), getTypeName(TypeRight)]) Else
   PutOpcode(o_or, ['ei1', 'ei2']);
End;
