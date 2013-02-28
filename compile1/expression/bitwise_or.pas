Procedure ParseBitwiseOR;
Var TypeLeft, TypeRight: PMType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
  if (not isTypeInt(Result)) Then
   CompileError(eUnsupportedOperator, [getTypeDeclaration(TypeLeft), getDisplay(Expr), getTypeDeclaration(TypeRight)]) Else
   PutOpcode(o_or, ['ei1', 'ei2']);
End;
