Procedure ParseXOR;
Var TypeLeft, TypeRight: PMType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
 Begin
  if (isTypeInt(Result)) Then
   PutOpcode(o_xor, ['ei1', 'ei2']) Else
  if (isTypeBool(Result)) Then
   PutOpcode(o_xor, ['eb1', 'eb1']) Else
   CompileError(eUnsupportedOperator, [getTypeDeclaration(TypeLeft), getDisplay(Expr), getTypeDeclaration(TypeRight)])
 End;
End;
