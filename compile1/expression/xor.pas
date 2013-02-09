Procedure ParseXOR;
Var TypeLeft, TypeRight: TVType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
 Begin
  if (isTypeInt(Result)) Then
   PutOpcode(o_xor, ['ei1', 'ei2']) Else
  if (isTypeBool(Result)) Then
   PutOpcode(o_xor, ['eb1', 'eb1']) Else
   CompileError(eUnsupportedOperator, [getTypeName(TypeLeft), getDisplay(Expr), getTypeName(TypeRight)])
 End;
End;
