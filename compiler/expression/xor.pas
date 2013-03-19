Procedure ParseXOR;
Var TypeLeft, TypeRight: TType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
 Begin
  if (Result.isInt) Then
   PutOpcode(o_xor, ['ei1', 'ei2']) Else
  if (Result.isBool) Then
   PutOpcode(o_xor, ['eb1', 'eb1']) Else
   CompileError(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]);
 End;
End;
