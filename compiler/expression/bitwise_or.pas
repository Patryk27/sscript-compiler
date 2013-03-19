Procedure ParseBitwiseOR;
Var TypeLeft, TypeRight: TType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
  if (not Result.isInt) Then
   CompileError(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]) Else
   PutOpcode(o_or, ['ei1', 'ei2']);
End;
