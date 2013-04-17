Procedure ParseBitwiseAND;
Var TypeLeft, TypeRight: TType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 if (Result = nil) Then
  Exit;

 With Compiler do
  if (not Result.isInt) Then
   CompileError(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]) Else
   PutOpcode(o_and, ['ei1', 'ei2']);
End;
