Procedure ParseBitwiseOR;
Var TypeLeft, TypeRight: TType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 if (Result = nil) Then
  Exit;

 With Compiler do
 Begin
  if (Result.isInt) Then
   PutOpcode(o_or, ['ei1', 'ei2']) Else
   CompileError(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]);
 End;
End;
