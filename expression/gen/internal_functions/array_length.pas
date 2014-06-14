Procedure __array_length;
Begin
 Result := TYPE_INT;

 // array.length()
 if (Length(Expr^.ParamList) = 0) Then
 Begin
  Compiler.PutOpcode(o_arlen, ['er1', 'ei1']);
 End Else

 // invalid syntax
 Begin
  Error(eWrongParamCount, ['length', Length(Expr^.ParamList), '0']);
 End;
End;
