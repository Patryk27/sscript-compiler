Procedure __array_length;
Begin
 {
  `array.length` can be called in 2 ways:
    1) .length()
    2) the wrong way ;)
 }

 // no parameters passed, case #1
 if (Length(Expr^.ParamList) = 0) Then
 Begin
  Compiler.PutOpcode(o_arlen, ['er1', 'ei1']);
 End Else

 // invalid syntax
 Begin
  Error(eWrongParamCount, ['length', Length(Expr^.ParamList), '1']);
 End;
End;
