Procedure __array_length;
Begin
 if (not Compiler.isTypeArray(method, False)) Then // only `array.length()` is allowed
 Begin
  Error(eMethodNotFound, [name, Compiler.getTypeDeclaration(method)]);
  Exit;
 End;

 {
  `array.length` can be called in 3 ways:
    1) .length()
    2) .length(int dimension)
    3) the wrong way (:D)
 }
 if (Length(Expr^.ParamList) = 0) Then // no params passed, case #1
 Begin
  Compiler.PutOpcode(o_arlen, ['er1', 1, 'ei1']);
 End Else

 if (Length(Expr^.ParamList) = 1) Then // 1 param passed, case #2
 Begin
  param := Parse(Expr^.ParamList[0]);
  RePop(Expr^.ParamList[0], param, 1);

  if (not Compiler.isTypeInt(param)) Then // have to be `int`
  Begin
   Error(eWrongTypeInCall, ['length', Compiler.getTypeDeclaration(param), 'int']);
   Exit;
  End;

  Compiler.PutOpcode(o_arlen, ['er1', 'e'+Compiler.getTypePrefix(param)+'1', 'ei1']);
 End Else

  Error(eWrongParamCount, ['length', Length(Expr^.ParamList), '1']); // wrong syntax
End;
