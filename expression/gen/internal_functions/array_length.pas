Procedure __array_length;
Var Param: TType;
Begin
 {
  `array.length` can be called in 3 ways:
    1) .length()
    2) .length(int dimension)
    3) the wrong way ;)
 }
 if (Length(Expr^.ParamList) = 0) Then // no params passed, case #1
 Begin
  Compiler.PutOpcode(o_arlen, ['er1', 1, 'ei1']);
 End Else

 if (Length(Expr^.ParamList) = 1) Then // 1 param passed, case #2
 Begin
  Param := Parse(Expr^.ParamList[0]);
  RePop(Expr^.ParamList[0], Param, 1);

  if (not Param.isInt) Then // have to be `int`
  Begin
   Error(eWrongTypeInCall, ['length', Param.asString, 'int']);
   Exit;
  End;

  Compiler.PutOpcode(o_arlen, ['er1', 'e'+Param.RegPrefix+'1', 'ei1']);
 End Else

  Error(eWrongParamCount, ['length', Length(Expr^.ParamList), '1']); // wrong syntax
End;
