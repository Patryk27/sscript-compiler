Procedure ParsePIncDec;
Var Variable: TRVariable;
    RegChar : Char;
    TypeID  : PMType;

 Procedure Fail;
 Begin
  if (Expr^.Typ in [mtPreInc, mtPreDec]) Then
   Error(eUnsupportedUOperator, [getDisplay(Expr), Compiler.getTypeDeclaration(Variable.Typ)]) Else
   Error(eUnsupportedUOperator, [Compiler.getTypeDeclaration(Variable.Typ), getDisplay(Expr)]);
 End;

Begin
 // left side have to be a l-value
 if (not isLValue(Left)) Then
 Begin
  Error(Left^.Token, eLValueRequired, []);
  Exit;
 End;

 Variable := getVariable(Expr^.Left, True);
 if (Variable.ID = -1) Then // variable not found
  Exit;

 if (not Compiler.isTypeNumerical(Variable.Typ)) Then
  Fail;

 { not array }
 if (Variable.getArray = 0) Then
 Begin
  if (Expr^.Typ in [mtPostInc, mtPostDec]) and (isSubCall) Then
   Compiler.PutOpcode(o_mov, ['e'+Variable.RegChar+'1', Variable.PosStr]);

  Case Expr^.Typ of
   mtPreInc, mtPostInc: Compiler.PutOpcode(o_add, [Variable.PosStr, 1]);
   mtPreDec, mtPostDec: Compiler.PutOpcode(o_sub, [Variable.PosStr, 1]);
  End;

  if (Expr^.Typ in [mtPreInc, mtPreDec]) and (isSubCall) Then
   Compiler.PutOpcode(o_mov, ['e'+Variable.RegChar+'1', Variable.PosStr]);

  Result := Variable.Typ;
 End Else

 { array }
 Begin
  // Step 1: load a current array's element's value into second register
  RegChar := Compiler.getTypePrefix(Compiler.getArrayBaseType(Variable.Typ));
  TypeID  := __variable_getvalue_array_reg(Variable, 2, RegChar, Left);

  if (Compiler.isTypeArray(TypeID)) Then
  Begin
   Fail;
   Exit;
  End;

  // Step 1.5
  if (Expr^.Typ in [mtPostInc, mtPostDec]) and (isSubCall) Then
   Compiler.PutOpcode(o_mov, ['e'+RegChar+'1', 'e'+RegChar+'2']);

  // Step 2: increase od decrease value
  Case Expr^.Typ of
   mtPreInc, mtPostInc: Compiler.PutOpcode(o_add, ['e'+RegChar+'2', 1]);
   mtPreDec, mtPostDec: Compiler.PutOpcode(o_sub, ['e'+RegChar+'2', 1]);
  End;

  // Step 2.5
  if (Expr^.Typ in [mtPreInc, mtPreDec]) and (isSubCall) Then
   Compiler.PutOpcode(o_mov, ['e'+RegChar+'1', 'e'+RegChar+'2']);

  // Step 3: save new value into the array
  __variable_setvalue_array_reg(Variable, 2, RegChar, Left);

  Result := TypeID;
 End;
End;
