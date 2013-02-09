Procedure ParseArithmeticOperator(const WithAssign: Boolean);
Var TypeLeft, TypeRight: TVType;
    Opcode             : TOpcode_E;
    Variable           : TRVariable;
    RegChar            : Char;
Begin
 (*
  `WithAssign` operators are `+=`, `-=` and so on.
  `not WithAssign` operators are just `+`, `-` (...)
 *)
 if (WithAssign) Then
 Begin
  if (not isLValue(Left)) Then
  Begin
   Error(eLValueExpected, [getDisplay(Left)]);
   Exit;
  End;

  Variable := getVariable(Left, True);
  if (Variable.ID = -1) Then // variable not found
   Exit;
 End Else
  Variable.getArray := 0;

 { compile both sides }
 Result := CompileSimple(TypeLeft, TypeRight, WithAssign);

 // `array <operator> array` is an invalid construction
 With Compiler do
  if (isTypeArray(TypeLeft) and isTypeArray(TypeRight)) and
     ((not isTypeString(TypeLeft)) or (not isTypeString(TypeRight))) Then
  Begin
   Error(eUnsupportedOperator, [Compiler.getTypeName(TypeLeft), getDisplay(Expr), Compiler.getTypeName(TypeRight)]);
   Exit;
  End;

 { prepare opcode }
 Case Expr^.Typ of
  mtAdd, mtAddEq: if (Compiler.isTypeString(TypeLeft)) Then
                   Opcode := o_strjoin Else
                   Opcode := o_add;
  mtSub, mtSubEq: Opcode := o_sub;
  mtMul, mtMulEq: Opcode := o_mul;
  mtDiv, mtDivEq: Opcode := o_div;
  mtMod, mtModEq: Opcode := o_mod; // @TODO: ints only
  mtSHL, mtSHLEq: Opcode := o_shl;
  mtSHR, mtSHREq: Opcode := o_shr;
 End;

 { not arrays }
 if (Variable.getArray = 0) or (Compiler.isTypeString(Variable.Typ) and (Compiler.TypeTable[Variable.Typ].ArrayDimCount = 1)) Then
 Begin
  if ((not (Expr^.Typ in [mtAdd, mtAddEq])) and (not Compiler.isTypeNumerical(Result))) Then // numerical types only (except '+' and '+=' for strings)
  Begin
   Error(eUnsupportedOperator, [Compiler.getTypeName(TypeLeft), getDisplay(Expr), Compiler.getTypeName(TypeRight)]);
   Exit;
  End;

  // opcode
  Case WithAssign of
   True: Compiler.PutOpcode(Opcode, [Variable.PosStr, 'e'+Compiler.getTypePrefix(TypeRight)+'2']);
   False: Compiler.PutOpcode(Opcode, ['e'+Compiler.getTypePrefix(TypeLeft)+'1', 'e'+Compiler.getTypePrefix(TypeRight)+'2']);
  End;

  // type-check
  if (WithAssign) Then
   if (not Compiler.CompareTypes(Variable.Typ, TypeRight)) Then
    Error(eWrongTypeInAssign, [Variable.Name, Compiler.getTypeName(TypeRight), Compiler.getTypeName(Variable.Typ)]);
 End Else

 { arrays }
 Begin
  (*
   @Note:
   When a pure variable is passed into `+=` (and derivatives), it can be directly increased, decreased etc., as we exactly know
   where it is (exact stack-value-position or register).
   When an array is passed, we know only its object-pointer, so we have to:
        1.Get a current value from the array.
        2.Increase/decrease/anything else it.
        3.Save a new value into the array.
  *)

  // Step 1: get a current value from the array
  RegChar  := Compiler.getTypePrefix(Compiler.getArrayBaseType(Variable.Typ));
  TypeLeft := __variable_getvalue_array_reg(Variable.ID, 1, RegChar, Left);

  if (not Compiler.CompareTypes(TypeLeft, TypeRight)) Then
  Begin
   Error(eWrongTypeInAssign, [Variable.Name, Compiler.getTypeName(TypeRight), Compiler.getTypeName(TypeLeft)]);
   Exit;
  End;

  // Step 1.5: cast-table
  With Compiler do
  Begin
   // ?
  End;

  // Step 2: change this value
  Compiler.PutOpcode(Opcode, ['e'+RegChar+'1', 'e'+Compiler.getTypePrefix(TypeRight)+'2']);

  // Step 3: save new value into the array
  __variable_setvalue_array_reg(Variable.ID, 1, RegChar, Left);
 End;
End;
