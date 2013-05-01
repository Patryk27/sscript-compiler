Procedure ParseArithmeticOperator(const WithAssign: Boolean);
Var TypeLeft, TypeRight: TType;
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
   Error(Left^.Token, eLValueRequired, []);
   Exit;
  End;

  Variable := getVariable(Left, True);
  if (Variable.ID = -1) Then // variable not found
   Exit;
 End Else
  Variable.getArray := 0;

 { compile both sides }
 Result := CompileSimple(TypeLeft, TypeRight, WithAssign);

 if (Result = nil) Then
 Begin
  DevLog('Warning: ParseArithmeticOperator() -> CompileSimple() returned `nil`; leaving function...');
  Exit;
 End;

 // `array <operator> array` is an invalid construction
 With Compiler do
  if (TypeLeft.isArray(False) and TypeRight.isArray(False)) Then
  Begin
   Error(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]);
   Exit;
  End;

 { prepare opcode }
 Case Expr^.Typ of
  mtAdd, mtAddEq: if (TypeLeft.isString) Then
                   Opcode := o_strjoin Else
                   Opcode := o_add;
  mtSub, mtSubEq: Opcode := o_sub;
  mtMul, mtMulEq: Opcode := o_mul;
  mtDiv, mtDivEq: Opcode := o_div;
  mtMod, mtModEq: Opcode := o_mod;
  mtSHL, mtSHLEq: Opcode := o_shl;
  mtSHR, mtSHREq: Opcode := o_shr;
 End;

 { not arrays }
 if (Variable.getArray = 0) or (Variable.Typ.isString and (Variable.Typ.ArrayDimCount = 1)) Then
 Begin
  if ((not (Expr^.Typ in [mtAdd, mtAddEq])) and (not Result.isNumerical)) or // numerical types only (except '+' and '+=' for strings)
     ((Opcode in [o_mod, o_shl, o_shr]) and (not Result.isInt)) Then // some operators are `int-`only
  Begin
   Error(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]);
   Exit;
  End;

  // put opcode
  Case WithAssign of
   True: Compiler.PutOpcode(Opcode, [Variable.PosStr, 'e'+TypeRight.RegPrefix+'2']);
   False: Compiler.PutOpcode(Opcode, ['e'+TypeLeft.RegPrefix+'1', 'e'+TypeRight.RegPrefix+'2']);
  End;

  // check types
  if (WithAssign) Then
   if (not TypeRight.CanBeAssignedTo(Variable.Typ)) Then
    Error(eWrongTypeInAssign, [Variable.Name, TypeRight.asString, Variable.Typ.asString]);
 End Else

 { arrays }
 Begin
  (*
   @Note:
   When a pure variable is passed into `+=` (and derivatives), it can be directly increased, decreased etc., as we exactly know
   where it is (exact stack position or register).
   When an array element is passed, we know only its object-pointer and element's index, so we have to:
        1.Get a current value from the array.
        2.Increase/decrease/anything else it.
        3.Save a new value into the array.
  *)

  // Step 1: get a current value from the array
  RegChar  := Variable.Typ.ArrayBase.RegPrefix;
  TypeLeft := __variable_getvalue_array_reg(Variable, 1, RegChar, Left);

  if (TypeLeft.isArray) Then
  Begin
   Error(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]);
   Exit;
  End;

  if (not TypeRight.CanBeAssignedTo(TypeLeft)) Then
  Begin
   Error(eWrongTypeInAssign, [Variable.Name, TypeRight.asString, TypeLeft.asString]);
   Exit;
  End;

  // Step 2: change this value
  Compiler.PutOpcode(Opcode, ['e'+RegChar+'1', 'e'+TypeRight.RegPrefix+'2']);

  // Step 3: save new value into the array
  __variable_setvalue_array_reg(Variable, 1, RegChar, Left);
 End;
End;
