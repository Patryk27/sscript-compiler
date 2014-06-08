(* __simple_assign *)
{ variable = value; }
Procedure __simple_assign(const Variable: TRVariable);
Label WrongTypeInAssign;
Var AssignedType: TType;
Begin
 // check for invalid variable use - like "intvar[index] = 0xCAFEBABE;" ('int' is not an array).
 if (Left^.Typ = mtArrayElement) Then
 Begin
  Error(eInvalidArraySubscript, [Variable.Typ.asString, Parse(Left^.Right).asString]);
  Exit;
 End;

 // if variable is stored in a register, we can directly set this variable's value (without using a helper register)
 if (Variable.isStoredInRegister) Then
 Begin
  AssignedType := Parse(Right, Variable.LocationData.RegisterID, Variable.RegChar);
 End Else

 // otherwise use a helper register
 Begin
  // parse expression and load it into a helper register (e_1)
  AssignedType := Parse(Right, 1);

  if (AssignedType = nil) Then
   goto WrongTypeInAssign;

  // repop value back to the register (if it was put on the stack)
  RePop(Right, AssignedType, 1);

  // set variable value
  __variable_setvalue_reg(Variable, 1, AssignedType.RegPrefix);
 End;

 // quick chech for "foo = foo;" assignments.
 if (Right^.Symbol = Left^.Symbol) Then
 Begin
  Hint(hExpressionHasNoEffect, []);
 End;

 if (AssignedType = nil) Then
 Begin
  DevLog(dvError, 'AssignedType = nil; leaving function...');
  Exit;
 End;

 // put opcode
 Compiler.PutOpcode(o_mov, ['e'+AssignedType.RegPrefix+'1', Variable.PosStr]);

 // do type check
 if (not AssignedType.CanBeAssignedTo(Variable.Typ)) Then
 Begin
  WrongTypeInAssign:
  Error(eWrongTypeInAssign, [Variable.Name, AssignedType.asString, Variable.Typ.asString]);
  Exit;
 End;

 Result := Variable.Typ;
End;

(* __string_char_assign *)
{ string[index] = char; }
Procedure __string_char_assign(const Variable: TRVariable);
Var IndexType, AssignedType: TType;
Begin
 // parse the right side (to ec1, preferably)
 AssignedType := Parse(Right, 1, 'c');

 // ensure it's char
 if (not AssignedType.isChar) Then
 Begin
  Error(eWrongTypeInAssign, [Variable.Name, AssignedType.asString, 'char']);
  Exit;
 End;

 // check the left side
 if (Left^.Left^.Typ = mtArrayElement) Then // too much! eg.: string[index1][index2]
 Begin
  Error(eInvalidArrayAssign, []);
  Exit;
 End;

 // parse the left side (to ei1, preferably)
 IndexType := Parse(Left^.Right, 1, 'i');

 // ensure it's int
 if (not IndexType.isInt) Then
 Begin
  Error(eInvalidArraySubscript, [Variable.Typ.asString, IndexType.asString]);
  Exit;
 End;

 // repop
 RePop(Right, AssignedType, 1);
 RePop(Left^.Right, IndexType, 1);

 // put opcode
 Compiler.PutOpcode(o_strset, [Variable.PosStr, 'ei1', 'ec1']);
End;

(* __array_assign *)
{ array = other_array; }
{ array[index] = value; }
{ array[a][b] = value; }
Procedure __array_assign(const Variable: TRVariable);
Var IndexType, AssignedType, TmpType: TType;

    Index, DimCount: uint8;
    ShouldFail     : Boolean;

//    IndexExpr: PExpressionNode;
Begin
 Index := 0;

 // push indexes onto the stack
 //IndexExpr := Left;
 While (Left^.Typ <> mtIdentifier) do
 Begin
  IndexType := Parse(Left^.Right);

  // make sure subscript is an integer
  if (not IndexType.isInt) Then
   Error(eInvalidArraySubscript, [Variable.Typ.asString, IndexType.asString]);

  Left := Left^.Left;
  Inc(Index);
 End;

 // is it a pointer assignment? (changing what our varable points at, not the array elements themselves)
 if (Index = 0) Then
 Begin
  if (Variable.isStoredInRegister) Then
  Begin
   AssignedType := Parse(Right, Variable.LocationData.RegisterID, Variable.RegChar);
  End Else
  Begin
   AssignedType := Parse(Right, 1); // this value will be our new pointer
   RePop(Right, AssignedType, 1);
  End;

  // type check
  if (not AssignedType.CanBeAssignedTo(Variable.Typ)) Then
  Begin
   Error(eWrongTypeInAssign, [Variable.Name, AssignedType.asString, Variable.Typ.asString]);
   Exit;
  End;

  // set new pointer, if not done already
  if (not Variable.isStoredInRegister) Then
   Result := __variable_setvalue_reg(Variable, 1, AssignedType.RegPrefix);

  Exit;
 End;

 // so it isn't a pointer assignment - we're modyfing real array elements here
 DimCount := Variable.Typ.ArrayDimCount;

 // special case - if we have just "array[index]", we can use "arset1" opcode - so load value to ei2 register
 {if (IndexCount = 1) Then
 Begin
  RePop(IndexExpr^.Right, TYPE_INT, 2);
 End;} // @TODO: what about a case where the assigned expression is complex and invalidates ei2 register?

 // parse assigned value
 AssignedType := Parse(Right, 1);
 RePop(Right, AssignedType, 1);

 // do type check
 With Compiler do
 Begin
  TmpType := Variable.Typ;

  // get lower array type
  For DimCount := 1 To Index Do
  Begin
   TmpType := TmpType.getLowerArray;

   if (TmpType = nil) Then
   Begin
    Error(eInvalidArrayAssign, []);
    Exit;
   End;
  End;

  // another type check
  ShouldFail := False;

  if (TmpType.isString and (Integer(Variable.Typ.ArrayDimCount)-Index <= 0)) Then
  Begin
   ShouldFail := not AssignedType.isChar;
   TmpType    := TYPE_CHAR;
  End;

  {
   @Note: I think it needs a small explanation:

   As you can see - I did a special case for strings; let's consider this code:

     var<string[]> tab = new string[10];
     str[1] = "Hello World!";
     str[1][3] = '_';

   Here we have an array alocation and three assignments.
   The second assignment is a simple `"Hello World!'` saving into the second item of the array, so it changes the whole element's value.
   ... but the third changes only a one char of that array's element's value.
   So we need to detect this situation and don't display error message.
   That's what this `if` above does.
  }

  if (not AssignedType.CanBeAssignedTo(TmpType)) or (ShouldFail) Then
   Error(eWrongTypeInAssign, [Variable.Name, AssignedType.asString, TmpType.asString]);
 End;

 // set new array's element's value
 Compiler.PutOpcode(o_arset, [Variable.PosStr, Index, 'e'+AssignedType.RegPrefix+'1']);
 Dec(PushedValues, Index);

 Result := TmpType;
End;

(* ParseAssign *)
Procedure ParseAssign;
Var Variable: TRVariable;
Begin
 { left side is l-value, right side is the expression to parse (a value, which we'll assign into the l-value) }
 if (not isLValue(Left)) Then
 Begin
  Error(Left^.Token, eLValueExpected, []);
  Exit;
 End;

 Variable := getVariable(Left, True);

 // variable not found
 if (Variable.Symbol = nil) Then
  Exit;

 // error message had been already shown in `getVariable`
 if (Variable.isConst) Then
  Exit;

 // jump to the right code generator
 if (not Variable.Typ.isArray) {or (Right^.Typ = mtNew)} Then
  __simple_assign(Variable) Else

 if (Variable.Typ.isString) and (Variable.Typ.ArrayDimCount = 1) and (Left^.Typ = mtArrayElement) Then
  __string_char_assign(Variable) Else

  __array_assign(Variable);
End;
