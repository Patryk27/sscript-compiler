Procedure ParseAssign;
Var Variable       : TRVariable;
    TypeID, TmpType: TType;
    Index, DimCount: Byte;
    ShouldFail     : Boolean;
Label asArray;
Begin
 { left side is l-value (variable), right side is the expression to parse (a value, which we'll assign into the variable) }

 if (not isLValue(Left)) Then
 Begin
  Error(Left^.Token, eLValueRequired, []);
  Exit;
 End;

 Variable := getVariable(Left, True);
 if (Variable.ID = -1) Then // variable not found
  Exit;

 if (Variable.isConst) Then // error message had been already shown in `getVariable`
  Exit;

 (* ===== not arrays ===== *)
 if (not Variable.Typ.isArray) or (Right^.Typ = mtNew) Then
 Begin
  if (Left^.Typ = mtArrayElement) Then // tried to access eg.`int`-typed variable like array
  Begin
   Error(eInvalidArraySubscript, [Variable.Typ.asString, Parse(Left^.Right).asString]);
   Exit;
  End;

  if (Variable.MemPos > 0) Then // if variable is stored in a register, we can directly set this variable's value (without using an helper register)
  Begin
   TypeID := Parse(Right, Variable.MemPos, Variable.RegChar);
  End Else
  Begin
   TypeID := Parse(Right, 1); // parse expression and load it into the helper register (e_1)
   RePop(Right, TypeID, 1);

   __variable_setvalue_reg(Variable, 1, TypeID.RegPrefix);
  End;

  Compiler.PutOpcode(o_mov, ['e'+TypeID.RegPrefix+'1', Variable.PosStr]);

  With Compiler do
   if (not TypeID.CanBeAssignedTo(Variable.Typ)) Then // type check
   Begin
    Error(eWrongTypeInAssign, [Variable.Name, TypeID.asString, Variable.Typ.asString]);
    Exit;
   End;

  Result := Variable.Typ;
  Exit;
 End;

 (* ===== arrays ===== *)
asArray:
 Index := 0;

 { push indexes onto the stack }
 While (Left^.Typ <> mtVariable) do
 Begin
  TypeID := Parse(Left^.Right);
  With Compiler do // array subscript must be an integer value
   if (not TypeID.isInt) Then
    Error(eInvalidArraySubscript, [Variable.Typ.asString, TypeID.asString]);

  Left := Left^.Left;
  Inc(Index);
 End;

 { normal arrays }
 if (Index = 0) Then // pointer assignment (changing what our varable points at)
 Begin
  TypeID := Parse(Right, 1); // this value will be our new pointer
  RePop(Right, TypeID, 1);

  { type check }
  With Compiler do
  Begin
   if (not TypeID.CanBeAssignedTo(Variable.Typ)) Then
   Begin
    Error(eWrongTypeInAssign, [Variable.Name, TypeID.asString, Variable.Typ.asString]);
    Exit;
   End;
  End;

  { set new pointer }
  Result := __variable_setvalue_reg(Variable, 1, TypeID.RegPrefix);

  Exit;
 End;

 { value assignment }
 DimCount := Variable.Typ.ArrayDimCount;
 if (Index <> DimCount) Then
 Begin
  // special case: strings
  if (Index = DimCount-1) and (Variable.Typ.isString) Then
  Begin
  End Else
   Error(eInvalidArrayAssign, []);
 End;

 TypeID := Parse(Right, 1); // this value will be saved into the array
 RePop(Right, TypeID, 1);

 { type check }
 With Compiler do
 Begin
  TmpType := Variable.Typ.ArrayBase;

  ShouldFail := False;

  if (TmpType.isString and (Integer(Variable.Typ.ArrayDimCount)-Index <= 0)) Then
  Begin
   ShouldFail := not TypeID.isChar;
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

  if (not TypeID.CanBeAssignedTo(TmpType)) or (ShouldFail) Then
   Error(eWrongTypeInAssign, [Variable.Name, TypeID.asString, TmpType.asString]);
 End;

 { set new array's element's value }
 Compiler.PutOpcode(o_arset, [Variable.PosStr, Index, 'e'+TypeID.RegPrefix+'1']);
 Dec(PushedValues, Index);

 Result := TypeID;
End;
