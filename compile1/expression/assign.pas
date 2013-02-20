Procedure ParseAssign;
Var Variable       : TRVariable;
    TypeID, TmpType: TVType;
    Index, DimCount: Byte;
    ShouldFail     : Boolean;
Begin
 { left side is l-value (variable), right side is the expression to parse (a value, which we'll assign into the variable) }

 if (not isLValue(Left)) Then
 Begin
  Error(eLValueExpected, [getDisplay(Left)]);
  Exit;
 End;

 Variable := getVariable(Left, True);
 if (Variable.ID = -1) Then // variable not found
  Exit;

 if (Variable.isConst) Then
  Exit;

 (* ===== not arrays ===== *)
 if (not Compiler.isTypeArray(Variable.Typ)) or (Right^.Typ = mtNew) Then
 Begin
  if (Left^.Typ = mtArrayElement) Then // tried to access eg.`int`-typed variable like array
  Begin
   Error(eInvalidArraySubscript, [Compiler.getTypeName(Variable.Typ), Compiler.getTypeName(Parse(Left^.Left))]);
   Exit;
  End;

  if (Variable.RegID > 0) Then // if variable is stored in the register, we can directly set this variable's value (without using a helper-register)
  Begin
   TypeID := Parse(Right, Variable.RegID, Variable.RegChar);
  End Else
  Begin
   TypeID := Parse(Right, 1); // parse expression and load it into the helper register (e_1)
   RePop(Right, TypeID, 1);

   __variable_setvalue_reg(Variable, 1, Compiler.getTypePrefix(TypeID));
  End;

  Compiler.PutOpcode(o_mov, ['e'+Compiler.getTypePrefix(TypeID)+'1', Variable.PosStr]);

  With Compiler do
   if (not CompareTypes(Variable.Typ, TypeID)) Then // type check
   Begin
    Error(eWrongTypeInAssign, [Variable.Name, getTypeName(TypeID), getTypeName(Variable.Typ)]);
    Exit;
   End;

  Exit;
 End;

 (* ===== arrays ===== *)
 Index := 0;

 { push indexes onto the stack }
 While (Left^.Typ <> mtVariable) do
 Begin
  TypeID := Parse(Left^.Right);
  With Compiler do // array subscript must be an integer value
   if (not isTypeInt(TypeID)) Then
    Error(eInvalidArraySubscript, [getTypeName(Variable.Typ), getTypeName(TypeID)]);

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
   if (not CompareTypes(TypeID, Variable.Typ)) Then
    Error(eWrongTypeInAssign, [Variable.Name, getTypeName(TypeID), getTypeName(Variable.Typ)]);
  End;

  { set new pointer }
  __variable_setvalue_reg(Variable, 1, Compiler.getTypePrefix(TypeID));

  Exit;
 End;

 { value assignment }
 DimCount := Compiler.TypeTable[Variable.Typ].ArrayDimCount;
 if (Index <> DimCount) Then
 Begin
  // special case: strings
  if (Index = DimCount-1) and (Compiler.isTypeString(Variable.Typ)) Then
  Begin
  End Else
   Error(eInvalidArrayAssign, []);
 End;

 TypeID := Parse(Right, 1); // this value will be saved into the array
 RePop(Right, TypeID, 1);

 { type check }
 With Compiler do
 Begin
  TmpType := getArrayBaseType(Variable.Typ);

  ShouldFail := False;

  if ((isTypeString(TmpType) and (Integer(Compiler.TypeTable[TmpType].ArrayDimCount)-Index < 0))) Then
  Begin
   ShouldFail := not isTypeChar(TypeID);
   TmpType    := TYPE_CHAR;
  End;

  {
   @Note: I think it needs a small explanation:

   Like you can see - I made a special case for strings; let's consider this code:

     var<string[]> tab = new string[10];
     str[1] = "Hello World!";
     str[1][3] = '_';

   Here we have an array alocation and two assignments.
   First assignment is a simple save `"Hello World!'` into the second item of the array, so it changes the whole element's value.
   ... but the second changes only a one char of that array's element's value.
   Since - for some specific internal reasons - the `TMType.ArrayBase` value of `string` type is equal `TYPE_STRING` (not `TYPE_CHAR`), that code would fail to compile.
   So we need to detect this situation and don't display error message.
   That's what this `if` above does.
  }

  if (not CompareTypes(TypeID, TmpType)) or (ShouldFail) Then//or (isTypeArray(TypeID) and not isTypeString(TypeID)) Then
   Error(eWrongTypeInAssign, [Variable.Name, getTypeName(TypeID), getTypeName(TmpType)]);
 End;

 { set new array's element's value }
 Compiler.PutOpcode(o_arset, [Variable.PosStr, Index, 'e'+Compiler.getTypePrefix(TypeID)+'1']);
 Dec(PushedValues, Index);
End;
