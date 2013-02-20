Function CompileSimple(out TypeLeft, TypeRight: TVType; const isLeftVariable: Boolean=False): TVType;
Var Variable   : TRVariable;
    LeftFirst  : Boolean=False;
    isComparing: Boolean;
Begin
 isComparing := Expr^.Typ in [mtEqual, mtDifferent, mtGreater, mtLower, mtGreaterEqual, mtLowerEqual];

 if (isLeftVariable) Then
 Begin
  Variable := getVariable(Left, False);

  TypeLeft  := Variable.Typ;
  TypeRight := Parse(Right, 2);
 End Else
 Begin
  if (countLeaves(Right) >= countLeaves(Left)) Then
  Begin
   LeftFirst := False;
   TypeRight := Parse(Right, 2); // right to second register
   TypeLeft  := Parse(Left, 1); // left to first register
  End Else
  Begin
   LeftFirst := True;
   TypeLeft  := Parse(Left, 1); // left to first register
   TypeRight := Parse(Right, 2); // right to second register
  End;
 End;

 // we must 'pop' result back to the corresponding register
 if (LeftFirst) Then
 Begin
  RePop(Right, TypeRight, 2);
  RePop(Left, TypeLeft, 1);
 End Else
 Begin
  RePop(Left, TypeLeft, 1);
  RePop(Right, TypeRight, 2);
 End;

 With Compiler do
 Begin
  { cast table }
  if (isTypeArray(TypeLeft, False) or isTypeArray(TypeRight, False)) Then // don't check arrays (except pure strings)
   Exit(TypeLeft);

  { float, int -> float, float }
  if (isTypeFloat(TypeLeft)) and (isTypeInt(TypeRight)) Then
  Begin
   PutOpcode(o_mov, ['e'+getTypePrefix(TypeLeft)+'2', 'e'+getTypePrefix(TypeRight)+'2']);
   TypeRight := TYPE_FLOAT;
  End;

  { int, float -> float, float }
  if (isTypeInt(TypeLeft)) and (isTypeFloat(TypeRight)) Then
  Begin
   PutOpcode(o_mov, ['e'+getTypePrefix(TypeRight)+'1', 'e'+getTypePrefix(TypeLeft)+'1']);
   TypeLeft := TYPE_FLOAT;
  End;

  { string, char -> string, string }
  if (isTypeString(TypeLeft)) and (isTypeChar(TypeRight)) and (not isComparing) Then
  Begin
   if (isTypeInt(TypeRight)) Then
   Begin
    PutOpcode(o_mov, ['e'+getTypePrefix(TYPE_CHAR)+'2', 'e'+getTypePrefix(TypeRight)+'2']);
    TypeRight := TYPE_CHAR;
   End;

   PutOpcode(o_mov, ['e'+getTypePrefix(TypeLeft)+'2', 'e'+getTypePrefix(TypeRight)+'2']);
   TypeRight := TYPE_STRING;
  End;

  { char, string -> string, string }
  if (isTypeChar(TypeLeft)) and (isTypeString(TypeRight)) and (not isComparing) Then
  Begin
   if (isTypeInt(TypeRight)) Then
   Begin
    PutOpcode(o_mov, ['e'+getTypePrefix(TYPE_CHAR)+'1', 'e'+getTypePrefix(TypeLeft)+'1']);
    TypeRight := TYPE_CHAR;
   End;

   PutOpcode(o_mov, ['e'+getTypePrefix(TypeRight)+'1', 'e'+getTypePrefix(TypeLeft)+'1']);
   TypeLeft := TYPE_STRING;
  End;

  { char, int -> int, int }
  if (isTypeChar(TypeLeft)) and (isTypeInt(TypeRight)) Then
  Begin
   PutOpcode(o_mov, ['e'+getTypePrefix(TypeRight)+'1', 'e'+getTypePrefix(TypeLeft)+'1']);
   TypeLeft := TYPE_INT;
  End;

  { int, char -> int, int }
  if (isTypeInt(TypeLeft)) and (isTypeChar(TypeRight)) Then
  Begin
   PutOpcode(o_mov, ['e'+getTypePrefix(TypeLeft)+'1', 'e'+getTypePrefix(TypeRight)+'1']);
   TypeLeft := TYPE_INT;
  End;

  { unsupported operator (eg.`int+string`) }
  if (not CompareTypes(TypeLeft, TypeRight)) or
     (isTypeFunctionPointer(TypeLeft) or isTypeFunctionPointer(TypeRight)) Then
  Begin
   Error(eUnsupportedOperator, [getTypeName(TypeLeft), getDisplay(Expr), getTypeName(TypeRight)]);
   Exit;
  End;
 End;

 Result := TypeLeft;
End;
