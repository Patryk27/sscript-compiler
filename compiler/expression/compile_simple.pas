Function CompileSimple(out TypeLeft, TypeRight: TType; const isLeftVariable: Boolean=False): TType;
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
  { type-promotion table }
  if (TypeLeft.isArray(False) or TypeRight.isArray(False)) Then // don't check arrays (except pure strings)
   Exit(TypeLeft);

  { float, int -> float, float }
  if (TypeLeft.isFloat) and (TypeRight.isInt) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeLeft.RegPrefix+'2', 'e'+TypeRight.RegPrefix+'2']);
   TypeRight := TYPE_FLOAT;
  End;

  { int, float -> float, float }
  if (TypeLeft.isInt) and (TypeRight.isFloat) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeRight.RegPrefix+'1', 'e'+TypeLeft.RegPrefix+'1']);
   TypeLeft := TYPE_FLOAT;
  End;

  { pointer, int -> int, int }
  if (TypeLeft.isFunctionPointer) and (TypeRight.isInt) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeRight.RegPrefix+'1', 'e'+TypeLeft.RegPrefix+'1']);
   TypeLeft := TYPE_INT;
  End;

  { int, pointer -> int, int }
  if (TypeLeft.isInt) and (TypeRight.isFunctionPointer) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeLeft.RegPrefix+'2', 'e'+TypeRight.RegPrefix+'2']);
   TypeRight := TYPE_INT;
  End;

  { bool, int -> int, int }
  if (TypeLeft.isBool) and (TypeRight.isInt) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeRight.RegPrefix+'1', 'e'+TypeLeft.RegPrefix+'1']);
   TypeLeft := TYPE_INT;
  End;

  { int, bool -> int, int }
  if (TypeLeft.isInt) and (TypeRight.isBool) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeLeft.RegPrefix+'2', 'e'+TypeRight.RegPrefix+'2']);
   TypeRight := TYPE_INT;
  End;

  { string, char -> string, string }
  if (TypeLeft.isString) and (TypeRight.isChar) and (not isComparing) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeLeft.RegPrefix+'2', 'e'+TypeRight.RegPrefix+'2']);
   TypeRight := TYPE_STRING;
  End;

  { char, string -> string, string }
  if (TypeLeft.isChar) and (TypeRight.isString) and (not isComparing) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeRight.RegPrefix+'1', 'e'+TypeLeft.RegPrefix+'1']);
   TypeLeft := TYPE_STRING;
  End;

  { char, int -> int, int }
  if (TypeLeft.isChar) and (TypeRight.isInt) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeRight.RegPrefix+'1', 'e'+TypeLeft.RegPrefix+'1']);
   TypeLeft := TYPE_INT;
  End;

  { int, char -> int, int }
  if (TypeLeft.isInt) and (TypeRight.isChar) Then
  Begin
   PutOpcode(o_mov, ['e'+TypeLeft.RegPrefix+'1', 'e'+TypeRight.RegPrefix+'1']);
   TypeLeft := TYPE_INT;
  End;

  { unsupported operator (eg.`int+string`) }
  if (not TypeRight.CanBeAssignedTo(TypeLeft)) Then
  Begin
   Error(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]);
   Exit;
  End;
 End;

 Result := TypeLeft;
End;
