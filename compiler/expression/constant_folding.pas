/// @TODO: int overflow

Procedure __constant_folding(const ErrorOnInvalidOperator: Boolean);

{ isConst }
Function isConst(Expr: PMExpression): Boolean;
Begin
 if (Expr = nil) Then
  Exit(False);

 Result := Expr^.Typ in [mtBool, mtChar, mtInt, mtFloat, mtString];
End;

{ Parse }
Procedure Parse(Expr: PMExpression);
Var Left, Right: PMExpression;
    Evaluated  : Boolean = False;
    I          : Integer;
Begin
 if (Expr = nil) Then // nothing to optimize
  Exit;

 Left  := Expr^.Left;
 Right := Expr^.Right;

 if (Left = nil) and (Right = nil) Then // nothing to optimize
  Exit;

 Parse(Left);
 Parse(Right);

 For I := Low(Expr^.ParamList) To High(Expr^.ParamList) Do
  Parse(Expr^.ParamList[I]);

 (* binary operators *)
 if (isConst(Left) and isConst(Right)) Then
 Begin
  if (Left^.Typ <> Right^.Typ) Then
  Begin
   if (Left^.Typ = mtInt) and (Right^.Typ = mtFloat) Then // int, float -> float, float
    Left^.Typ := mtFloat Else
   if (Left^.Typ = mtFloat) and (Right^.Typ = mtInt) Then // float, int -> float, float
    Right^.Typ := mtFloat Else
    Exit; // wrong types
  End;

  { bool }
  if (Left^.Typ = mtBool) Then
  Begin
   Evaluated := True;

   Case Expr^.Typ of
    mtXOR        { ^ } : Expr^.Value := Left^.Value xor Right^.Value;
    mtLogicalOR  { || }: Expr^.Value := Left^.Value or Right^.Value;
    mtLogicalAND { && }: Expr^.Value := Left^.Value and Right^.Value;

    else
     Evaluated := False;
   End;
  End;

  { int }
  if (Left^.Typ = mtInt) Then
  Begin
   Evaluated := True;

   Case Expr^.Typ of
    mtMod { % }:
     if (Right^.Value = 0) Then
      Compiler.CompileError(Expr^.Token, eDivByZero, []) Else
      Expr^.Value := Left^.Value mod Right^.Value;

    mtSHL { << }: Expr^.Value := Left^.Value shl Right^.Value;
    mtSHR { >> }: Expr^.Value := Left^.Value shr Right^.Value;
    mtXOR { ^ } : Expr^.Value := Left^.Value xor Right^.Value;

    mtBitwiseOR  { | }: Expr^.Value := Left^.Value or Right^.Value;
    mtBitwiseAND { & }: Expr^.Value := Left^.Value and Right^.Value;

    else
     Evaluated := False;
   End;
  End;

  { int or float }
  if (Left^.Typ in [mtInt, mtFloat]) and (not Evaluated) Then
  Begin
   Evaluated := True;

   Case Expr^.Typ of
    mtAdd { + }: Expr^.Value := Left^.Value + Right^.Value;
    mtSub { - }: Expr^.Value := Left^.Value - Right^.Value;
    mtMul { * }: Expr^.Value := Left^.Value * Right^.Value;
    mtDiv { / }:
     if (Right^.Value = 0) Then
      Compiler.CompileError(Expr^.Token, eDivByZero, []) Else
      Expr^.Value := Left^.Value / Right^.Value;

    else
     Evaluated := False;
   End;
  End;

  { string }
  if (Left^.Typ = mtString) Then
  Begin
   Evaluated := True;

   Case Expr^.Typ of
    mtAdd { + }: Expr^.Value := Left^.Value + Right^.Value;

    else
     Evaluated := False;
   End;
  End;

  if (Evaluated) Then
  Begin
   Expr^.Typ   := Left^.Typ;
   Expr^.Left  := nil;
   Expr^.Right := nil;

   Dispose(Left);
   Dispose(Right);
  End Else

   if (ErrorOnInvalidOperator) Then
    Compiler.CompileError(Expr^.Token, eUnsupportedOperator, [MExpressionDisplay[Left^.Typ], MExpressionDisplay[Expr^.Typ], MExpressionDisplay[Right^.Typ]]);
 End;

 (* unary operators *)
 if (isConst(Left)) and
    (Expr^.Typ in [mtNeg, mtLogicalNOT, mtBitwiseNOT]) Then
 Begin
  if (Expr^.Typ = mtNeg) Then { - }
  Begin
   { bool, int, float }
   if (Left^.Typ in [mtBool, mtInt, mtFloat]) Then
   Begin
    Evaluated   := True;
    Expr^.Value := -Left^.Value;
   End;
  End;

  { bool }
  if (Left^.Typ = mtBool) and (not Evaluated) Then
  Begin
   Evaluated := True;

   Case Expr^.Typ of
    mtLogicalNOT { ! }: Expr^.Value := not Left^.Value;

    else
     Evaluated := False;
   End;
  End;

  { int }
  if (Left^.Typ = mtInt) and (not Evaluated) Then
  Begin
   Evaluated := True;

   Case Expr^.Typ of
    mtBitwiseNOT { ~ }: Expr^.Value := not Left^.Value;

    else
     Evaluated := False;
   End;
  End;

  if (Evaluated) Then
  Begin
   Expr^.Left := nil;
   Expr^.Typ  := Left^.Typ;

   Dispose(Left);
  End Else

   if (ErrorOnInvalidOperator) Then
    Compiler.CompileError(Expr^.Token, eUnsupportedUOperator, [MExpressionDisplay[Expr^.Typ], MExpressionDisplay[Left^.Typ]]);
 End;
End;

Begin
 Parse(Tree);
End;
