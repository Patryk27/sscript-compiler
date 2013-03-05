Procedure __constant_folding(const ErrorOnInvalidOperator: Boolean);

{ isKnown }
Function isKnown(Expr: PMExpression): Boolean;
Begin
 if (Expr = nil) Then
  Exit(False);

 Result := Expr^.Typ in [mtBool, mtChar, mtInt, mtFloat, mtString];
End;

{ Parse }
Procedure Parse(Expr: PMExpression);
Var Left, Right: PMExpression;
    Evaluated  : Boolean;
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

 { binary operators }
 if (isKnown(Left) and isKnown(Right)) Then
 Begin
  if (Left^.Typ <> Right^.Typ) Then
  Begin
   if (Left^.Typ = mtInt) and (Right^.Typ = mtFloat) Then // int, float -> float, float
    Left^.Typ := mtFloat Else
   if (Left^.Typ = mtFloat) and (Right^.Typ = mtInt) Then // float, int -> float, float
    Right^.Typ := mtFloat Else
    Exit; // wrong types
  End;

  Evaluated := False;

  if (Left^.Typ in [mtInt, mtFloat]) Then // int and float
  Begin
   Evaluated := Expr^.Typ in [mtAdd, mtSub, mtMul, mtDiv];

   Case Expr^.Typ of
    mtAdd: Expr^.Value := Left^.Value + Right^.Value;
    mtSub: Expr^.Value := Left^.Value - Right^.Value;
    mtMul: Expr^.Value := Left^.Value * Right^.Value;
    mtDiv:
     if (Right^.Value = 0) Then
      Compiler.CompileError(eDivByZero) Else
      Expr^.Value := Left^.Value / Right^.Value;
   End;
  End;

  if (Left^.Typ = mtString) Then // string
  Begin
   Evaluated := Expr^.Typ in [mtAdd];

   Case Expr^.Typ of
    mtAdd: Expr^.Value := Left^.Value + Right^.Value;
   End;
  End;

  if (Evaluated) Then
  Begin
   Expr^.Left  := nil;
   Expr^.Right := nil;

   Dispose(Left);
   Dispose(Right);

   Expr^.Typ := Left^.Typ;
  End Else
   if (ErrorOnInvalidOperator) Then
    Compiler.CompileError(Expr^.Token, eUnsupportedOperator, [MExpressionDisplay[Left^.Typ], MExpressionDisplay[Expr^.Typ], MExpressionDisplay[Right^.Typ]]);
 End;

 { unary operators }
 if (isKnown(Left)) Then
 Begin
  if (Expr^.Typ = mtNeg) Then
  Begin
   if (Left^.Typ in [mtInt, mtFloat]) Then
   Begin
    Expr^.Left  := nil;
    Expr^.Value := -Left^.Value;
    Expr^.Typ   := Left^.Typ;

    Dispose(Left);
   End Else
    if (ErrorOnInvalidOperator) Then
     Compiler.CompileError(Expr^.Token, eUnsupportedUOperator, [MExpressionDisplay[Expr^.Typ], MExpressionDisplay[Left^.Typ]]);
  End;
 End;
End;

Begin
 Parse(Tree);
End;
