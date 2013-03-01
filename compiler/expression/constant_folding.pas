Procedure __constant_folding;

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
Begin
 if (Expr = nil) Then // nothing to optimize
  Exit;

 Left  := Expr^.Left;
 Right := Expr^.Right;

 if (Left = nil) and (Right = nil) Then // nothing to optimize
  Exit;

 Parse(Left);
 Parse(Right);

 // are values known?
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
  End;
 End;

 // unary operators
 if (Expr^.Typ = mtNeg) Then
 Begin
  if (Left^.Typ in [mtInt, mtFloat]) Then
  Begin
   Expr^.Left  := nil;
   Expr^.Value := -Left^.Value;
   Expr^.Typ   := Left^.Typ;

   Dispose(Left);
  End;
 End;
End;

Begin
 Parse(Tree);
End;
