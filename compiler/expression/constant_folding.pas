/// @TODO: int overflow

Procedure __constant_folding(const ErrorOnInvalidOperator: Boolean);

(* Parse *)
Procedure Parse(Expr: PExpression);
Var Left, Right: PExpression;
    Evaluated  : Boolean = False;
    I          : Integer;
Begin
 if (Expr = nil) Then // nothing to do
  Exit;

 Left  := Expr^.Left;
 Right := Expr^.Right;

 if (Left = nil) and (Right = nil) Then // nothing to do
  Exit;

 Parse(Left);
 Parse(Right);

 For I := Low(Expr^.ParamList) To High(Expr^.ParamList) Do
  Parse(Expr^.ParamList[I]);

 if (Left <> nil) and (Left^.Typ = mtVariable) and (Left^.Value <> null) and (not (Expr^.Typ in [mtAssign, mtPreInc, mtPostInc, mtPreDec, mtPostDec, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtShlEq, mtShrEq])) Then
 Begin
  Left^.Typ   := Left^.IdentType;
  Left^.Left  := nil;
  Left^.Right := nil;
 End;

 if (Right <> nil) and (Right^.Typ = mtVariable) and (Right^.Value <> null) Then
 Begin
  Right^.Typ   := Right^.IdentType;
  Right^.Left  := nil;
  Right^.Right := nil;
 End;

 (* binary operators *)
 if (Left^.isConstant and Right^.isConstant) Then
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

    mtLower        { <  }: Expr^.Value := Left^.Value < Right^.Value;
    mtLowerEqual   { <= }: Expr^.Value := Left^.Value <= Right^.Value;
    mtEqual        { == }: Expr^.Value := Left^.Value = Right^.Value;
    mtGreater      { > }: Expr^.Value := Left^.Value > Right^.Value;
    mtGreaterEqual { >= }: Expr^.Value := Left^.Value >= Right^.Value;

    else
     Evaluated := False;
   End;

   if (Expr^.Typ in [mtLower, mtLowerEqual, mtEqual, mtGreater, mtGreaterEqual]) Then
    Left^.Typ := mtBool;
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
    Compiler.CompileError(Expr^.Token, eUnsupportedOperator, [ExpressionDisplay[Left^.Typ], ExpressionDisplay[Expr^.Typ], ExpressionDisplay[Right^.Typ]]);
 End;

 (* unary operators *)
 if (Left^.isConstant) and
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
    Compiler.CompileError(Expr^.Token, eUnsupportedUOperator, [ExpressionDisplay[Expr^.Typ], ExpressionDisplay[Left^.Typ]]);
 End;
End;

Begin
 if (Tree <> nil) and (Tree^.Left = nil) and (Tree^.Right = nil) and (Tree^.Typ = mtVariable) and (Tree^.Value <> null) Then // @TODO: this is ugly
 Begin
  Tree^.Typ := Tree^.IdentType;
 End;

 Parse(Tree);
End;
