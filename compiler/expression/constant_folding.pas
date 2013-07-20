/// @TODO: int overflow

Procedure __constant_folding(const ErrorOnInvalidOperator: Boolean);

(* Parse *)
Procedure Parse(var Expr: PExpressionNode);
Var Tmp, Tmp2, Left, Right: PExpressionNode;
    Evaluated             : Boolean = False;
    I                     : Integer;
Begin
 if (Expr = nil) Then // nothing to do
  Exit;

 Left  := Expr^.Left;
 Right := Expr^.Right;

 if (Left = nil) and (Right = nil) Then // nothing to be done
  Exit;

 Parse(Left);
 Parse(Right);

 For I := Low(Expr^.ParamList) To High(Expr^.ParamList) Do
  Parse(Expr^.ParamList[I]);

 if (Left <> nil) and (Left^.Typ = mtIdentifier) and (Left^.Value <> null) and (not (Expr^.Typ in MLValueOperators)) Then
 Begin
  Left^.Typ   := Left^.IdentType;
  Left^.Left  := nil;
  Left^.Right := nil;
 End;

 if (Right <> nil) and (Right^.Typ = mtIdentifier) and (Right^.Value <> null) Then
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

//   Dispose(Left);
//   Dispose(Right);
  End Else

   if (ErrorOnInvalidOperator) Then
    Compiler.CompileError(Expr^.Token, eUnsupportedOperator, [ExpressionNodeString[Left^.Typ], ExpressionNodeString[Expr^.Typ], ExpressionNodeString[Right^.Typ]]);
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

//   Dispose(Left);
  End Else

   if (ErrorOnInvalidOperator) Then
    Compiler.CompileError(Expr^.Token, eUnsupportedUOperator, [ExpressionNodeString[Expr^.Typ], ExpressionNodeString[Left^.Typ]]);
 End;

 // ----------------------------------------- //

 { try to compute expressions like `x *= 5;` at compile-time (if `x` is known) }
 For I := Low(Simplify1Data) To High(Simplify1Data) Do
  if (Expr^.Typ = Simplify1Data[I].Post) Then
  Begin
   New(Tmp);
   Tmp^ := Expr^;

   New(Left);
   Left^ := Expr^.Left^;

   New(Right);
   Right^ := Expr^.Right^;

   Expr^.Typ := Simplify1Data[I].Pre;
   Parse(Expr);

   if (Expr^.Left = nil) and (Expr^.Right = nil) Then // if optimization succeeded
   Begin
    Left^.SSA := Left^.PostSSA;

    New(Tmp2);
    Tmp2^.Left  := Left;
    Tmp2^.Right := Expr;
    Tmp2^.Token := Expr^.Token;
    Tmp2^.Typ   := mtAssign;

    Expr := Tmp2;
   End Else
   Begin // couldn't optimize
    Expr^.Typ   := Simplify1Data[I].Post;
    Expr^.Left  := Left;
    Expr^.Right := Right;
   End;
  End;

 AnyChange := AnyChange or Evaluated;
End;

Begin
 if (Tree <> nil) and (Tree^.Left = nil) and (Tree^.Right = nil) and (Tree^.Typ = mtIdentifier) and (Tree^.Value <> null) Then // @TODO: this is ugly
  Tree^.Typ := Tree^.IdentType;

 Parse(Tree);
End;
