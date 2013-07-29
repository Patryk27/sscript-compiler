Procedure __tree_simplification(const ShowErrors: Boolean);

{ isVariable }
Function isVariable(const Node: PExpressionNode): Boolean;
Begin
 Result := (Node <> nil) and (Node^.Typ = mtIdentifier) and (Node^.Symbol <> nil);
End;

{ isVariableOrCall }
Function isVariableOrCall(const Node: PExpressionNode): Boolean;
Begin
 Result := (isVariable(Node) or (Node^.Typ in [mtFunctionCall, mtMethodCall]));
End;

{ isEqual }
Function isEqual(const Node: PExpressionNode; const Value: Int64): Boolean;
Begin
 Result := (Node^.Typ in [mtInt, mtFloat]) and (Node^.Value = Value);

 if (Node^.Typ = mtNeg) and (Value < 0) Then
  Result := isEqual(Node^.Left, -Value);
End;

{ TryToOptimizeSum }
Procedure TryToOptimizeSum(const Expr: PExpressionNode);
Var Count: Integer = 0;
    Can  : Boolean = True;

    // Visit
    Procedure Visit(const VisitExpr: PExpressionNode);
    Var Left, Right: PExpressionNode;
    Begin
     if (VisitExpr = nil) Then
      Exit;

     Left  := VisitExpr^.Left;
     Right := VisitExpr^.Right;

     if (VisitExpr^.Typ = mtAdd) Then
     Begin
      if (Left^.Typ = mtAdd) Then
       Visit(Left) Else
      if (Left^.Typ = mtIdentifier) Then
      Begin
       if (Left^.Symbol = Expr^.Right^.Symbol) Then
        Inc(Count) Else
        Can := False;
      End Else
       Can := False;

      if (Right^.Typ = mtAdd) Then
       Visit(Right) Else
      if (Right^.Typ = mtIdentifier) Then
      Begin
       if (Right^.Symbol = Expr^.Right^.Symbol) Then
        Inc(Count) Else
        Can := False;
      End Else
       Can := False;
     End;
    End;

Begin
 Visit(Expr);

 if (Can) and (Count > 0) Then
 Begin
  Expr^.Typ  := mtMul;
  Expr^.Left := MakeIntExpression(Count);

  AnyChange := True;
 End;
End;

// -------------------------------------------------------------------------- //
(* Parse *)
Procedure Parse(var Expr: PExpressionNode);
Var Tmp, Left, Right, Param: PExpressionNode;
    Simplify1              : TSimplify1Data;
Begin
 if (Expr = nil) Then
  Exit;

 Left  := Expr^.Left;
 Right := Expr^.Right;

 { 'expr + 0' -> 'expr' }
 { 'expr - 0' -> 'expr' }
 if (Expr^.Typ in [mtAdd, mtSub]) and (isEqual(Right, 0)) Then
 Begin
  Expr      := Expr^.Left;
  AnyChange := True;
 End;

 { '0 + expr' -> 'expr' }
 if (Expr^.Typ = mtAdd) and (isEqual(Left, 0)) Then
 Begin
  Expr      := Expr^.Right;
  AnyChange := True;
 End;

 { '0 - expr' -> '-expr' }
 if (Expr^.Typ = mtSub) and (isEqual(Left, 0)) Then
 Begin
  Expr^.Typ  := mtNeg;
  Expr^.Left := nil;
  AnyChange  := True;
 End;

 { 'x - x' -> '0' }
 if (Expr^.Typ = mtSub) and (isVariable(Left)) and (isVariable(Right)) and (Left^.Symbol = Right^.Symbol) Then
 Begin
  Expr      := MakeIntExpression(0, @Expr^.Token);
  AnyChange := True;
 End;

 { '(-x)+y' -> 'y-x' }
 if (Expr^.Typ = mtAdd) and (Left^.Typ = mtNeg) and (isVariableOrCall(Left^.Left)) and (isVariableOrCall(Right)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ   := mtSub;
  Tmp^.Left  := Right;
  Tmp^.Right := Left^.Left;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'x+(-y)' -> 'x-y' }
 if (Expr^.Typ = mtAdd) and (Right^.Typ = mtNeg) and (isVariableOrCall(Right^.Left)) and (isVariableOrCall(Left)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ   := mtSub;
  Tmp^.Left  := Left;
  Tmp^.Right := Right^.Left;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'expr*1' -> 'expr' }
 if (Expr^.Typ = mtMul) and (isEqual(Right, 1)) Then
 Begin
  Expr      := Left;
  AnyChange := True;
 End;

 { '1*expr' -> 'expr' }
 if (Expr^.Typ = mtMul) and (isEqual(Left, 1)) Then
 Begin
  Expr      := Right;
  AnyChange := True;
 End;

 { 'expr*(-1)' -> '-expr' }
 if (Expr^.Typ = mtMul) and (isEqual(Right, -1)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ  := mtNeg;
  Tmp^.Left := Left;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { '(-1)*expr' -> '-expr' }
 if (Expr^.Typ = mtMul) and (isEqual(Left, -1)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ  := mtNeg;
  Tmp^.Left := Right;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'expr/1' -> 'expr' }
 if (Expr^.Typ = mtDiv) and (isEqual(Right, 1)) Then
 Begin
  Expr      := Expr^.Left;
  AnyChange := True;
 End;

 { 'expr/(-1)' -> '-expr' }
 if (Expr^.Typ = mtDiv) and (isEqual(Right, -1)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ  := mtNeg;
  Tmp^.Left := Left;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'i+i+i+i+....+i' -> 'i * x' }
 if (Expr^.Typ = mtAdd) and (isVariable(Right)) Then
  TryToOptimizeSum(Expr);

 { 'x*i - i' -> 'i * (x-1)' }
 if (Expr^.Typ = mtSub) and (Left^.Typ = mtMul) and (Left^.Left^.HasValue) and (Left^.Right^.hasValue) and (Right^.hasValue) and
    (isVariable(Left^.Right)) and (isVariable(Right)) and (Left^.Right^.Symbol = Right^.Symbol) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ   := mtMul;
  Tmp^.Left  := Left^.Right;
  Tmp^.Right := EmptyExpression;

  Tmp^.Right^.Typ   := mtSub;
  Tmp^.Right^.Left  := Left^.Left;
  Tmp^.Right^.Right := MakeIntExpression(1);

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'i*x + i' -> 'i * (x+1)' }
 { 'i*x - i' -> 'i * (x-1)' }
 if (Expr^.Typ in [mtAdd, mtSub]) and (Left^.Typ = mtMul) and (Left^.Left^.hasValue) and (Left^.Right^.hasValue) and (Right^.hasValue) and
    (isVariable(Left^.Left)) and (isVariable(Right)) and (Left^.Left^.Symbol = Right^.Symbol) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ   := mtMul;
  Tmp^.Left  := Left^.Left;
  Tmp^.Right := EmptyExpression(@Right^.Token);

  Tmp^.Right^.Typ   := Expr^.Typ;
  Tmp^.Right^.Left  := Left^.Right;
  Tmp^.Right^.Right := MakeIntExpression(1);

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'i*x + i*y' -> 'i * (x+y)' }
 { 'i*x - i*y' -> 'i * (x-y)' }

 { 'x*i + y*i' -> 'i * (x+y)' }
 { 'x*i - y*i' -> 'i * (x-y)' }

 { 'i*x + y*i' -> 'i * (x+y)' }
 { 'i*x - y*i' -> 'i * (x-y)' }

 { 'x*i + i*y' -> 'i * (x+y)' }
 { 'x*i - i*y' -> 'i * (x-y)' }
 if (Expr^.Typ in [mtAdd, mtSub]) and
    (Left^.Typ = mtMul) and (Left^.Left^.hasValue) and (Left^.Right^.hasValue) and
    (Right^.Typ = mtMul) and (Right^.Left^.hasValue) and (Right^.Right^.hasValue) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ        := mtMul;
  Tmp^.Right      := EmptyExpression(@Right^.Token);
  Tmp^.Right^.Typ := Expr^.Typ;

  if (isVariable(Left^.Left)) and (isVariable(Right^.Left)) and (Left^.Left^.Symbol = Right^.Left^.Symbol) Then // first and second case
  Begin
   Tmp^.Left         := Left^.Left;
   Tmp^.Right^.Left  := Left^.Right;
   Tmp^.Right^.Right := Right^.Right;

   Expr      := Tmp;
   AnyChange := True;
  End;

  if (isVariable(Left^.Right)) and (isVariable(Right^.Right)) and (Left^.Right^.Symbol = Right^.Right^.Symbol) Then // third and fourth case
  Begin
   Tmp^.Left         := Left^.Right;
   Tmp^.Right^.Left  := Left^.Left;
   Tmp^.Right^.Right := Right^.Left;

   Expr      := Tmp;
   AnyChange := True;
  End;

  if (isVariable(Left^.Left)) and (isVariable(Right^.Right)) and (Left^.Left^.Symbol = Right^.Right^.Symbol) Then // fifth and sixth case
  Begin
   Tmp^.Left         := Left^.Left;
   Tmp^.Right^.Left  := Left^.Right;
   Tmp^.Right^.Right := Right^.Left;

   Expr      := Tmp;
   AnyChange := True;
  End;

  if (isVariable(Left^.Right)) and (isVariable(Right^.Left)) and (Left^.Right^.Symbol = Right^.Left^.Symbol) Then // seventh and eighth case
  Begin
   Tmp^.Left         := Left^.Right;
   Tmp^.Right^.Left  := Left^.Left;
   Tmp^.Right^.Right := Right^.Right;

   Expr      := Tmp;
   AnyChange := True;
  End;
 End;

 { 'i = i' -> nothing }
 if (Expr^.Typ = mtAssign) and (isVariable(Left)) and (isVariable(Right)) and (Right^.Symbol = Left^.Symbol) Then
 Begin
  Expr      := nil;
  AnyChange := True;
  Exit;
 End;

 { 'i+i+i+i+....+i' -> 'i * x' }
 if (Expr^.Typ = mtAdd) and (isVariable(Right)) Then
  TryToOptimizeSum(Expr);

 { 'x = x op expr' -> 'x op= expr' }
 For Simplify1 in Simplify1Data Do
  if (Expr^.Typ = mtAssign) and (isVariable(Left)) and
     (Right^.Typ = Simplify1.Pre) and (Right^.Left^.Symbol = Left^.Symbol) Then
  Begin
   Expr^.Typ   := Simplify1.Post;
   Expr^.Right := Right^.Right;
   AnyChange   := True;
   Break;
  End;

 { 'x = expr op x' -> 'x op= expr' }
 For Simplify1 in Simplify2Data Do
  if (Expr^.Typ = mtAssign) and (isVariable(Left)) and
     (Right^.Typ = Simplify1.Pre) and (Right^.Right^.Symbol = Left^.Symbol) and (Right^.Left^.hasValue) Then
  Begin
   Expr^.Typ   := Simplify1.Post;
   Expr^.Right := Right^.Left;
   AnyChange   := True;
   Break;
  End;

 { 'x += x' -> 'x *= 2' }
 if (Expr^.Typ = mtAddEq) and (isVariable(Left)) and (Right^.Left <> nil) and (Left^.Symbol = Right^.Left^.Symbol) Then
 Begin
  Expr^.Typ   := mtMulEq;
  Expr^.Right := MakeIntExpression(2);

  AnyChange := True;
 End;

 Parse(Expr^.Left);
 Parse(Expr^.Right);
 For Param in Expr^.ParamList Do
  Parse(Param);
End;

Var Tmp: Boolean;
Begin
 Tmp := AnyChange;

 Repeat
  AnyChange := False;

  if (AnyChange) Then
   Tmp := True;

  Parse(Tree);
 Until (not AnyChange);

 AnyChange := Tmp;
End;
