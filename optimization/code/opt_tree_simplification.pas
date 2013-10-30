Procedure __tree_simplification(const ShowErrors: Boolean);

{ isVariable }
Function isVariable(const Node: PExpressionNode): Boolean;
Begin
 Result := (Node <> nil) and (Node^.Typ = mtIdentifier) and (Node^.Symbol <> nil) and (TSymbol(Node^.Symbol).Typ = stVariable);
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

{ isEqual }
Function isEqual(const Node: PExpressionNode; const Value: Boolean): Boolean;
Begin
 Result := (Node^.Typ = mtBool) and (Node^.Value = Value);
End;

{ isNumeric }
Function isNumeric(const Node: PExpressionNode): Boolean;
Begin
 Result := (Node^.getType in [mtInt, mtFloat]);
End;

{ isNumericVar }
Function isNumericVar(const Symbol: TObject): Boolean;
Var Sym: TSymbol;
Begin
 if (Symbol = nil) Then
 Begin
  Result := False;
 End Else
 Begin
  Sym := (Symbol as TSymbol);

  if (Sym = nil) Then
   Exit(False);

  if (Sym.Typ <> stVariable) Then
   Exit(False);

  Result := Sym.mVariable.Typ.isNumerical;
 End;
End;

{ isNumericVar }
Function isNumericVar(const Expr: PExpressionNode): Boolean;
Begin
 Result := isNumericVar(Expr^.Symbol);
End;

// -------------------------------------------------------------------------- //
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
Var Tmp, Param: PExpressionNode;
    Simplify1 : TSimplify1Data;
    PowerOf2  : uint8;
Begin
 if (Expr = nil) Then
  Exit;

 { 'numexpr + 0' -> 'numexpr' }
 { 'numexpr - 0' -> 'numexpr' }
 if (Expr^.Typ in [mtAdd, mtSub]) and (isEqual(Expr^.Right, 0)) and (isNumeric(Expr^.Left)) Then
 Begin
  Expr      := Expr^.Left;
  AnyChange := True;
 End;

 { '0 + numexpr' -> 'numexpr' }
 if (Expr^.Typ = mtAdd) and (isEqual(Expr^.Left, 0)) and (isNumeric(Expr^.Right)) Then
 Begin
  Expr      := Expr^.Right;
  AnyChange := True;
 End;

 { '0 - numexpr' -> '-numexpr' }
 if (Expr^.Typ = mtSub) and (isEqual(Expr^.Left, 0)) and (isNumeric(Expr^.Left)) Then
 Begin
  Expr^.Typ   := mtNeg;
  Expr^.Left  := Expr^.Right;
  Expr^.Right := nil;
  AnyChange   := True;
 End;

 { 'numvar - numvar' -> '0' }
 if (Expr^.Typ = mtSub) and (isVariable(Expr^.Left)) and (isVariable(Expr^.Right)) and (Expr^.Left^.Symbol = Expr^.Right^.Symbol) and (isNumericVar(Expr^.Left^.Symbol)) Then
 Begin
  Expr      := MakeIntExpression(0, @Expr^.Token);
  AnyChange := True;
 End;

 { '(-numexprA)+numexprB' -> 'numexprB-numexprA' }
 if (Expr^.Typ = mtAdd) and (Expr^.Left^.Typ = mtNeg) and (isNumeric(Expr^.Left)) and (isNumeric(Expr^.Right)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ   := mtSub;
  Tmp^.Left  := Expr^.Right;
  Tmp^.Right := Expr^.Left^.Left;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'numexprA+(-numexprB)' -> 'numexprA-numexprB' }
 if (Expr^.Typ = mtAdd) and (Expr^.Right^.Typ = mtNeg) and (isNumeric(Expr^.Left)) and (isNumeric(Expr^.Right)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ   := mtSub;
  Tmp^.Left  := Expr^.Left;
  Tmp^.Right := Expr^.Right^.Left;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'numexpr*1' -> 'numexpr' }
 if (Expr^.Typ = mtMul) and (isEqual(Expr^.Right, 1)) and (isNumeric(Expr^.Left)) Then
 Begin
  Expr      := Expr^.Left;
  AnyChange := True;
 End;

 { '1*numexpr' -> 'numexpr' }
 if (Expr^.Typ = mtMul) and (isEqual(Expr^.Left, 1)) and (isNumeric(Expr^.Right)) Then
 Begin
  Expr      := Expr^.Right;
  AnyChange := True;
 End;

 { 'numexpr*(-1)' -> '-numexpr' }
 if (Expr^.Typ = mtMul) and (isEqual(Expr^.Right, -1)) and (isNumeric(Expr^.Left)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ  := mtNeg;
  Tmp^.Left := Expr^.Left;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { '(-1)*numexpr' -> '-numexpr' }
 if (Expr^.Typ = mtMul) and (isEqual(Expr^.Left, -1)) and (isNumeric(Expr^.Right)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ  := mtNeg;
  Tmp^.Left := Expr^.Right;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'numexpr/1' -> 'numexpr' }
 if (Expr^.Typ = mtDiv) and (isEqual(Expr^.Right, 1)) and (isNumeric(Expr^.Left)) Then
 Begin
  Expr      := Expr^.Left;
  AnyChange := True;
 End;

 { 'numexpr/(-1)' -> '-numexpr' }
 if (Expr^.Typ = mtDiv) and (isEqual(Expr^.Right, -1)) and (isNumeric(Expr^.Left)) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ  := mtNeg;
  Tmp^.Left := Expr^.Left;

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'numvar += 1' -> '++i' }
 if (Expr^.Typ = mtAddEq) and (isVariable(Expr^.Left)) and (isNumericVar(Expr^.Left^.Symbol)) and (isEqual(Expr^.Right, 1)) Then
 Begin
  Expr^.Typ   := mtPreInc;
  Expr^.Right := nil;
  AnyChange   := True;
 End;

 { 'numvar -= 1' -> '--i' }
 if (Expr^.Typ = mtSubEq) and (isVariable(Expr^.Left)) and (isNumericVar(Expr^.Left^.Symbol)) and (isEqual(Expr^.Right, 1)) Then
 Begin
  Expr^.Typ   := mtPreDec;
  Expr^.Right := nil;
  AnyChange   := True;
 End;

 { 'numvar+numvar+numvar+numvar+....+numvar' -> 'numvar * amount' }
 if (Expr^.Typ = mtAdd) and (isVariable(Expr^.Right)) Then
  TryToOptimizeSum(Expr);

 { 'numvarA*numvarB - numvarB' -> 'numvarB * (numvarA-1)' }
 if (Expr^.Typ = mtSub) and (Expr^.Left^.Typ = mtMul) and (Expr^.Left^.Left^.hasValue) and (Expr^.Left^.Right^.hasValue) and (Expr^.Right^.hasValue) and
    (isVariable(Expr^.Left^.Right)) and (isVariable(Expr^.Right)) and (isNumericVar(Expr^.Right^.Symbol)) and
    (Expr^.Left^.Right^.Symbol = Expr^.Right^.Symbol) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ   := mtMul;
  Tmp^.Left  := Expr^.Left^.Right;
  Tmp^.Right := EmptyExpression;

  Tmp^.Right^.Typ   := mtSub;
  Tmp^.Right^.Left  := Expr^.Left^.Left;
  Tmp^.Right^.Right := MakeIntExpression(1);

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'numvarA*numvarB + numvarA' -> 'numvarA * (numvarB+1)' }
 { 'numvarA*numvarB - numvarA' -> 'numvarA * (numvarB-1)' }
 if (Expr^.Typ in [mtAdd, mtSub]) and (Expr^.Left^.Typ = mtMul) and (Expr^.Left^.Left^.hasValue) and (Expr^.Left^.Right^.hasValue) and (Expr^.Right^.hasValue) and
    (isVariable(Expr^.Left^.Left)) and (isVariable(Expr^.Right)) and (isNumericVar(Expr^.Right^.Symbol)) and
    (Expr^.Left^.Left^.Symbol = Expr^.Right^.Symbol) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ   := mtMul;
  Tmp^.Left  := Expr^.Left^.Left;
  Tmp^.Right := EmptyExpression(@Expr^.Right^.Token);

  Tmp^.Right^.Typ   := Expr^.Typ;
  Tmp^.Right^.Left  := Expr^.Left^.Right;
  Tmp^.Right^.Right := MakeIntExpression(1);

  Expr      := Tmp;
  AnyChange := True;
 End;

 { 'numvarA*numvarB + numvarA*numvarC' -> 'numvarA * (numvarB+numvarC)' }
 { 'numvarA*numvarB - numvarA*numvarC' -> 'numvarA * (numvarB-numvarC)' }

 { 'numvarB*numvarA + numvarC*numvarA' -> 'numvarA * (numvarB+numvarC)' }
 { 'numvarB*numvarA - numvarC*numvarA' -> 'numvarA * (numvarB-numvarC)' }

 { 'numvarA*numvarB + numvarC*numvarA' -> 'numvarA * (numvarB+numvarC)' }
 { 'numvarA*numvarB - numvarC*numvarA' -> 'numvarA * (numvarB-numvarC)' }

 { 'numvarB*numvarA + numvarA*numvarC' -> 'numvarA * (numvarB+numvarC)' }
 { 'numvarB*numvarA - numvarA*numvarC' -> 'numvarA * (numvarB-numvarC)' }
 if (Expr^.Typ in [mtAdd, mtSub]) and
    (Expr^.Left^.Typ = mtMul) and (Expr^.Left^.Left^.hasValue) and (Expr^.Left^.Right^.hasValue) and
    (Expr^.Right^.Typ = mtMul) and (Expr^.Right^.Left^.hasValue) and (Expr^.Right^.Right^.hasValue) Then
 Begin
  Tmp := EmptyExpression(@Expr^.Token);

  Tmp^.Typ        := mtMul;
  Tmp^.Right      := EmptyExpression(@Expr^.Right^.Token);
  Tmp^.Right^.Typ := Expr^.Typ;

  if (isNumericVar(Expr^.Left^.Left)) and (isNumericVar(Expr^.Right^.Left)) and (Expr^.Left^.Left^.Symbol = Expr^.Right^.Left^.Symbol) Then // first and second case
  Begin
   Tmp^.Left         := Expr^.Left^.Left;
   Tmp^.Right^.Left  := Expr^.Left^.Right;
   Tmp^.Right^.Right := Expr^.Right^.Right;

   Expr      := Tmp;
   AnyChange := True;
  End;

  if (isNumericVar(Expr^.Left^.Right)) and (isNumericVar(Expr^.Right^.Right)) and (Expr^.Left^.Right^.Symbol = Expr^.Right^.Right^.Symbol) Then // third and fourth case
  Begin
   Tmp^.Left         := Expr^.Left^.Right;
   Tmp^.Right^.Left  := Expr^.Left^.Left;
   Tmp^.Right^.Right := Expr^.Right^.Left;

   Expr      := Tmp;
   AnyChange := True;
  End;

  if (isNumericVar(Expr^.Left^.Left)) and (isNumericVar(Expr^.Right^.Right)) and (Expr^.Left^.Left^.Symbol = Expr^.Right^.Right^.Symbol) Then // fifth and sixth case
  Begin
   Tmp^.Left         := Expr^.Left^.Left;
   Tmp^.Right^.Left  := Expr^.Left^.Right;
   Tmp^.Right^.Right := Expr^.Right^.Left;

   Expr      := Tmp;
   AnyChange := True;
  End;

  if (isNumericVar(Expr^.Left^.Right)) and (isNumericVar(Expr^.Right^.Left)) and (Expr^.Left^.Right^.Symbol = Expr^.Right^.Left^.Symbol) Then // seventh and eighth case
  Begin
   Tmp^.Left         := Expr^.Left^.Right;
   Tmp^.Right^.Left  := Expr^.Left^.Left;
   Tmp^.Right^.Right := Expr^.Right^.Right;

   Expr      := Tmp;
   AnyChange := True;
  End;
 End;

 { 'var = var' -> nothing }
 if (Expr^.Typ = mtAssign) and (isVariable(Expr^.Left)) and (Expr^.Right^.Symbol = Expr^.Left^.Symbol) Then
 Begin
  Compiler.CompileHint(Expr^.Token, hExpressionHasNoEffect, []);

  Expr      := nil;
  AnyChange := True;
  Exit;
 End;

 { 'var == var', 'var <= var', 'var >= var' -> true } // @TODO: these operators are not defined for every types (basically do simple type-check)
 if (Expr^.Typ in [mtEqual, mtLowerEqual, mtGreaterEqual]) and (isVariable(Expr^.Left)) and (Expr^.Left^.Symbol = Expr^.Right^.Symbol) Then
 Begin
  Compiler.CompileHint(Expr^.Token, hExpressionAlwaysTrue, []);

  Expr      := MakeBoolExpression(True, @Expr^.Token);
  AnyChange := True;
 End;

 { 'var != var', 'var < var', 'var < var' -> false }
 if (Expr^.Typ in [mtDifferent, mtLower, mtGreater]) and (isVariable(Expr^.Left)) and (Expr^.Left^.Symbol = Expr^.Right^.Symbol) Then
 Begin
  Compiler.CompileHint(Expr^.Token, hExpressionAlwaysFalse, []);

  Expr      := MakeBoolExpression(False, @Expr^.Token);
  AnyChange := True;
 End;

 { 'numvar+numvar+numvar+numvar+....+numvar' -> 'numvar * amount' }
 if (Expr^.Typ = mtAdd) and (isNumericVar(Expr^.Right)) Then
  TryToOptimizeSum(Expr);

 { 'var = var op expr' -> 'var op= expr' }
 For Simplify1 in Simplify1Data Do
  if (Expr^.Typ = mtAssign) and (isVariable(Expr^.Left)) and
     (Expr^.Right^.Typ = Simplify1.Pre) and (Expr^.Right^.Left^.Symbol = Expr^.Left^.Symbol) Then
  Begin
   Expr^.Typ   := Simplify1.Post;
   Expr^.Right := Expr^.Right^.Right;
   AnyChange   := True;
   Break;
  End;

 { 'var = expr op var' -> 'var op= expr' }
 For Simplify1 in Simplify2Data Do
  if (Expr^.Typ = mtAssign) and (isVariable(Expr^.Left)) and
     (Expr^.Right^.Typ = Simplify1.Pre) and (Expr^.Right^.Right^.Symbol = Expr^.Left^.Symbol) and (Expr^.Right^.Left^.hasValue) Then
  Begin
   Expr^.Typ   := Simplify1.Post;
   Expr^.Right := Expr^.Right^.Left;
   AnyChange   := True;
   Break;
  End;

 { '!(exprA op exprB)' -> 'exprA !op exprB'; like '!(a==b)' -> '(a!=b) }
 For Simplify1 in Simplify3Data Do
  if (Expr^.Typ = mtLogicalNOT) and (Expr^.Left^.Typ = Simplify1.Pre) Then
  Begin
   Expr            := Expr^.Left;
   Expr^.Left^.Typ := Simplify1.Post;
   AnyChange       := True;
   Break;
  End;

 { '!!expr' -> 'expr' }
 if (Expr^.Typ = mtLogicalNOT) and (Expr^.Left^.Typ = mtLogicalNOT) Then
 Begin
  Expr      := Expr^.Left^.Left;
  AnyChange := True;
 End;

 { 'boolexpr || true' -> 'true' }
 if (Expr^.Typ = mtLogicalOR) and (isEqual(Expr^.Right, True)) and (Expr^.Left^.getType = mtBool) Then
 Begin
  Compiler.CompileHint(Expr^.Token, hExpressionAlwaysTrue, []);

  Expr      := MakeBoolExpression(True, @Expr^.Token);
  AnyChange := True;
 End;

 { 'boolexpr && false' -> 'false' }
 if (Expr^.Typ = mtLogicalAND) and (isEqual(Expr^.Right, False)) and (Expr^.Left^.getType = mtBool) Then
 Begin
  Compiler.CompileHint(Expr^.Token, hExpressionAlwaysFalse, []);

  Expr      := MakeBoolExpression(False, @Expr^.Token);
  AnyChange := True;
 End;

 { 'numvar += numvar' -> 'numvar *= 2' (read+read+add+write -> read+mul+write) }
 if (Expr^.Typ = mtAddEq) and (isNumericVar(Expr^.Left)) and (Expr^.Left^.Symbol = Expr^.Right^.Symbol) Then
 Begin
  Expr^.Typ   := mtMulEq;
  Expr^.Right := MakeIntExpression(2);

  AnyChange := True;
 End;

 { 'numvar += -numexpr' -> 'numvar -= numexpr' }
 if (Expr^.Typ = mtAddEq) and (isNumericVar(Expr^.Left)) and (Expr^.Right^.Typ = mtNeg) and (isNumeric(Expr^.Right)) Then
 Begin
  Expr^.Typ   := mtSubEq;
  Expr^.Right := Expr^.Right^.Left;
  AnyChange   := True;
 End;

 { 'numvar -= -numexpr' -> 'numvar += numexpr' }
 if (Expr^.Typ = mtSubEq) and (isNumericVar(Expr^.Left)) and (Expr^.Right^.Typ = mtNeg) and (isNumeric(Expr^.Right)) Then
 Begin
  Expr^.Typ   := mtAddEq;
  Expr^.Right := Expr^.Right^.Left;
  AnyChange   := True;
 End;

 { '-const' -> 'negative value of const' }
 if (Expr^.Typ = mtNeg) and (Expr^.Left^.Typ in [mtInt, mtFloat]) Then
 Begin
  Expr        := Expr^.Left;
  Expr^.Value := -Expr^.Value;
  AnyChange   := True;
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
