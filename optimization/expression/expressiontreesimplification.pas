(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
Unit ExpressionTreeSimplification;

 Interface
 Uses ExpressionOptimizer, ExpressionParser, Expression;

 { TExpressionTreeSimplification }
 Type TExpressionTreeSimplification =
      Class (TExpressionOptimizer)
       Private
        Function TryToOptimizeSum(const Node: PExpressionNode): Boolean;

        Function Parse(var Node: PExpressionNode): Boolean;

       Public
        Function Execute(var Node: PExpressionNode): Boolean; override;
       End;

 Implementation
Uses Messages, symdef;

(* isVariable *)
Function isVariable(const Node: PExpressionNode): Boolean;
Begin
 Result := (Node <> nil) and (Node^.Typ = mtIdentifier) and (Node^.Symbol <> nil) and (TSymbol(Node^.Symbol).Typ = stVariable);
End;

(* isVariableOrCall *)
Function isVariableOrCall(const Node: PExpressionNode): Boolean;
Begin
 Result := (isVariable(Node) or (Node^.Typ in [mtFunctionCall, mtMethodCall]));
End;

(* isEqual *)
Function isEqual(const Node: PExpressionNode; const Value: Int64): Boolean;
Begin
 if (Node = nil) Then
  Exit(False);

 Result := (Node^.Typ in [mtInt, mtFloat]) and (Node^.Value = Value);

 if (Node^.Typ = mtNeg) and (Value < 0) Then
  Result := isEqual(Node^.Left, -Value);
End;

(* isEqual *)
Function isEqual(const Node: PExpressionNode; const Value: Boolean): Boolean;
Begin
 Result := (Node^.Typ = mtBool) and (Node^.Value = Value);
End;

(* isNumeric *)
Function isNumeric(const Node: PExpressionNode): Boolean;
Begin
 Result := (Node^.getType in [mtInt, mtFloat]);
End;

(* isNumericVar *)
Function isNumericVar(const Symbol: TObject): Boolean;
Var Sym: TSymbol;
Begin
 Sym := (Symbol as TSymbol);

 if (Sym = nil) or (Sym.Typ <> stVariable) Then
  Exit(False);

 Result := Sym.mVariable.Typ.isNumerical;
End;

(* isNumericVar *)
Function isNumericVar(const Node: PExpressionNode): Boolean;
Begin
 Result := isNumericVar(Node^.Symbol);
End;


// -------------------------------------------------------------------------- //
(* TExpressionTreeSimplification.TryToOptimizeSum *)
Function TExpressionTreeSimplification.TryToOptimizeSum(const Node: PExpressionNode): Boolean;
Var Count: Integer = 0;
    Can  : Boolean = True;

  { Visit }
  Procedure Visit(const VisitNode: PExpressionNode);
  Var CheckNode: PExpressionNode;
      Check    : Array[0..1] of PExpressionNode;
  Begin
   if (VisitNode = nil) Then
    Exit;

   Check[0] := VisitNode^.Left;
   Check[1] := VisitNode^.Right;

   if (VisitNode^.Typ = mtAdd) Then
   Begin
    For CheckNode in Check Do
    Begin
     if (CheckNode^.Typ = mtAdd) Then
     Begin
      Visit(CheckNode);
      Continue;
     End;

     if (CheckNode^.Typ = mtIdentifier) Then
     Begin
      if (CheckNode^.Symbol = Node^.Right^.Symbol) Then
       Inc(Count) Else
       Can := False;
     End Else
      Can := False;
    End;
   End;
  End;

Begin
 Visit(Node);

 if (Can) and (Count > 0) Then
 Begin
  Node^.Typ  := mtMul;
  Node^.Left := MakeIntExpression(Count);
  Exit(True);
 End Else
  Exit(False);
End;

(* TExpressionTreeSimplification.Parse *)
Function TExpressionTreeSimplification.Parse(var Node: PExpressionNode): Boolean;
Var Simplify1: TOperatorSimplifyData;
    Param    : int16;
    Tmp      : PExpressionNode;
Begin
 Result := False;

 if (Node = nil) Then
  Exit;

 { 'numexpr + 0' -> 'numexpr' }
 { 'numexpr - 0' -> 'numexpr' }
 if (Node^.Typ in [mtAdd, mtSub]) and (isEqual(Node^.Right, 0)) and (isNumeric(Node^.Left)) Then
 Begin
  Node   := Node^.Left;
  Result := True;
 End;

 { '0 + numexpr' -> 'numexpr' }
 if (Node^.Typ = mtAdd) and (isEqual(Node^.Left, 0)) and (isNumeric(Node^.Right)) Then
 Begin
  Node   := Node^.Right;
  Result := True;
 End;

 { '0 - numexpr' -> '-numexpr' }
 if (Node^.Typ = mtSub) and (isEqual(Node^.Left, 0)) and (isNumeric(Node^.Left)) Then
 Begin
  Node^.Typ   := mtNeg;
  Node^.Left  := Node^.Right;
  Node^.Right := nil;
  Result      := True;
 End;

 { 'numvar - numvar' -> '0' }
 if (Node^.Typ = mtSub) and (isVariable(Node^.Left)) and (isVariable(Node^.Right)) and (Node^.Left^.Symbol = Node^.Right^.Symbol) and (isNumericVar(Node^.Left^.Symbol)) Then
 Begin
  Node   := MakeIntExpression(0, @Node^.Token);
  Result := True;
 End;

 { '(-numexprA)+numexprB' -> 'numexprB-numexprA' }
 if (Node^.Typ = mtAdd) and (Node^.Left^.Typ = mtNeg) and (isNumeric(Node^.Left)) and (isNumeric(Node^.Right)) Then
 Begin
  Tmp := EmptyExpression(@Node^.Token);

  Tmp^.Typ   := mtSub;
  Tmp^.Left  := Node^.Right;
  Tmp^.Right := Node^.Left^.Left;

  Node   := Tmp;
  Result := True;
 End;

 { 'numexprA+(-numexprB)' -> 'numexprA-numexprB' }
 if (Node^.Typ = mtAdd) and (Node^.Right^.Typ = mtNeg) and (isNumeric(Node^.Left)) and (isNumeric(Node^.Right)) Then
 Begin
  Tmp := EmptyExpression(@Node^.Token);

  Tmp^.Typ   := mtSub;
  Tmp^.Left  := Node^.Left;
  Tmp^.Right := Node^.Right^.Left;

  Node   := Tmp;
  Result := True;
 End;

 { 'numexpr*1' -> 'numexpr' }
 if (Node^.Typ = mtMul) and (isEqual(Node^.Right, 1)) and (isNumeric(Node^.Left)) Then
 Begin
  Node   := Node^.Left;
  Result := True;
 End;

 { '1*numexpr' -> 'numexpr' }
 if (Node^.Typ = mtMul) and (isEqual(Node^.Left, 1)) and (isNumeric(Node^.Right)) Then
 Begin
  Node   := Node^.Right;
  Result := True;
 End;

 { 'numexpr*(-1)' -> '-numexpr' }
 if (Node^.Typ = mtMul) and (isEqual(Node^.Right, -1)) and (isNumeric(Node^.Left)) Then
 Begin
  Tmp := EmptyExpression(@Node^.Token);

  Tmp^.Typ  := mtNeg;
  Tmp^.Left := Node^.Left;

  Node   := Tmp;
  Result := True;
 End;

 { '(-1)*numexpr' -> '-numexpr' }
 if (Node^.Typ = mtMul) and (isEqual(Node^.Left, -1)) and (isNumeric(Node^.Right)) Then
 Begin
  Tmp := EmptyExpression(@Node^.Token);

  Tmp^.Typ  := mtNeg;
  Tmp^.Left := Node^.Right;

  Node   := Tmp;
  Result := True;
 End;

 { 'numexpr/1' -> 'numexpr' }
 if (Node^.Typ = mtDiv) and (isEqual(Node^.Right, 1)) and (isNumeric(Node^.Left)) Then
 Begin
  Node   := Node^.Left;
  Result := True;
 End;

 { 'numexpr/(-1)' -> '-numexpr' }
 if (Node^.Typ = mtDiv) and (isEqual(Node^.Right, -1)) and (isNumeric(Node^.Left)) Then
 Begin
  Tmp := EmptyExpression(@Node^.Token);

  Tmp^.Typ  := mtNeg;
  Tmp^.Left := Node^.Left;

  Node   := Tmp;
  Result := True;
 End;

 { 'numvar += 1' -> '++i' }
 if (Node^.Typ = mtAddEq) and (isVariable(Node^.Left)) and (isNumericVar(Node^.Left^.Symbol)) and (isEqual(Node^.Right, 1)) Then
 Begin
  Node^.Typ   := mtPreInc;
  Node^.Right := nil;
  Result      := True;
 End;

 { 'numvar -= 1' -> '--i' }
 if (Node^.Typ = mtSubEq) and (isVariable(Node^.Left)) and (isNumericVar(Node^.Left^.Symbol)) and (isEqual(Node^.Right, 1)) Then
 Begin
  Node^.Typ   := mtPreDec;
  Node^.Right := nil;
  Result      := True;
 End;

 { 'numvar+numvar+numvar+numvar+....+numvar' -> 'numvar * amount' }
 if (Node^.Typ = mtAdd) and (isVariable(Node^.Right)) Then
 Begin
  Result := TryToOptimizeSum(Node);
 End;

 { 'numvarA*numvarB - numvarB' -> 'numvarB * (numvarA-1)' }
 if (Node^.Typ = mtSub) and (Node^.Left^.Typ = mtMul) and (Node^.Left^.Left^.hasValue) and (Node^.Left^.Right^.hasValue) and (Node^.Right^.hasValue) and
    (isVariable(Node^.Left^.Right)) and (isVariable(Node^.Right)) and (isNumericVar(Node^.Right^.Symbol)) and
    (Node^.Left^.Right^.Symbol = Node^.Right^.Symbol) Then
 Begin
  Tmp := EmptyExpression(@Node^.Token);

  Tmp^.Typ   := mtMul;
  Tmp^.Left  := Node^.Left^.Right;
  Tmp^.Right := EmptyExpression;

  Tmp^.Right^.Typ   := mtSub;
  Tmp^.Right^.Left  := Node^.Left^.Left;
  Tmp^.Right^.Right := MakeIntExpression(1);

  Node   := Tmp;
  Result := True;
 End;

 { 'numvarA*numvarB + numvarA' -> 'numvarA * (numvarB+1)' }
 { 'numvarA*numvarB - numvarA' -> 'numvarA * (numvarB-1)' }
 if (Node^.Typ in [mtAdd, mtSub]) and (Node^.Left^.Typ = mtMul) and (Node^.Left^.Left^.hasValue) and (Node^.Left^.Right^.hasValue) and (Node^.Right^.hasValue) and
    (isVariable(Node^.Left^.Left)) and (isVariable(Node^.Right)) and (isNumericVar(Node^.Right^.Symbol)) and
    (Node^.Left^.Left^.Symbol = Node^.Right^.Symbol) Then
 Begin
  Tmp := EmptyExpression(@Node^.Token);

  Tmp^.Typ   := mtMul;
  Tmp^.Left  := Node^.Left^.Left;
  Tmp^.Right := EmptyExpression(@Node^.Right^.Token);

  Tmp^.Right^.Typ   := Node^.Typ;
  Tmp^.Right^.Left  := Node^.Left^.Right;
  Tmp^.Right^.Right := MakeIntExpression(1);

  Node   := Tmp;
  Result := True;
 End;

 { 'numvarA*numvarB + numvarA*numvarC' -> 'numvarA * (numvarB+numvarC)' }
 { 'numvarA*numvarB - numvarA*numvarC' -> 'numvarA * (numvarB-numvarC)' }

 { 'numvarB*numvarA + numvarC*numvarA' -> 'numvarA * (numvarB+numvarC)' }
 { 'numvarB*numvarA - numvarC*numvarA' -> 'numvarA * (numvarB-numvarC)' }

 { 'numvarA*numvarB + numvarC*numvarA' -> 'numvarA * (numvarB+numvarC)' }
 { 'numvarA*numvarB - numvarC*numvarA' -> 'numvarA * (numvarB-numvarC)' }

 { 'numvarB*numvarA + numvarA*numvarC' -> 'numvarA * (numvarB+numvarC)' }
 { 'numvarB*numvarA - numvarA*numvarC' -> 'numvarA * (numvarB-numvarC)' }
 if (Node^.Typ in [mtAdd, mtSub]) and
    (Node^.Left^.Typ = mtMul) and (Node^.Left^.Left^.hasValue) and (Node^.Left^.Right^.hasValue) and
    (Node^.Right^.Typ = mtMul) and (Node^.Right^.Left^.hasValue) and (Node^.Right^.Right^.hasValue) Then
 Begin
  Tmp := EmptyExpression(@Node^.Token);

  Tmp^.Typ        := mtMul;
  Tmp^.Right      := EmptyExpression(@Node^.Right^.Token);
  Tmp^.Right^.Typ := Node^.Typ;

  if (isNumericVar(Node^.Left^.Left)) and (isNumericVar(Node^.Right^.Left)) and (Node^.Left^.Left^.Symbol = Node^.Right^.Left^.Symbol) Then // first and second case
  Begin
   Tmp^.Left         := Node^.Left^.Left;
   Tmp^.Right^.Left  := Node^.Left^.Right;
   Tmp^.Right^.Right := Node^.Right^.Right;

   Node   := Tmp;
   Result := True;
  End;

  if (isNumericVar(Node^.Left^.Right)) and (isNumericVar(Node^.Right^.Right)) and (Node^.Left^.Right^.Symbol = Node^.Right^.Right^.Symbol) Then // third and fourth case
  Begin
   Tmp^.Left         := Node^.Left^.Right;
   Tmp^.Right^.Left  := Node^.Left^.Left;
   Tmp^.Right^.Right := Node^.Right^.Left;

   Node   := Tmp;
   Result := True;
  End;

  if (isNumericVar(Node^.Left^.Left)) and (isNumericVar(Node^.Right^.Right)) and (Node^.Left^.Left^.Symbol = Node^.Right^.Right^.Symbol) Then // fifth and sixth case
  Begin
   Tmp^.Left         := Node^.Left^.Left;
   Tmp^.Right^.Left  := Node^.Left^.Right;
   Tmp^.Right^.Right := Node^.Right^.Left;

   Node   := Tmp;
   Result := True;
  End;

  if (isNumericVar(Node^.Left^.Right)) and (isNumericVar(Node^.Right^.Left)) and (Node^.Left^.Right^.Symbol = Node^.Right^.Left^.Symbol) Then // seventh and eighth case
  Begin
   Tmp^.Left         := Node^.Left^.Right;
   Tmp^.Right^.Left  := Node^.Left^.Left;
   Tmp^.Right^.Right := Node^.Right^.Right;

   Node   := Tmp;
   Result := True;
  End;
 End;

 { 'var = var' -> nothing }
 if (Node^.Typ = mtAssign) and (isVariable(Node^.Left)) and (Node^.Right^.Symbol = Node^.Left^.Symbol) Then
 Begin
  Compiler.CompileHint(Node^.Token, hExpressionHasNoEffect, []);

  Node := nil;
  Exit(True);
 End;

 { 'var == var', 'var <= var', 'var >= var' -> true } // @TODO: these operators are not defined for every type (basically do simple type-check here)
 if (Node^.Typ in [mtEqual, mtLowerEqual, mtGreaterEqual]) and (isVariable(Node^.Left)) and (Node^.Left^.Symbol = Node^.Right^.Symbol) Then
 Begin
  Compiler.CompileHint(Node^.Token, hExpressionAlwaysTrue, []);

  Node   := MakeBoolExpression(True, @Node^.Token);
  Result := True;
 End;

 { 'var != var', 'var < var', 'var < var' -> false }
 if (Node^.Typ in [mtDifferent, mtLower, mtGreater]) and (isVariable(Node^.Left)) and (Node^.Left^.Symbol = Node^.Right^.Symbol) Then
 Begin
  Compiler.CompileHint(Node^.Token, hExpressionAlwaysFalse, []);

  Node   := MakeBoolExpression(False, @Node^.Token);
  Result := True;
 End;

 { 'numvar+numvar+numvar+numvar+....+numvar' -> 'numvar * amount' }
 if (Node^.Typ = mtAdd) and (isNumericVar(Node^.Right)) Then
  TryToOptimizeSum(Node);

 { 'var = var op Node' -> 'var op= Node' }
 For Simplify1 in OperatorSimplifyData1 Do
  if (Node^.Typ = mtAssign) and (isVariable(Node^.Left)) and
     (Node^.Right^.Typ = Simplify1.Before) and (Node^.Right^.Left^.Symbol = Node^.Left^.Symbol) Then
  Begin
   Node^.Typ   := Simplify1.After;
   Node^.Right := Node^.Right^.Right;
   Result      := True;
   Break;
  End;

 { 'var = Node op var' -> 'var op= Node' }
 For Simplify1 in OperatorSimplifyData2 Do
  if (Node^.Typ = mtAssign) and (isVariable(Node^.Left)) and
     (Node^.Right^.Typ = Simplify1.Before) and (Node^.Right^.Right^.Symbol = Node^.Left^.Symbol) and (Node^.Right^.Left^.hasValue) Then
  Begin
   Node^.Typ   := Simplify1.After;
   Node^.Right := Node^.Right^.Left;
   Result      := True;
   Break;
  End;

 { '!(exprA op exprB)' -> 'exprA !op exprB'; like '!(a==b)' -> '(a!=b) }
 For Simplify1 in OperatorSimplifyData3 Do
  if (Node^.Typ = mtLogicalNOT) and (Node^.Left^.Typ = Simplify1.Before) Then
  Begin
   Node            := Node^.Left;
   Node^.Left^.Typ := Simplify1.After;
   Result          := True;
   Break;
  End;

 { '!!Node' -> 'Node' }
 if (Node^.Typ = mtLogicalNOT) and (Node^.Left^.Typ = mtLogicalNOT) Then
 Begin
  Node   := Node^.Left^.Left;
  Result := True;
 End;

 { 'boolexpr || true' -> 'true' }
 if (Node^.Typ = mtLogicalOR) and (isEqual(Node^.Right, True)) and (Node^.Left^.getType = mtBool) Then
 Begin
  Compiler.CompileHint(Node^.Token, hExpressionAlwaysTrue, []);

  Node   := MakeBoolExpression(True, @Node^.Token);
  Result := True;
 End;

 { 'boolexpr && false' -> 'false' }
 if (Node^.Typ = mtLogicalAND) and (isEqual(Node^.Right, False)) and (Node^.Left^.getType = mtBool) Then
 Begin
  Compiler.CompileHint(Node^.Token, hExpressionAlwaysFalse, []);

  Node   := MakeBoolExpression(False, @Node^.Token);
  Result := True;
 End;

 { 'numvar += numvar' -> 'numvar *= 2' (read+read+add+write -> read+mul+write) }
 if (Node^.Typ = mtAddEq) and (isNumericVar(Node^.Left)) and (Node^.Left^.Symbol = Node^.Right^.Symbol) Then
 Begin
  Node^.Typ   := mtMulEq;
  Node^.Right := MakeIntExpression(2);

  Result := True;
 End;

 { 'numvar += -numexpr' -> 'numvar -= numexpr' }
 if (Node^.Typ = mtAddEq) and (isNumericVar(Node^.Left)) and (Node^.Right^.Typ = mtNeg) and (isNumeric(Node^.Right)) Then
 Begin
  Node^.Typ   := mtSubEq;
  Node^.Right := Node^.Right^.Left;
  Result      := True;
 End;

 { 'numvar -= -numexpr' -> 'numvar += numexpr' }
 if (Node^.Typ = mtSubEq) and (isNumericVar(Node^.Left)) and (Node^.Right^.Typ = mtNeg) and (isNumeric(Node^.Right)) Then
 Begin
  Node^.Typ   := mtAddEq;
  Node^.Right := Node^.Right^.Left;
  Result      := True;
 End;

 { '-const' -> 'negative value of const', for what it's worth... }
 if (Node^.Typ = mtNeg) and (Node^.Left^.Typ in [mtInt, mtFloat]) Then
 Begin
  Node        := Node^.Left;
  Node^.Value := -Node^.Value;
  Result      := True;
 End;

 Parse(Node^.Left);
 Parse(Node^.Right);
 For Param := Low(Node^.ParamList) To High(Node^.ParamList) Do
  Parse(Node^.ParamList[Param]);
End;

(* TExpressionTreeSimplification.Execute *)
Function TExpressionTreeSimplification.Execute(var Node: PExpressionNode): Boolean;
Begin
 Result := Parse(Node);
End;

End.
