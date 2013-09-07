Type TVarMap = specialize TFPGMap<Pointer, uint32>; // TFPGMap<TSymbol, uint32>;
Var VarMap: TVarMap;

{ IncreaseSSA }
Procedure IncreaseSSA(const Expr: PExpressionNode; const PostSSAOnly: Boolean);
Var mVar: TVariable;
    ID  : Integer;
Begin
 if (Expr^.Symbol = nil) Then // probably shouldn't happen
  Exit;

 mVar := TSymbol(Expr^.Symbol).mVariable;

 if (VarMap.IndexOf(mVar) = -1) Then
  ID := 0 Else
  ID := VarMap[mVar]+1;

 if (PostSSAOnly) Then
 Begin
  SetLength(Expr^.PostSSA.Values, 1);
  Expr^.PostSSA.Values[0] := ID;
 End Else
 Begin
  SetLength(Expr^.SSA.Values, 1);
  Expr^.SSA.Values[0] := ID;
  Expr^.PostSSA       := Expr^.SSA;
 End;

 if (VarMap.IndexOf(mVar) = -1) Then
  VarMap.Add(mVar, 0) Else
  VarMap[mVar] := VarMap[mVar]+1;
End;

(* VisitExpression *)
Procedure VisitExpression(Expr: PExpressionNode);
Var Param : PExpressionNode;
    I     : Integer;
    PList : TParamList;
    Symbol: TSymbol;
Begin
 if (Expr = nil) Then
  Exit;

 if (Expr^.Typ = mtAssign) Then
 Begin
  IncreaseSSA(Expr^.Left, False);
 End Else

 if (Expr^.Typ in MLValueOperators) Then
 Begin
  IncreaseSSA(Expr^.Left, True);
 End Else

 if (Expr^.Typ = mtFunctionCall) Then // we need to check for potential pass-by-ref (as it increases the SSA var's ID as well)
 Begin
  if (Expr^.Symbol <> nil) Then
  Begin
   Symbol := TSymbol(Expr^.Symbol);

   Case Symbol.Typ of
    stFunction: PList := Symbol.mFunction.ParamList;
    stVariable: PList := Symbol.mVariable.Typ.FuncParams;
    else
     TCompiler(Compiler).CompileError(eInternalError, ['{ ssa_stage1 } VisitExpression: unknown symbol type ('+IntToStr(ord(Symbol.Typ))+')!']);
   End;

   For I := Low(PList) To High(PList) Do // iterate each parameter
    if (PList[I].isVar) Then
    Begin
     if (I <= High(Expr^.ParamList)) Then
      IncreaseSSA(Expr^.ParamList[I], False);
    End;
  End;
 End;

 VisitExpression(Expr^.Left);
 VisitExpression(Expr^.Right);
 For Param in Expr^.ParamList Do
  VisitExpression(Param);
End;

(* VisitNode *)
Procedure VisitNode(Node, EndNode: TCFGNode);
Var Child: TCFGNode;
    mVar : TVariable;
Begin
 if (Node = nil) or (Node = EndNode) or (VisitedNodes.IndexOf(Node) <> -1) Then
  Exit;
 VisitedNodes.Add(Node);

 VisitExpression(Node.Value); // visit node's expression

 if (Node.Typ = cetForeach) Then
 Begin
  mVar := Node.Foreach.LoopVar as TVariable;

  if (VarMap.IndexOf(mVar) = -1) Then
   VarMap.Add(mVar, 0) Else
   VarMap[mVar] := VarMap[mVar]+1;

  Node.Foreach.LoopVarSSAID := VarMap[mVar];
 End;

 For Child in Node.Child Do
  VisitNode(Child, EndNode);
End;

(* Execute *)
Procedure Execute;
Begin
 VarMap := TVarMap.Create;

 Try
  VisitedNodes.Clear;
  VisitNode(Func.FlowGraph.Root, nil);
 Finally
  VarMap.Free;
 End;
End;
