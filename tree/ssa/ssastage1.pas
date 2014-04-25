(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
Unit SSAStage1;

 Interface
 Uses Expression, symdef, FlowGraph, SSA, FGL;

 Type TVarMap = specialize TFPGMap<Pointer, uint32>; // TFPGMap<TSymbol, uint32>;

 { TSSAStage1 }
 Type TSSAStage1 =
      Class
       Private
        Generator: TSSAGenerator;

        VisitedNodes: TCFGNodeList;
        VarMap      : TVarMap;

       Private
        Procedure IncreaseSSA(Node: PExpressionNode; const PostSSAOnly: Boolean);

        Procedure VisitExpression(const Node: PExpressionNode);
        Procedure VisitNode(const Node, EndNode: TCFGNode);

       Public
        Constructor Create(const fGenerator: TSSAGenerator);

        Procedure Execute;
       End;

 Implementation
Uses SysUtils, Messages;

(* TSSAStage1.IncreaseSSA *)
Procedure TSSAStage1.IncreaseSSA(Node: PExpressionNode; const PostSSAOnly: Boolean);
Var mVar: TVariable;
    ID  : Integer;
Begin
 While (Node^.Typ = mtArrayElement) Do
  Node := Node^.Left;

 if (Node^.Symbol = nil) Then // probably shouldn't happen
  Exit;

 mVar := TSymbol(Node^.Symbol).mVariable;

 if (VarMap.IndexOf(mVar) = -1) Then
  ID := 0 Else
  ID := VarMap[mVar]+1;

 if (PostSSAOnly) Then
 Begin
  SetLength(Node^.PostSSA.Values, 1);
  Node^.PostSSA.Values[0] := ID;
 End Else
 Begin
  SetLength(Node^.SSA.Values, 1);
  Node^.SSA.Values[0] := ID;
  Node^.PostSSA       := Node^.SSA;
 End;

 if (VarMap.IndexOf(mVar) = -1) Then
  VarMap.Add(mVar, 0) Else
  VarMap[mVar] := VarMap[mVar]+1;
End;

(* TSSAStage1.VisitExpression *)
Procedure TSSAStage1.VisitExpression(const Node: PExpressionNode);
Var Symbol: TSymbol;
    Param : PExpressionNode;
    PList : TFunctionParamList;
    I     : Integer;
Begin
 if (Node = nil) Then
  Exit;

 if (Node^.Typ = mtAssign) Then
 Begin
  IncreaseSSA(Node^.Left, False);
 End Else

 if (Node^.Typ in MLValueOperators) Then
 Begin
  IncreaseSSA(Node^.Left, True);
 End Else

 if (Node^.Typ = mtFunctionCall) Then // we need to check for potential pass-by-ref (as it increases the SSA var's ID as well)
 Begin
  if (Node^.Symbol <> nil) Then
  Begin
   Symbol := TSymbol(Node^.Symbol);

   Case Symbol.Typ of
    stFunction: PList := Symbol.mFunction.ParamList;
    stVariable: PList := Symbol.mVariable.Typ.FuncParams;

    else
     raise ESSAGeneratorException.CreateFmt('Unknown symbol type: %d', [ord(Symbol.Typ)]);
   End;

   For I := Low(PList) To High(PList) Do // iterate each parameter
   Begin
    if (PList[I].isVar) Then
    Begin
     if (I <= High(Node^.ParamList)) Then
      IncreaseSSA(Node^.ParamList[I], False);
    End;
   End;
  End;
 End;

 VisitExpression(Node^.Left);
 VisitExpression(Node^.Right);
 For Param in Node^.ParamList Do
  VisitExpression(Param);
End;

(* TSSAStage1.VisitNode *)
Procedure TSSAStage1.VisitNode(const Node, EndNode: TCFGNode);
Var Edge: TCFGNode;
    mVar: TVariable;
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

 For Edge in Node.Edges Do
  VisitNode(Edge, EndNode);
End;

(* TSSAStage1.Create *)
Constructor TSSAStage1.Create(const fGenerator: TSSAGenerator);
Begin
 Generator := fGenerator;
End;

(* TSSAStage1.Execute *)
Procedure TSSAStage1.Execute;
Begin
 VisitedNodes := TCFGNodeList.Create;
 VarMap       := TVarMap.Create;

 Try
  VisitNode(Generator.getCurrentFunction.FlowGraph.Root, nil);
 Finally
  VisitedNodes.Free;
  VarMap.Free;
 End;
End;
End.
