(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
{$WARNING unimplemented: TCFGExpressionOptimization}
Unit CFGExpressions;

 Interface
 Uses CFGOptimizer, Expression, FlowGraph, FGL;

 { TCFGExpressionSimplification }
 Type TCFGExpressionSimplification =
      Class (TCFGOptimizer)
       Private
        VisitedNodes: TCFGNodeList;
        Optimized   : Boolean;

       Private
        Procedure VisitNode(const Node: TCFGNode);

       Public
        Function Execute: Boolean; override;
       End;

 { TCFGExpressionOptimization }
 Type TCFGExpressionOptimization =
      Class (TCFGOptimizer)
       Private
        // THandler
        Type THandler = Procedure (const Node: TCFGNode) of object;

        // TVarData - used in constant propagation algorithm
        Type PVarData = ^TVarData;
             TVarData =
             Record
              Symbol: Pointer;
             // SSAID : TSSAVarID;
              Value : TExpressionNode;
             End;

        // TVarList
        Type TVarList = specialize TFPGList<PVarData>;

       Private
        VisitedNodes: TCFGNodeList;
        Optimized   : Boolean;

        VarList: TVarList;

        Handler: THandler;

       Private
        //Function cp_findVarData(const Symbol: Pointer; const SSAID: TSSAVarID): PVarData;
        Procedure cp_VisitExpression(const Node: TExpressionNode);

        Procedure VisitNode(const Node: TCFGNode);

        Procedure ConstantFolding(const Node: TCFGNode);
        Procedure ConstantPropagation(const Node: TCFGNode);

       Public
        Function Execute: Boolean; override;
       End;

 Implementation
Uses CommandLine;

(* TCFGExpressionSimplification.VisitNode *)
Procedure TCFGExpressionSimplification.VisitNode(const Node: TCFGNode);
Var OldNode: TExpressionNode;
    Edge   : TCFGNode;
Begin
 if (Node = nil) or (VisitedNodes.IndexOf(Node) > -1) Then
  Exit;
 VisitedNodes.Add(Node);

 if (Node.Value <> nil) Then
 Begin
  Compiler.fCurrentNode := Node;

  OldNode    := Node.Value;
  Node.Value := Node.Value.Optimize();
  Optimized  := Optimized or (Node.Value.getCost < OldNode.getCost);
  OldNode.Free;
 End;

 For Edge in Node.Edges Do
  VisitNode(Edge);
End;

(* TCFGExpressionSimplification.Execute *)
Function TCFGExpressionSimplification.Execute: Boolean;
Begin
 VisitedNodes := TCFGNodeList.Create;
 Optimized    := False;

 Try
  VisitNode(CurrentFunction.FlowGraph.Root);
 Finally
  VisitedNodes.Free;
 End;

 Exit(Optimized);
End;

// -------------------------------------------------------------------------- //

(* TCFGExpressionOptimization.cp_findVarData *)
{Function TCFGExpressionOptimization.cp_findVarData(const Symbol: Pointer; const SSAID: TSSAVarID): PVarData;
Begin
 For Result in VarList Do
  if (Result^.Symbol = Symbol) and (Result^.SSAID = SSAID) Then
   Exit;

 Exit(nil);
End;}

(* TCFGExpressionOptimization.cp_VisitExpression *)
Procedure TCFGExpressionOptimization.cp_VisitExpression(const Node: TExpressionNode);
{Var VarData: PVarData;
    Param  : TExpressionNode;
Begin
 if (Node = nil) Then
  Exit;

 { var = constant }
 if (Node^.Typ = mtAssign) and (Node^.Right^.isConstant) Then
 Begin
  New(VarData);
  VarData^.Symbol := Node^.Left^.Symbol;
  VarData^.SSAID  := Node^.Left^.SSA;
  VarData^.Value  := Node^.Right;

  VarList.Add(VarData);
 End Else

 { var op= something (eg.x *= 5) }
 if (Node^.Typ in MLValueOperators) and (Node^.Left^.Typ = mtIdentifier) and (Node^.Left^.IdentType = mtNothing) Then
 Begin
  VarData := cp_findVarData(Node^.Left^.Symbol, Node^.Left^.SSA);

  if (VarData <> nil) Then
  Begin
   Node^.Left^.IdentType  := VarData^.Value^.Typ;
   Node^.Left^.IdentValue := VarData^.Value^.Value;

   Optimized := True;
  End;
 End Else

 { var }
 if (Node^.Typ = mtIdentifier) and (Node^.IdentType = mtNothing) Then
 Begin
  VarData := cp_findVarData(Node^.Symbol, Node^.SSA);

  if (VarData <> nil) Then
  Begin
   Node^.Typ   := VarData^.Value^.Typ;
   Node^.Value := VarData^.Value^.Value;

   Node^.IdentType  := Node^.Typ;
   Node^.IdentValue := Node^.Value;

   Optimized := True;
  End;
 End;

 if (not (Node^.Typ in MLValueOperators)) Then
  cp_VisitExpression(Node^.Left);

 cp_VisitExpression(Node^.Right);

 For Param in Node^.ParamList Do
  cp_VisitExpression(Param);
End;}
Begin
End;

(* TCFGExpressionOptimization.VisitNode *)
Procedure TCFGExpressionOptimization.VisitNode(const Node: TCFGNode);
Var Edge: TCFGNode;
Begin
 if (Node = nil) or (VisitedNodes.IndexOf(Node) > -1) Then
  Exit;
 VisitedNodes.Add(Node);

 Compiler.fCurrentNode := Node;
 Handler(Node);

 For Edge in Node.Edges Do
  VisitNode(Edge);
End;

(* TCFGExpressionOptimization.ConstantFolding *)
Procedure TCFGExpressionOptimization.ConstantFolding(const Node: TCFGNode);
Var OldNode: TExpressionNode;
Begin
 if (Node.Value <> nil) Then
 Begin
  OldNode    := Node.Value;
  Node.Value := Node.Value.Evaluate();
  Optimized  := Optimized or (Node.Value.getCost < OldNode.getCost);
  OldNode.Free;
 End;
End;

(* TCFGExpressionOptimization.ConstantPropagation *)
Procedure TCFGExpressionOptimization.ConstantPropagation(const Node: TCFGNode);
Begin
 cp_VisitExpression(Node.Value);
End;

(* TCFGExpressionOptimization.Execute *)
Function TCFGExpressionOptimization.Execute: Boolean;
Begin
 VisitedNodes := TCFGNodeList.Create;
 VarList      := TVarList.Create;

 Try
  Repeat
   Optimized := False;

   // do constant folding
   if (CmdLine.getBoolSwitch(opt__constant_folding)) Then
   Begin
    Handler := @ConstantFolding;
    VisitedNodes.Clear;
    VisitNode(CurrentFunction.FlowGraph.Root);
   End;

   // do constant propagation
   if (CmdLine.getBoolSwitch(opt__constant_propagation)) Then
   Begin
    Handler := @ConstantPropagation;
    VisitedNodes.Clear;
    VisitNode(CurrentFunction.FlowGraph.Root);
   End;
  Until (not Optimized);
 Finally
  VisitedNodes.Free;
  VarList.Free;
 End;

 Exit(Optimized);
End;

End.
