(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.

 This optimizer does two kinds of branch optimization:

 1) Removing constant branches - removing "if (true)" and "if (false)" conditionals.

 2) Rearranging branch flow;
 I.e. changing:
  > if (!something)
  >   a(); else
  >   b();

 To:
  > if (something)
  >  b(); else
  >  a();
*)
Unit CFGBranchSimplification;

 Interface
 Uses CFGOptimizer, SysUtils;

 { TCFGBranchSimplification }
 Type TCFGBranchSimplification =
      Class (TCFGOptimizer)
       Private
        Optimized: Boolean;

       Private
        Procedure OptimizeConstantBranches;
        Procedure OptimizeBranchesFlow;

       Public
        Function Execute: Boolean; override;
       End;

 Implementation
Uses Logging, SSCompiler, FlowGraph, Expression, Messages, CFGExpressions;

(* TCFGBranchSimplification.OptimizeConstantBranches *)
Procedure TCFGBranchSimplification.OptimizeConstantBranches;
Var AnythingOptimized: Boolean = False;
    VisitedNodes     : TCFGNodeList;

  { RemapBytecode }
  Procedure RemapBytecode(const Node: TCFGNode; const LabelFrom, LabelTo: String);
  Var Edge: TCFGNode;
      I   : uint8;
  Begin
   if (VisitedNodes.IndexOf(Node) <> -1) or (Node = nil) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Typ = cetBytecode) Then
   Begin
    With Node.Bytecode do
     For I := Low(OpcodeArgList^) To High(OpcodeArgList^) Do
      if (OpcodeArgList^[I].VType = vtPChar) and (OpcodeArgList^[I].VPChar = ':'+LabelFrom) Then
      Begin
       // FreeMem(OpcodeArgList^[I].VPChar); // @TODO (?)
       OpcodeArgList^[I].VPChar := CopyStringToPChar(':'+LabelTo);
      End;
   End;

   For Edge in Node.Edges Do
    RemapBytecode(Edge, LabelFrom, LabelTo);
  End;

  { Remap }
  Procedure Remap(const Root, Parent, Node, nFrom, nTo, nFromParent, nToParent: TCFGNode);
  Var I: int32;
  Begin
   if (VisitedNodes.IndexOf(Node) <> -1) or (Node = nil) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Parent = nFromParent) Then
    Node.Parent := nToParent;

   For I := 0 To Node.Edges.Count-1 Do
   Begin
    if (Node.Edges[I] = nFrom) Then
    Begin
     Node.Edges[I]        := nTo;
     Node.Edges[I].Parent := Parent;
    End Else
     Remap(Root, Node, Node.Edges[I], nFrom, nTo, nFromParent, nToParent);
   End;
  End;

  { Visit }
  Procedure Visit(const Parent, Node: TCFGNode; const EdgeID: int32);
  Var I        : int8;
      Value    : Boolean;
      NewParent: TCFGNode;
  Begin
   if (AnythingOptimized) or (Node = nil) or (VisitedNodes.IndexOf(Node) <> -1) Then // don't try to optimize many branches at once
    Exit;

   VisitedNodes.Add(Node);

   if (Node.Typ = cetCondition) and (not Node.isVolatile) Then // if condition...
   Begin
    if (Node.Value^.isConstant) and (Node.Value^.Typ in [mtBool, mtInt]) Then // if can be optimized...
    Begin
     Value := Node.Value^.Value;

     DevLog(dvInfo, 'Branch at line %d has been removed (it always evaluates to %s).', [Node.Value^.Token.Line, BoolToStr(Value, 'true', 'false')]);

     NewParent := Node.Edges[ord(not Value)]; // taken (true) = left edge, not taken (false) = right edge
     RemovedNodes.Add(Node.Edges[ord(Value)]);

     VisitedNodes.Clear;
     RemapBytecode(CurrentFunction.FlowGraph.Root, Node.Edges[ord(Value)].getName, Node.getName);

     While (NewParent.Typ = cetNone) Do
     Begin
      if (NewParent.Edges.Count = 0) Then
       Break;

      NewParent := NewParent.Edges[0];

      if (NewParent = nil) Then
       Compiler.CompileError(eInternalError, ['NewParent = nil']);
     End;

     VisitedNodes.Clear;
     CurrentFunction.FlowGraph.RemapSSA(Node.Edges[ord(Value)], Node.Edges[2], Node.Edges[2]);

     VisitedNodes.Clear;
     Remap(CurrentFunction.FlowGraph.Root, Parent, Node.Edges[ord(not Value)], Node, NewParent, Node, NewParent);

     VisitedNodes.Clear;
     Remap(CurrentFunction.FlowGraph.Root, Parent, Node.Edges[ord(Value)], Node, NewParent, Node, NewParent);

     NewParent.Parent := Parent;

     if (Parent = nil) Then
      CurrentFunction.FlowGraph.Root := NewParent Else
      Parent.Edges[EdgeID] := NewParent;

     AnythingOptimized := True;
     Exit;
    End;
   End;

   For I := 0 To Node.Edges.Count-1 Do
    Visit(Node, Node.Edges[I], I);
  End;

Var OptBranches: uint32 = 0;
Begin
 VisitedNodes := TCFGNodeList.Create;

 Try
  Repeat
   AnythingOptimized := False;
   VisitedNodes.Clear;
   Visit(nil, CurrentFunction.FlowGraph.Root, 0);

   if (AnythingOptimized) Then // removing a branch might have given a new expression optimization possibilities - worth a try
   Begin
    Inc(OptBranches);
    Optimized := True;

    With TCFGExpressionOptimization.Create(Compiler, CurrentFunction) do
    Begin
     Execute;
     Free;
    End;
   End;
  Until (not AnythingOptimized);
 Finally
  VisitedNodes.Free;
 End;

 DevLog(dvInfo, 'Optimized %d constant branches.', [OptBranches]);
End;

(* TCFGBranchSimplification.OptimizeBranchesFlow *)
Procedure TCFGBranchSimplification.OptimizeBranchesFlow;
Var VisitedNodes: TCFGNodeList;
    OptBranches : uint32 = 0;

  { Visit }
  Procedure Visit(const Node: TCFGNode);
  Var Edge, Tmp: TCFGNode;
  Begin
   if (Node = nil) or (VisitedNodes.IndexOf(Node) > -1) Then
    Exit;

   VisitedNodes.Add(Node);

   if (Node.getType = cetCondition) and (not Node.isVolatile) Then // if condition
   Begin
    if (Node.Value^.getType = mtBool) Then // if bool-based condition
    Begin
     if (Node.Value^.Typ = mtLogicalNOT) Then
      Inc(OptBranches);

     While (Node.Value^.Typ = mtLogicalNOT) Do
     Begin
      Node.Value := Node.Value^.Left; // remove unary '!'

      Tmp           := Node.Edges[0]; // switch edges
      Node.Edges[0] := Node.Edges[1];
      Node.Edges[1] := Tmp;
     End;
    End;
   End;

   For Edge in Node.Edges Do
    Visit(Edge);
  End;

Begin
 VisitedNodes := TCFGNodeList.Create;

 Try
  Visit(CurrentFunction.FlowGraph.Root);
 Finally
  VisitedNodes.Free;
 End;

 DevLog(dvInfo, 'Optimized flow of %d branches.', [OptBranches]);
End;

(* TCFGBranchSimplification.Execute *)
Function TCFGBranchSimplification.Execute: Boolean;
Begin
 Optimized := False;

 OptimizeConstantBranches;
 OptimizeBranchesFlow;

 Exit(Optimized);
End;

End.
