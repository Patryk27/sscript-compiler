(* OptimizeBranches *)
Procedure OptimizeBranches;
Var AnythingOptimized: Boolean = False;

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
  Var I        : int32;
      Value    : Boolean;
      NewParent: TCFGNode;
  Begin
   if (AnythingOptimized) Then // don't try to optimize multiple branches at once
    Exit;

   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Typ = cetCondition) and (not Node.isVolatile) Then // if condition...
   Begin
    if (Node.Value^.isConstant) and (Node.Value^.Typ in [mtBool, mtInt]) Then // if can be optimized...
    Begin
     Value := Node.Value^.Value;

     DevLog(dvInfo, 'OptimizeBranches', 'Branch at line '+IntToStr(Node.Value^.Token.Line)+' has been removed (it always evaluates to '+BoolToStr(Value, 'true', 'false')+').');

     NewParent := Node.Edges[ord(not Value)]; // taken (true) = left edge, not taken (false) = right edge
     RemovedNodes.Add(Node.Edges[ord(Value)]);

     VisitedNodes.Clear;
     RemapBytecode(Func.FlowGraph.Root, Node.Edges[ord(Value)].getName, Node.getName);

     While (NewParent.Typ = cetNone) Do
     Begin
      if (NewParent.Edges.Count = 0) Then
       Break;

      NewParent := NewParent.Edges[0];

      if (NewParent = nil) Then
       TCompiler(CompilerPnt).CompileError(eInternalError, ['NewParent = nil']);
     End;

     VisitedNodes.Clear;
     RemapSSA(Node.Edges[ord(Value)], Node.Edges[2], Node.Edges[2]);

     VisitedNodes.Clear;
     Remap(Func.FlowGraph.Root, Parent, Node.Edges[ord(not Value)], Node, NewParent, Node, NewParent);

     VisitedNodes.Clear;
     Remap(Func.FlowGraph.Root, Parent, Node.Edges[ord(Value)], Node, NewParent, Node, NewParent);

     NewParent.Parent := Parent;

     if (Parent = nil) Then
      Func.FlowGraph.Root := NewParent Else
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
 Repeat
  AnythingOptimized := False;
  VisitedNodes.Clear;
  Visit(nil, Func.FlowGraph.Root, 0);

  if (AnythingOptimized) Then
  Begin
   OptimizeExpressions;
   Inc(OptBranches);
  End;

  if (OptBranches > 2000) Then // probably the optimizer has entered an infinite loop
   TCompiler(CompilerPnt).CompileError(eInternalError, ['It seems that your code somehow crashes the branch optimizer (it stays in infinite loop), sorry...']);
 Until (not AnythingOptimized);

 DevLog(dvInfo, 'OptimizeBranches', 'Optimized branches: '+IntToStr(OptBranches));
End;
