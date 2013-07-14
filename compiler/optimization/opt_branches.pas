(* OptimizeBranches *)
Procedure OptimizeBranches;
Var AnythingOptimized: Boolean = False;
  { Remap }
  Procedure Remap(Parent, Node, nFrom, nTo, nFromParent, nToParent: TCFGNode);
  Var I: Integer;
  Begin
   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Parent = nFromParent) Then
    Node.Parent := nToParent;

   For I := 0 To Node.Child.Count-1 Do
   Begin
    if (Node.Child[I] = nFrom) Then
    Begin
     Node.Child[I]        := nTo;
     Node.Child[I].Parent := Parent;
    End Else
     Remap(Node, Node.Child[I], nFrom, nTo, nFromParent, nToParent);
   End;
  End;

  { Visit }
  Procedure Visit(Parent, Node: TCFGNode; ChildID: Integer);
  Var I        : Integer;
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

     DevLog(dvInfo, 'OptimizeBranches', 'Branch at line '+IntToStr(Node.Value^.Token.Line)+' has been removed (it always evaluates to '+BoolToStr(Value, 'true', 'false')+')!');

     NewParent := Node.Child[ord(not Value)]; // taken (true) = left child, not taken (false) = right child
     RemovedNodes.Add(Node.Child[ord(Value)]);

     While (NewParent.Typ = cetNone) Do
     Begin
      if (NewParent.Child.Count = 0) Then
       Break;

      NewParent := NewParent.Child[0];

      if (NewParent = nil) Then
       TCompiler(Compiler).CompileError(eInternalError, ['NewParent = nil']);
     End;

     VisitedNodes.Clear;
     RemapSSA(Node.Child[ord(Value)], Node.Child[2], Node.Child[2]);

     VisitedNodes.Clear;
     Remap(Parent, Node.Child[ord(not Value)], Node, NewParent, Node, NewParent);

     VisitedNodes.Clear;
     Remap(Parent, Node.Child[ord(Value)], Node, NewParent, Node, NewParent);

     NewParent.Parent := Parent;

     if (Parent = nil) Then
      Func.FlowGraph.Root := NewParent Else
      Parent.Child[ChildID] := NewParent;

     AnythingOptimized := True;
     Exit;
    End;
   End;

   For I := 0 To Node.Child.Count-1 Do
    Visit(Node, Node.Child[I], I);
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

  if (OptBranches > 1000) Then // probably the optimizer has entered an infinite loop
   TCompiler(Compiler).CompileError(eInternalError, ['It seems that your code somehow crashes the branch optimizer (it stays in infinite loop), sorry...']);
 Until (not AnythingOptimized);

 DevLog(dvInfo, 'OptimizeBranches', 'Optimized branches: '+IntToStr(OptBranches));
End;
