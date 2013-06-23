(* OptimizeBranches *)
Procedure OptimizeBranches;
Var AnythingOptimized: Boolean = False;

  // Remap
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

  // Visit
  Procedure Visit(Parent, Node: TCFGNode; ChildID: Integer);
  Var I        : Integer;
      Value    : Boolean;
      NewParent: TCFGNode;
  Begin
   if (AnythingOptimized) Then
    Exit;

   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Typ = cetCondition) Then // if condition...
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

  // Foo
  {Procedure Foo(Node: TCFGNode; Deep: uint8=0);
  Var I, Q: Integer;
      P   : String;
  Begin
   if (Node.Parent = nil) Then
    P := '<none>' Else
    P := Node.Parent.getName+' ('+ExpressionToString(Node.Parent.Value)+')';

   For Q := 0 To Deep-1 Do
    Write(' ');
   Writeln('Node [', Node.getName, '] :: ', Node.Typ, ' :: ', ExpressionToString(Node.Value), ' ; parent = ', P);

   For I := 0 To Node.Child.Count-1 Do
   Begin
    For Q := 0 To Deep-1 Do
     Write(' ');
    Writeln('[', Node.getName, '].child ', I);
    Foo(Node.Child[I], Deep+3);
   End;
  End;}

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
 Until (not AnythingOptimized);

 DevLog(dvInfo, 'OptimizeBranches', 'Optimized branches: '+IntToStr(OptBranches));
End;
