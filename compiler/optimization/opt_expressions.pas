(* OptimizeExpressions *)
Procedure OptimizeExpressions;

{ Stage1 }
// does constant folding
Procedure Stage1;
  Procedure Visit(Node: TCFGNode);
  Var Child: TCFGNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then // if node has been visited more than once, don't check it again
    Exit;
   VisitedNodes.Add(Node);

   TCompiler(Compiler).fCurrentNode := Node;

   if (Node.Value <> nil) Then
    ExpressionCompiler.OptimizeExpression(TCompiler(Compiler), Node.Value, [oInsertConstants, oConstantFolding]); // @TODO (last parameter)

   For Child in Node.Child Do
    Visit(Child);
  End;

Begin
 VisitedNodes.Clear;
 Visit(Func.FlowGraph.Root);
End;

{ Stage2 }
// removes unused variable assigs
Procedure Stage2;
  Procedure Visit(Node: TCFGNode);
  Var Child, Back : TCFGNode;
      CanBeRemoved: Boolean;
      Symbol      : TSymbol;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   TCompiler(Compiler).fCurrentNode := Node;

   if (Node.Value <> nil) and (Node.Value^.Typ = mtAssign) Then // @TODO: assigns can be nested!
   Begin
    CanBeRemoved := False; // can currently checked assignment be removed?

    Back := Node.Parent;

    // get last variable's assign node
    While (Back <> nil) Do
    Begin
     if (Back.Value <> nil) and (Back.Value^.Typ = mtAssign) and (Back.Value^.Left^.IdentName = Node.Value^.Left^.IdentName) Then
     Begin
      Symbol := TSymbol(Back.Value^.Left^.Symbol);

      if (Symbol = nil) Then
       Symbol := TSymbol(Node.Value^.Left^.Symbol);

      if (Symbol = nil) Then // can happen when operating on array elements
      Begin
       Back := Back.Parent;
       Continue;
      End;

      if (Symbol is TLocalSymbol) and (not isVariableUsed(TLocalSymbol(Back.Value^.Left^.Symbol).mVariable, Back, Node)) Then // if variable's value is not used between assignments, we can remove that first assign
      Begin
       CanBeRemoved := True;
       Break;
      End;
     End;

     Back := Back.Parent;
    End;

    if (CanBeRemoved) Then
    Begin
     Back.Typ   := cetNone;
     Back.Value := nil;
    End;
   End;

   For Child in Node.Child Do
    Visit(Child);
  End;

Begin
 VisitedNodes.Clear;
 Visit(Func.FlowGraph.Root);
End;

Begin
 Stage1;
 Stage2;
End;
