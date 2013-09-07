(* RemoveUnusedAssigns *)
Procedure RemoveUnusedAssigns;
Var AnyChange: Boolean;

  { Visit }
  Procedure Visit(Node: TCFGNode);
  Var Child, Back, Tmp: TCFGNode;
      CanBeRemoved    : Boolean;
      Symbol          : TSymbol;
      Second          : PExpressionNode;
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
     if (Back.Value <> nil) Then
     Begin
      Second := Back.Value^.FindAssignment(Node.Value^.Left^.IdentName);

      if (Second <> nil) Then // if assign found...
      Begin
       Symbol := TSymbol(Second^.Left^.Symbol);

       if (Symbol = nil) Then
        Symbol := TSymbol(Second^.Left^.Symbol);

       if (Symbol = nil) Then // can happen when operating on array elements
       Begin
        Back := Back.Parent;
        Continue;
       End;

       if (not isVariableUsed(TSymbol(Second^.Left^.Symbol).mVariable, Back, Node)) Then // if variable's value is not used between assignments, we can remove that first assign
       Begin
        CanBeRemoved := True;
        Break;
       End;
      End;
     End;

     Back := Back.Parent;
    End;

    if (CanBeRemoved) Then
    Begin
     Tmp       := TCFGNode.Create(nil, nil);
     Tmp.Typ   := cetExpression;
     Tmp.Value := Back.Value;
     RemovedNodes.Add(Tmp);

     RemapSSA(Back, Back, Func.FlowGraph.Root, True);
     Back.Typ   := cetNone;
     Back.Value := nil;
     AnyChange  := True;
    End;
   End;

   For Child in Node.Child Do
    Visit(Child);
  End;

Begin
 Repeat
  AnyChange := False;

  VisitedNodes.Clear;
  Visit(Func.FlowGraph.Root);
 Until (not AnyChange);
End;

(* RemoveUnusedVariables *)
Procedure RemoveUnusedVariables;
Var ID, KilledVars: Integer;

  { RemoveVarAssign }
  Procedure RemoveVarAssign(Node: TCFGNode);
  Var Child, TmpNode : TCFGNode;
      Assign, TmpExpr: PExpressionNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Value <> nil) Then
   Begin
    Assign := Node.Value^.FindAssignment(Func.SymbolList[ID].Name);

    if (Assign <> nil) Then
    Begin
     New(TmpExpr);
     TmpExpr^      := Assign^;
     TmpNode       := TCFGNode.Create(nil, nil);
     TmpNode.Typ   := cetExpression;
     TmpNode.Value := TmpExpr;
     RemovedNodes.Add(TmpNode);

     if (Node.Value^.HasCall) Then // if right side of the asignment is a call, remove the assignment, but leave the call
     Begin
      Node.Value^.RemoveAssignments(Func.SymbolList[ID].Name);
     End Else
     Begin // otherwise just remove the whole expression
      Node.Typ   := cetNone;
      Node.Value := nil;
     End;
    End;
   End;

   For Child in Node.Child Do
    RemoveVarAssign(Child);
  End;

Begin
 With Func do
 Begin
  ID         := 0;
  KilledVars := 0;

  While (ID < SymbolList.Count) Do
  Begin
   if (SymbolList[ID].Typ = stVariable) and (not SymbolList[ID].mVariable.isFuncParam) and (not SymbolList[ID].mVariable.isVolatile) Then
    if (not isVariableUsed(SymbolList[ID].mVariable, Func.FlowGraph.Root, nil)) Then // if variable's value isn't used anywhere...
    Begin
     VisitedNodes.Clear;
     RemoveVarAssign(Func.FlowGraph.Root); // remove every assignment to this variable

     DevLog(dvInfo, 'RemoveUnusedVariables', 'Variable killed: '+SymbolList[ID].Name);
     SymbolList.Remove(SymbolList[ID]);
     Dec(ID);
     Inc(KilledVars);
    End;

   Inc(ID);
  End;

  DevLog(dvInfo, 'RemoveUnusedVariables', 'Killed variables: '+IntToStr(KilledVars));
 End;
End;
