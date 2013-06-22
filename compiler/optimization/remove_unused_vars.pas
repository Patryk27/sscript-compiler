(* RemoveUnusedVariables *)
Procedure RemoveUnusedVariables;
Var ID, KilledVars: Integer;

  // RemoveVarAssign
  Procedure RemoveVarAssign(Node: TCFGNode);
  Var Child: TCFGNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Typ = cetExpression) and (Node.Value^.Typ = mtAssign) and (Node.Value^.Left^.IdentName = Func.SymbolList[ID].Name) Then // @TODO: assigns can be nested!
   Begin
    if (Node.Value^.Right^.HasCall) Then // if right side of the asignment is a call, remove the assign, but leave the call
     Node.Value := Node.Value^.Right Else
     Begin // otherwise just remove the whole expression
      Node.Typ   := cetNone;
      Node.Value := nil;
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
   if (SymbolList[ID].Typ = lsVariable) and (not SymbolList[ID].mVariable.isFuncParam) Then
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
