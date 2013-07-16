(* RemoveUnusedVariables *)
Procedure RemoveUnusedVariables;
Var ID, KilledVars: Integer;

  // RemoveVarAssign
  Procedure RemoveVarAssign(Node: TCFGNode);
  Var Child : TCFGNode;
      Assign: PExpressionNode;
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
   if (SymbolList[ID].Typ = stVariable) and (not SymbolList[ID].mVariable.isFuncParam) Then
    if (not SymbolList[ID].mVariable.isVolatile) Then
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
