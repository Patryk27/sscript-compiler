(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
{$WARNING unimplemented: TCFGDeadCodeRemoval}
Unit CFGDeadCode;

 Interface
 Uses CFGOptimizer, FlowGraph;

 { TCFGDeadCodeRemoval }
 Type TCFGDeadCodeRemoval =
      Class (TCFGOptimizer)
       Private
        VisitedNodes: TCFGNodeList;
        Optimized   : Boolean;

       Private
        Procedure RemoveUnusedAssigns;

       Public
        Function Execute: Boolean; override;
       End;

 Implementation
Uses Expression, symdef;

(* TCFGDeadCodeRemoval.RemoveUnusedAssigns *)
Procedure TCFGDeadCodeRemoval.RemoveUnusedAssigns;
Var AnyChange: Boolean;

  { CheckBackward }
  Function CheckBackward(const Node: TCFGNode; out Current: TCFGNode): Boolean;
  {Var LastAssign: PExpressionNode;
      Symbol    : TSymbol;
  Begin
   // begin from parent
   Current := Node.Parent;

   // search for last assign to this variable
   While (Current <> nil) Do
   Begin
    if (Current.Value <> nil) Then // if it's an expression
    Begin
     LastAssign := Current.Value^.FindAssignment(Node.Value^.Left^.Symbol); // @TODO: what about nested assignments?

     if (LastAssign <> nil) Then // if assign found...
     Begin
      Symbol := TSymbol(LastAssign^.Left^.Symbol); // fetch symbol

      if (Symbol = nil) Then // can happen when operating on array elements - give up and move to the parent node
      Begin
       Current := Current.Parent;
       Continue;
      End;

      if (not isVariableRead(Symbol.mVariable, Current, Node)) Then // if variable's value is not used between assignments, we can remove that first assign
      Begin
       Exit(True);
      End;
     End;
    End;

    Current := Current.Parent;
   End;

   Current := nil;
   Exit(False);
  End;}
  Begin
  End;

  { CheckForward }
  Function CheckForward(const Node: TCFGNode; out FoundNode: TCFGNode): Boolean;
  {Var VisitedNodes: TCFGNodeList;
      Found       : Boolean = False;

    // Visit
    Function Visit(const Node: TCFGNode): Boolean;
    Var Edge: TCFGNode;
    Begin
     if (Node = nil) or (VisitedNodes.IndexOf(Node) > -1) Then
      Exit;

     For Edge in Node.Edges Do
     Begin
      if (not Result) Then
       Exit;

      Result := Result and Visit(Edge);
     End;
    End;

  Begin
   Exit(False); // @TODO

   VisitedNodes := TCFGNodeList.Create;

   Try
    Visit(Node.Edges[0]);
   Finally
    VisitedNodes.Free;
   End;

   Exit(Found);
  End;}
  Begin
  End;

  { Visit }
  Procedure Visit(const Node: TCFGNode);
  {Var Edge, Tmp, Found: TCFGNode;
  Begin
   if (Node = nil) or (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;

   VisitedNodes.Add(Node);

   Compiler.fCurrentNode := Node;

   if (Node.Value <> nil) and (Node.Value^.Typ = mtAssign) Then // @TODO: assigns can be nested!
   Begin
    if (CheckBackward(Node, Found) or CheckForward(Node, Found)) Then // can currently checked assignment be removed?
    Begin
     Tmp       := CurrentFunction.createNode(nil, nil);
     Tmp.Typ   := cetExpression;
     Tmp.Value := Found.Value;
     RemovedNodes.Add(Tmp);

     CurrentFunction.FlowGraph.RemapSSA(Found, Found, CurrentFunction.FlowGraph.Root, True);

     Found.Typ   := cetNone; // it's easier than removing node from the graph
     Found.Value := nil;

     AnyChange := True;
    End;
   End;

   For Edge in Node.Edges Do
    Visit(Edge);
  End;}
  Begin
  End;

Begin
 Repeat
  AnyChange := False;

  VisitedNodes.Clear;
  Visit(CurrentFunction.FlowGraph.Root);

  if (AnyChange) Then
   Optimized := True;
 Until (not AnyChange);
End;

(* TCFGDeadCodeRemoval.Execute *)
Function TCFGDeadCodeRemoval.Execute: Boolean;
Begin
 VisitedNodes := TCFGNodeList.Create;
 Optimized    := False;

 Try
  RemoveUnusedAssigns;
 Finally
  VisitedNodes.Free;
 End;

 Result := Optimized;
End;

End.

(* RemoveUnusedAssigns *)
Procedure RemoveUnusedAssigns;
Var AnyChange: Boolean;

  { Visit }
  Procedure Visit(const Node: TCFGNode);
  Var Edge, Back, Tmp: TCFGNode;
      CanBeRemoved   : Boolean;
      Symbol         : TSymbol;
      Second         : PExpressionNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   TCompiler(CompilerPnt).fCurrentNode := Node;

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

       if (not isVariableRead(TSymbol(Second^.Left^.Symbol).mVariable, Back, Node)) Then // if variable's value is not used between assignments, we can remove that first assign
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
     Tmp       := TCompiler(CompilerPnt).getCurrentFunction.createNode(nil, nil);
     Tmp.Typ   := cetExpression;
     Tmp.Value := Back.Value;
     RemovedNodes.Add(Tmp);

     RemapSSA(Back, Back, Func.FlowGraph.Root, True);
     Back.Typ   := cetNone;
     Back.Value := nil;
     AnyChange  := True;
    End;
   End;

   For Edge in Node.Edges Do
    Visit(Edge);
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
Var ID, KilledVars: int32;

  { RemoveVarAssign }
  Procedure RemoveVarAssign(const Node: TCFGNode);
  Var Edge, TmpNode  : TCFGNode;
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
     TmpNode       := TCompiler(CompilerPnt).getCurrentFunction.createNode(nil, nil);
     TmpNode.Typ   := cetExpression;
     TmpNode.Value := TmpExpr;
     RemovedNodes.Add(TmpNode);

     if (Node.Value^.containsCall) Then // if right side of the asignment is a call, remove the assignment, but leave the call
     Begin
      Node.Value^.RemoveAssignments(Func.SymbolList[ID].Name);
     End Else
     Begin // otherwise just remove the whole expression
      Node.Typ   := cetNone;
      Node.Value := nil;
     End;
    End;
   End;

   For Edge in Node.Edges Do
    RemoveVarAssign(Edge);
  End;

Begin
 With Func do
 Begin
  ID         := 0;
  KilledVars := 0;

  While (ID < SymbolList.Count) Do
  Begin
   if (SymbolList[ID].Typ = stVariable) and (not SymbolList[ID].mVariable.isFuncParam) and (not SymbolList[ID].mVariable.isVolatile) Then
    if (not isVariableRead(SymbolList[ID].mVariable, Func.FlowGraph.Root, nil)) Then // if variable's value isn't used anywhere...
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
