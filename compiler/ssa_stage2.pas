Var VisitedParentNodes: TCFGNodeList;

(* FetchSSAVarID *)
Function FetchSSAVarID(Symbol: Pointer; SearchNode: TCFGNode): TSSAVarID;

  { VisitExpression }
  Function VisitExpression(Expr: PExpression): TSSAVarID;
  Var Param: PExpression;
      PList: TParamList;
      I    : Integer;
      Sym  : TSymbol;
  Begin
   Result.Typ := sstNone;

   if (Expr = nil) Then
    Exit;

   if (Expr^.Typ in MLValueOperators) and (Expr^.Left^.Symbol = Symbol) Then
    Exit(Expr^.Left^.SSA);

   if (Expr^.Typ = mtFunctionCall) and (Expr^.Symbol <> nil) Then
   Begin
    Sym := TSymbol(Expr^.Symbol);

    Case Sym.Typ of
     stFunction: PList := Sym.mFunction.ParamList;
     stVariable: PList := Sym.mVariable.Typ.FuncParams;
     else
      TCompiler(Compiler).CompileError(eInternalError, ['{ ssa_stage2 } VisitExpression: unknown symbol type ('+IntToStr(ord(Sym.Typ))+')!']);
    End;

    For I := Low(PList) To High(PList) Do // iterate each parameter
     if (PList[I].isVar) Then
      if (I <= High(Expr^.ParamList)) Then
      Begin
       if (Expr^.ParamList[I]^.Symbol = Symbol) Then
        Exit(Expr^.ParamList[I]^.SSA);
      End;
   End;

   Result := VisitExpression(Expr^.Left);

   if (Result.Typ = sstNone) Then
    Result := VisitExpression(Expr^.Right);

   For Param in Expr^.ParamList Do
    if (Result.Typ = sstNone) Then
     Result := VisitExpression(Param);
  End;

  { VisitNode }
  Function VisitNode(Node, EndNode: TCFGNode; const CheckChildrenNotParent: Boolean=False; const CheckEndNode: Boolean=False): TSSAVarID;
  Var Left, Right, Parent: TSSAVarID;
      I, J               : Integer;
      Can                : Boolean;
      Child              : TCFGNode;
  Begin
   Result.Typ := sstNone;

   if (Node = nil) or ((not CheckEndNode) and (Node = EndNode)) or (VisitedParentNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedParentNodes.Add(Node);

   if (Node.Typ = cetCondition) Then // if condition...
   Begin
    // -> left
    Left.Typ := sstNone;

    if (Left.Typ = sstNone) Then
     Left := VisitNode(Node.Child[0], Node.Child[2], True, True);

    if (Left.Typ = sstNone) and
       (AnythingFromNodePointsAt(Node.Child[0], Node.Child[2], Node)) and
       (AnythingFromNodePointsAt(Node.Child[0], Node.Child[2], SearchNode)) Then // if inside a loop... (see note below)
        Left := VisitNode(SearchNode, Node.Child[2], True, True);

    // -> right
    Right.Typ := sstNone;

    if (Right.Typ = sstNone) Then
     Right := VisitNode(Node.Child[1], Node.Child[2], True, True);

    if (Right.Typ = sstNone) and
       (AnythingFromNodePointsAt(Node.Child[1], Node.Child[2], Node)) and
       (AnythingFromNodePointsAt(Node.Child[1], Node.Child[2], SearchNode)) Then // if inside a loop... (see note below)
        Right := VisitNode(SearchNode, Node.Child[2], True, True);

    // -> parent
    Parent := VisitNode(Node.Parent, EndNode, False, True);

    {
     @Note (about that 4 if-s above):

     Let's consider this code:
       for (var<int> i=0; i<=2; i++)
         i;
       return 0;

     In control flow graph it will look like this:
           [ i=0 ]
              |
          [ i<= 2 ]-----
          /       \     \
       [ # ]       \     \
         |          |     \
       [ i ]        |      \
         |          |       \
      [ i++ ]  [ return 0 ] |
          \                 /
           -----------------
     (# - hidden temporary node which is result of `for` loop parsing)

     If we were parsing `i` and did just 'Left := VisitNode(Node.Child[0], Node.Child[2], True);', it would return nothing (sstNone, precisely), as
     the `#` node located before the `i` node would have been parsed for the second time.
     Thus instead of clearing the `VisitedParentNodes` list (which would cause stack overflow), we're just searching from the `i` node (which hasn't
     been parsed so far).
     I guess that's all the magic here.
    }

    // coalesce left side
    For I := 0 To High(Left.Value) Do
    Begin
     SetLength(Result.Value, Length(Result.Value)+1);
     Result.Value[High(Result.Value)] := Left.Value[I];
    End;

    // coalesce right side checking if there aren't any duplicates
    For I := 0 To High(Right.Value) Do
    Begin
     Can := True;

     For J := 0 To High(Result.Value) Do
      if (Result.Value[J] = Right.Value[I]) Then
       Can := False;

     if (Can) Then
     Begin
      SetLength(Result.Value, Length(Result.Value)+1);
      Result.Value[High(Result.Value)] := Right.Value[I];
     End;
    End;

    // coalesce right side checking if there aren't any duplicates
    For I := 0 To High(Parent.Value) Do
    Begin
     Can := True;

     For J := 0 To High(Result.Value) Do
      if (Result.Value[J] = Parent.Value[I]) Then
       Can := False;

     if (Can) Then
     Begin
      SetLength(Result.Value, Length(Result.Value)+1);
      Result.Value[High(Result.Value)] := Parent.Value[I];
     End;

     // @TODO: too much DRY!
    End;

    if (Length(Result.Value) = 1) Then
     Result.Typ := sstSingle Else
     Result.Typ := sstPhi;
   End;

   if (CheckChildrenNotParent) Then
   Begin
    For Child in Node.Child Do
     if (Result.Typ = sstNone) Then
      Result := VisitNode(Child, EndNode, True, CheckEndNode);
   End;

   if (Result.Typ = sstNone) Then
    Result := VisitExpression(Node.Value);

   if (not CheckChildrenNotParent) and (Result.Typ = sstNone) Then
    Result := VisitNode(Node.Parent, EndNode, False, CheckEndNode);
  End;

Var Origin: TCFGNode;
Begin
 Origin := SearchNode;

 VisitedParentNodes.Clear;

 Result.Typ := sstNone;

 if (Symbol = nil) Then
 Begin
  DevLog(dvError, 'FetchSSAVarID', 'Function called with `Symbol = nil` (shouldn''t happen!)');
  Exit;
 End;

 if (SearchNode.Typ = cetCondition) Then
 Begin
  if (AnythingFromNodePointsAt(SearchNode.Child[0], SearchNode.Child[2], SearchNode)) or
     (AnythingFromNodePointsAt(SearchNode.Child[1], SearchNode.Child[2], SearchNode)) Then
  Begin
   // if inside a loop
   Result := VisitNode(SearchNode, nil);
  End;
 End;

// SearchNode := SearchNode.Parent;

 if (Result.Typ = sstNone) Then
  Result := VisitNode(SearchNode.Parent, nil);

 if (Result.Typ = sstNone) Then
 Begin
  DevLog(dvWarning, 'FetchSSAVarID', 'Couldn''t fetch variable''s SSA ID; var = '+TSymbol(Symbol).Name+', line = '+IntToStr(Origin.getToken^.Line));

  With TSymbol(Symbol).mVariable do
   if (not isConst) and (not isFuncParam) Then
    TCompiler(Compiler).CompileHint(Origin.getToken, hUseOfUninitializedVariable, [RefSymbol.Name]);
 End;
End;

(* VisitExpression *)
Procedure VisitExpression(Node: TCFGNode; Expr: PExpression);
Var Param: PExpression;
Begin
 if (Expr = nil) Then
  Exit;

 if (Expr^.Typ = mtVariable) and (Expr^.SSA.Typ = sstNone) Then // if variable
  Expr^.SSA := FetchSSAVarID(Expr^.Symbol, Node);

 VisitExpression(Node, Expr^.Left);
 VisitExpression(Node, Expr^.Right);
 For Param in Expr^.ParamList Do
  VisitExpression(Node, Param);
End;

(* VisitNode *)
Procedure VisitNode(Node, EndNode: TCFGNode);
Var Child: TCFGNode;
Begin
 if (Node = nil) or (Node = EndNode) or (VisitedNodes.IndexOf(Node) <> -1) Then
  Exit;
 VisitedNodes.Add(Node);

 VisitExpression(Node, Node.Value); // visit node's expression

 For Child in Node.Child Do
  VisitNode(Child, EndNode);
End;

(* Execute *)
Procedure Execute;
Begin
 VisitedParentNodes := TCFGNodeList.Create;

 Try
  VisitedNodes.Clear;
  VisitNode(Func.FlowGraph.Root, nil);
 Finally
  VisitedParentNodes.Free;
 End;
End;
