Var VisitedParentNodes: TCFGNodeList;

{ DumpGraph } // debug only
Procedure DumpGraph(Node: TCFGNode; Deep: uint8=0);
Var I, Q: Integer;
    P   : String;
Begin
 if (Node = nil) Then
  Exit;

 if (VisitedNodes.IndexOf(Node) > -1) Then
 Begin
  For Q := 0 To Deep-1 Do
   Write(' ');
  Writeln('@', Node.getName);
  Exit;
 End;
 VisitedNodes.Add(Node);

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
  DumpGraph(Node.Child[I], Deep+3);
 End;
End;

(* Coalesce *)
Procedure Coalesce(var Input: TSSAVarID; const What: TSSAVarID);
Var I, J: Integer;
    Can : Boolean;
Begin
 For I := 0 To High(What.Values) Do
 Begin
  Can := True;

  For J := 0 To High(Input.Values) Do
   if (Input.Values[J] = What.Values[I]) Then
    Can := False;

  if (Can) Then
  Begin
   SetLength(Input.Values, Length(Input.Values)+1);
   Input.Values[High(Input.Values)] := What.Values[I];
  End;
 End;
End;

(* FetchSSAVarID *)
Function FetchSSAVarID(Symbol: TSymbol; SearchNode: TCFGNode): TSSAVarID;

  { VisitExpression }
  Function VisitExpression(Expr: PExpressionNode): TSSAVarID;
  Var Param: PExpressionNode;
      PList: TParamList;
      I    : Integer;
      Sym  : TSymbol;
  Begin
   SetLength(Result.Values, 0);

   if (Expr = nil) Then
    Exit;

   if (Expr^.Typ in MLValueOperators) and (Expr^.Left^.Symbol = Symbol) Then
    Exit(Expr^.Left^.PostSSA);

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

   if (Length(Result.Values) = 0) Then
    Result := VisitExpression(Expr^.Right);

   For Param in Expr^.ParamList Do
    if (Length(Result.Values) = 0) Then
     Result := VisitExpression(Param);
  End;

  { VisitNode }
  Function VisitNode(Node, EndNode: TCFGNode; const CheckChildrenNotParent: Boolean=False; const CheckEndNode: Boolean=False): TSSAVarID;
  Var Left, Right, Parent    : TSSAVarID;
      PointsLeft, PointsRight: Boolean;
      Child                  : TCFGNode;
      Foreach                : TSSAVarID;
  Begin
  // Writeln(Node.Typ, ' :: ', ExpressionToString(Node.Value));

   SetLength(Result.Values, 0);

   if (Node = nil) or ((not CheckEndNode) and (Node = EndNode)) or (VisitedParentNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedParentNodes.Add(Node);

   { condition }
   if (Node.Typ = cetCondition) Then
   Begin
    PointsLeft  := AnythingFromNodePointsAt(Node.Child[0], Node.Child[2], SearchNode);
    PointsRight := AnythingFromNodePointsAt(Node.Child[1], Node.Child[2], SearchNode);

    // -> parent
    Parent := VisitNode(Node.Parent, EndNode, False, True);

    // -> left
    Left := VisitNode(Node.Child[0], Node.Child[2], True, False);

    if (Length(Left.Values) = 0) and (PointsLeft) and
       (AnythingFromNodePointsAt(Node.Child[0], Node.Child[2], Node)) Then // if inside a loop... (see note below)
        Left := VisitNode(SearchNode, Node.Child[2], True, False);

    // -> right
    Right := VisitNode(Node.Child[1], Node.Child[2], True, False);

    if (Length(Right.Values) = 0) and (PointsRight) and
       (AnythingFromNodePointsAt(Node.Child[1], Node.Child[2], Node)) Then // if inside a loop... (see note below)
        Right := VisitNode(SearchNode, Node.Child[2], True, False);

    {
     @Note (about that 2 long if-s above):

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
     (# - hidden temporary node which is result of the `for` loop parsing)

     If we were parsing `i` and did just 'Left := VisitNode(Node.Child[0], Node.Child[2], True);', it would return nothing (empty set, precisely), as
     the `#` node located before the `i` node would have been parsed for the second time.
     Thus instead of clearing the `VisitedParentNodes` list (which would cause stack overflow), we're just searching from right the `i` node (which
     hasn't been parsed so far).
     I guess that's all the magic here.
    }

    if (Length(Left.Values) = 0) or (Length(Right.Values) = 0) Then
     Coalesce(Result, Parent);

    if (PointsLeft) Then
     Coalesce(Result, Left);

    if (PointsRight) Then
     Coalesce(Result, Right);

    if (AnythingFromNodePointsAt(Node.Child[2], nil, SearchNode)) Then
    Begin
     Coalesce(Result, Left);
     Coalesce(Result, Right);
    End;
   End Else

   { try..catch }
   if (Node.Typ = cetTryCatch) Then
   Begin
    if (AnythingFromNodePointsAt(Node.Child[0], nil, SearchNode)) Then // when we came from the "try" block, parse "try" and parent
     Coalesce(Result, VisitNode(Node.Child[0], nil, True)) Else // 'try'

    if (AnythingFromNodePointsAt(Node.Child[1], nil, SearchNode)) Then // when we came from the "catch" block, parse "try", "catch" and parent
    Begin
     Coalesce(Result, VisitNode(Node.Child[0], nil, True)); // 'try'
     Coalesce(Result, VisitNode(Node.Child[1], nil, True)); // 'catch'
    End Else

    Begin // otherwise parse "try", "catch" and parent block
     Coalesce(Result, VisitNode(Node.Child[0], nil, True)); // 'try'
     Coalesce(Result, VisitNode(Node.Child[1], nil, True)); // 'catch'
    End;

    Coalesce(Result, VisitNode(Node.Parent, EndNode, False, True)); // parent
   End Else

   { foreach }
   if (Node.Typ = cetForeach) Then
   Begin
    SetLength(Foreach.Values, 1);
    Foreach.Values[0] := Node.Foreach.LoopVarSSAID;

    if (Symbol.mVariable = Node.Foreach.LoopVar) Then
     Coalesce(Result, Foreach);

    Coalesce(Result, VisitNode(Node.Child[0], nil));
   End;

   if (CheckChildrenNotParent) Then
   Begin
    For Child in Node.Child Do
     if (Length(Result.Values) = 0) Then
      Result := VisitNode(Child, EndNode, True, CheckEndNode);
   End;

   if (Length(Result.Values) = 0) Then
    Result := VisitExpression(Node.Value);

   if (not CheckChildrenNotParent) and (Length(Result.Values) = 0) Then
    Result := VisitNode(Node.Parent, EndNode, False, CheckEndNode);
  End;

Var Origin: TCFGNode;
Begin
 if (Symbol = nil) Then
 Begin
  DevLog(dvError, 'FetchSSAVarID', 'Function called with `Symbol = nil` (shouldn''t happen!)');
  Exit;
 End;

 Origin := SearchNode;
 VisitedParentNodes.Clear;

 SetLength(Result.Values, 0);

 if (SearchNode.Typ = cetCondition) Then
  if (AnythingFromNodePointsAt(SearchNode.Child[0], SearchNode.Child[2], SearchNode)) or
     (AnythingFromNodePointsAt(SearchNode.Child[1], SearchNode.Child[2], SearchNode)) Then
  Begin
   Result := VisitNode(SearchNode, nil); // if inside a loop
  End;

 if (Length(Result.Values) = 0) Then
  Result := VisitNode(SearchNode.Parent, nil);

 if (Length(Result.Values) = 0) Then
 Begin
  DevLog(dvWarning, 'FetchSSAVarID', 'Couldn''t fetch variable''s SSA id; var = '+TSymbol(Symbol).Name+', line = '+IntToStr(Origin.getToken^.Line));

  With TSymbol(Symbol).mVariable do
   if (not isConst) and (not isFuncParam) and (not isCatchVar) and (RefSymbol.isLocal) Then
    TCompiler(Compiler).CompileHint(Origin.getToken, hUseOfUninitializedVariable, [RefSymbol.Name]);
 End;
End;

(* VisitExpression *)
Procedure VisitExpression(Node: TCFGNode; Expr: PExpressionNode);
Var Param: PExpressionNode;
Begin
 if (Expr = nil) Then
  Exit;

 if (Expr^.Typ = mtIdentifier) and (Length(Expr^.SSA.Values) = 0) Then // if variable with no SSA idenitifer assigned yet
 Begin
//  Writeln(ExpressionToString(Expr));
  Expr^.SSA := FetchSSAVarID(TSymbol(Expr^.Symbol), Node);
//  Writeln(ExpressionToString(Expr));
//  Writeln;
 End;

 if (Expr^.Typ in [mtFunctionCall, mtMethodCall]) Then
 Begin
  For Param in Expr^.ParamList Do
   VisitExpression(Node, Param);
 End Else
 Begin
  VisitExpression(Node, Expr^.Left);
  VisitExpression(Node, Expr^.Right);
 End;
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
