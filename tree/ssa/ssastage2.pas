(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
Unit SSAStage2;

 Interface
 Uses Expression, FlowGraph, symdef, SSA;

 { TSSAStage2 }
 Type TSSAStage2 =
      Class
       Private
        Generator: TSSAGenerator;

        VisitedParentNodes, VisitedNodes: TCFGNodeList;

       Private
        Function FetchSSAVarID(const Symbol: TSymbol; const SearchNode: TCFGNode): TSSAVarID;

        Procedure VisitExpression(const CFGNode: TCFGNode; const ExprNode: PExpressionNode);
        Procedure VisitNode(const Node, EndNode: TCFGNode);

       Public
        Constructor Create(const fGenerator: TSSAGenerator);

        Procedure Execute;
       End;

 Implementation
Uses CompilerUnit, Messages, SysUtils;

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

(* TSSAStage2.FetchSSAVarID *)
Function TSSAStage2.FetchSSAVarID(const Symbol: TSymbol; const SearchNode: TCFGNode): TSSAVarID;

  { fsVisitExpression }
  Function fsVisitExpression(const Expr: PExpressionNode): TSSAVarID;
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
      Generator.getCompiler.CompileError(eInternalError, ['TSSAStage2.fsVisitExpression: unknown symbol type ('+IntToStr(ord(Sym.Typ))+')!']);
    End;

    For I := Low(PList) To High(PList) Do // iterate each parameter
     if (PList[I].isVar) Then
      if (I <= High(Expr^.ParamList)) Then
      Begin
       if (Expr^.ParamList[I]^.Symbol = Symbol) Then
        Exit(Expr^.ParamList[I]^.SSA);
      End;
   End;

   Result := fsVisitExpression(Expr^.Left);

   if (Length(Result.Values) = 0) Then
    Result := fsVisitExpression(Expr^.Right);

   For Param in Expr^.ParamList Do
    if (Length(Result.Values) = 0) Then
     Result := fsVisitExpression(Param);
  End;

  { fsVisitNode }
  Function fsVisitNode(const Node, EndNode: TCFGNode; const CheckEdgesNotParent: Boolean=False; const CheckEndNode: Boolean=False): TSSAVarID;
  Var Left, Right, Parent    : TSSAVarID;
      PointsLeft, PointsRight: Boolean;
      Edge                   : TCFGNode;
      Foreach                : TSSAVarID;
  Begin
   SetLength(Result.Values, 0);

   if (Node = nil) or ((not CheckEndNode) and (Node = EndNode)) or (VisitedParentNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedParentNodes.Add(Node);

   { condition }
   if (Node.Typ = cetCondition) Then
   Begin
    PointsLeft  := AnythingFromNodePointsAt(Node.Edges[0], Node.Edges[2], SearchNode);
    PointsRight := AnythingFromNodePointsAt(Node.Edges[1], Node.Edges[2], SearchNode);

    // -> parent
    Parent := fsVisitNode(Node.Parent, EndNode, False, True);

    // -> left
    Left := fsVisitNode(Node.Edges[0], Node.Edges[2], True, False);

    if (Length(Left.Values) = 0) and (PointsLeft) and
       (AnythingFromNodePointsAt(Node.Edges[0], Node.Edges[2], Node)) Then // if inside a loop... (see note below)
        Left := fsVisitNode(SearchNode, Node.Edges[2], True, False);

    // -> right
    Right := fsVisitNode(Node.Edges[1], Node.Edges[2], True, False);

    if (Length(Right.Values) = 0) and (PointsRight) and
       (AnythingFromNodePointsAt(Node.Edges[1], Node.Edges[2], Node)) Then // if inside a loop... (see note below)
        Right := fsVisitNode(SearchNode, Node.Edges[2], True, False);

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

     If we were parsing `i` and did just 'Left := fsVisitNode(Node.Edges[0], Node.Edges[2], True);', it would return nothing (empty set, precisely), as
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

    if (AnythingFromNodePointsAt(Node.Edges[2], nil, SearchNode)) Then
    Begin
     Coalesce(Result, Left);
     Coalesce(Result, Right);
    End;
   End Else

   { try..catch }
   if (Node.Typ = cetTryCatch) Then
   Begin
    if (AnythingFromNodePointsAt(Node.Edges[0], nil, SearchNode)) Then // when we came from the "try" block, parse "try" and parent
     Coalesce(Result, fsVisitNode(Node.Edges[0], nil, True)) Else // 'try'

    if (AnythingFromNodePointsAt(Node.Edges[1], nil, SearchNode)) Then // when we came from the "catch" block, parse "try", "catch" and parent
    Begin
     Coalesce(Result, fsVisitNode(Node.Edges[0], nil, True)); // 'try'
     Coalesce(Result, fsVisitNode(Node.Edges[1], nil, True)); // 'catch'
    End Else

    Begin // otherwise parse "try", "catch" and parent block
     Coalesce(Result, fsVisitNode(Node.Edges[0], nil, True)); // 'try'
     Coalesce(Result, fsVisitNode(Node.Edges[1], nil, True)); // 'catch'
    End;

    Coalesce(Result, fsVisitNode(Node.Parent, EndNode, False, True)); // parent
   End Else

   { foreach }
   if (Node.Typ = cetForeach) Then
   Begin
    SetLength(Foreach.Values, 1);
    Foreach.Values[0] := Node.Foreach.LoopVarSSAID;

    if (Symbol.mVariable = Node.Foreach.LoopVar) Then
     Coalesce(Result, Foreach);

    Coalesce(Result, fsVisitNode(Node.Edges[0], nil));
   End;

   if (CheckEdgesNotParent) Then
   Begin
    For Edge in Node.Edges Do
     if (Length(Result.Values) = 0) Then
      Result := fsVisitNode(Edge, EndNode, True, CheckEndNode);
   End;

   if (Length(Result.Values) = 0) Then
    Result := fsVisitExpression(Node.Value);

   if (not CheckEdgesNotParent) and (Length(Result.Values) = 0) Then
    Result := fsVisitNode(Node.Parent, EndNode, False, CheckEndNode);
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
  if (AnythingFromNodePointsAt(SearchNode.Edges[0], SearchNode.Edges[2], SearchNode)) or
     (AnythingFromNodePointsAt(SearchNode.Edges[1], SearchNode.Edges[2], SearchNode)) Then
  Begin
   Result := fsVisitNode(SearchNode, nil); // if inside a loop
  End;

 if (Length(Result.Values) = 0) Then
  Result := fsVisitNode(SearchNode.Parent, nil);

 if (Length(Result.Values) = 0) Then
 Begin
  DevLog(dvWarning, 'FetchSSAVarID', 'Couldn''t fetch variable''s SSA id; var = '+TSymbol(Symbol).Name+', line = '+IntToStr(Origin.getToken^.Line));

  With TSymbol(Symbol).mVariable do
   if (not isConst) and (not isFuncParam) and (not isCatchVar) and (RefSymbol.isLocal) Then
    Generator.getCompiler.CompileHint(Origin.getToken, hUseOfUninitializedVariable, [RefSymbol.Name]);
 End;
End;

(* TSSAStage2.VisitExpression *)
Procedure TSSAStage2.VisitExpression(const CFGNode: TCFGNode; const ExprNode: PExpressionNode);
Var Param: PExpressionNode;
Begin
 if (ExprNode = nil) Then
  Exit;

 if (ExprNode^.Typ = mtIdentifier) and (Length(ExprNode^.SSA.Values) = 0) Then // if variable with no SSA idenitifer assigned yet
 Begin
  ExprNode^.SSA := FetchSSAVarID(TSymbol(ExprNode^.Symbol), CFGNode);
 End;

 if (ExprNode^.Typ in [mtFunctionCall, mtMethodCall]) Then
 Begin
  For Param in ExprNode^.ParamList Do
   VisitExpression(CFGNode, Param);
 End Else
 Begin
  VisitExpression(CFGNode, ExprNode^.Left);
  VisitExpression(CFGNode, ExprNode^.Right);
 End;
End;

(* TSSAStage2.VisitNode *)
Procedure TSSAStage2.VisitNode(const Node, EndNode: TCFGNode);
Var Edge: TCFGNode;
Begin
 if (Node = nil) or (Node = EndNode) or (VisitedNodes.IndexOf(Node) <> -1) Then
  Exit;
 VisitedNodes.Add(Node);

 VisitExpression(Node, Node.Value); // visit node's expression

 For Edge in Node.Edges Do
  VisitNode(Edge, EndNode);
End;

(* TSSAStage2.Create *)
Constructor TSSAStage2.Create(const fGenerator: TSSAGenerator);
Begin
 Generator := fGenerator;
End;

(* TSSAStage2.Execute *)
Procedure TSSAStage2.Execute;
Begin
 VisitedParentNodes := TCFGNodeList.Create;
 VisitedNodes       := TCFGNodeList.Create;

 Try
  VisitNode(Generator.getCurrentFunction.FlowGraph.Root, nil);
 Finally
  VisitedParentNodes.Free;
  VisitedNodes.Free;
 End;
End;

End.
