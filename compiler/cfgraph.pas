(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.

 cfgraph.pas
 Control flow graph.
*)
Unit cfgraph;

 Interface
 Uses FGL, Expression, Tokens;

 Type TVarRecArray = Array of TVarRec;
      PVarRecArray = ^TVarRecArray;

 Const NEXT_FALSE = 0;
       NEXT_TRUE  = 1;

 Type TCFGNodeType = (cetNone, cetExpression, cetCondition, cetReturn, cetThrow, cetTryCatch, cetObjDelete, cetBytecode);

 Type TCFGNode = Class;

 Type TCFGNodeList = specialize TFPGList<TCFGNode>;

 Type TCFGNode = Class
                  Private
                   Name  : String;
                   fToken: PToken_P;

                  Public
                   Typ  : TCFGNodeType;
                   Value: PExpression;

                   Parent: TCFGNode;
                   Child : TCFGNodeList;

                   Bytecode: Record // if `Typ = cetBytecode`
                              OpcodeName   : String;
                              OpcodeArgList: PVarRecArray;

                              LabelName: String;
                             End;

                   Constructor Create(fParent: TCFGNode; ffToken: PToken_P=nil);
                   Constructor Create(fParent: TCFGNode; fTyp: TCFGNodeType; fValue: PExpression; ffToken: PToken_P=nil);

                   Function isThere(const SearchType: TCFGNodeType): Boolean;

                   Function getToken: PToken_P;
                   Property getName: String read Name;
                  End;

 Type TCFGraph = Class
                  Root, Last: TCFGNode;

                  Constructor Create;
                  Procedure AddNode(Node: TCFGNode);
                 End;

 Procedure DrawGraph(Graph: TCFGraph);

 Function AnythingFromNodePointsAt(rBeginNode, rEndNode, AtWhat: TCFGNode): Boolean;

 Function getVariableUseCount(VariablePnt: Pointer; rBeginNode, rEndNode: TCFGNode): uint32;

 Function isVariableModified(VariablePnt: Pointer; rBeginNode, rEndNode: TCFGNode): Boolean;
 Function isVariableUsed(VariablePnt: Pointer; rBeginNode, rEndNode: TCFGNode): Boolean;

 Implementation
Uses Math, Classes, SysUtils, symdef, ExpressionCompiler;

{ RandomSymbol }
Function RandomSymbol: String;
Var I: uint8;
Begin
 Result := '';

 For I := 0 To 10 Do
  Result += chr(ord('a')+Random(20));
End;

// -------------------------------------------------------------------------- //
(* DrawGraph *)
Procedure DrawGraph(Graph: TCFGraph);
Var Str, Visited: TStringList;

  { NodeToString }
  Function NodeToString(Node: TCFGNode): String;
  Begin
   if (Node.Typ = cetExpression) Then
    Result := ExpressionToString(PExpression(Node.Value)) Else

   if (Node.Typ = cetCondition) Then
    Result := 'if ('+ExpressionToString(PExpression(Node.Value))+')' Else

   if (Node.Typ = cetReturn) Then
    Result := 'return '+ExpressionToString(PExpression(Node.Value)) Else

   if (Node.Typ = cetThrow) Then
    Result := 'throw '+ExpressionToString(PExpression(Node.Value)) Else

   if (Node.Typ = cetTryCatch) Then
    Result := 'try' Else

   if (Node.Typ = cetBytecode) Then
    Result := '<bytecode>' Else

    Result := '';

   Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  End;

  { Parse }
  Function Parse(Node: TCFGNode): String;
  Var Str: String;
  Begin
   Result := '';

   if (Node = nil) Then
    Exit('nil');

   if (Visited.IndexOf(Node.Name) <> -1) Then
   Begin
    if (Node.Typ = cetNone) Then
    Begin
     if (Node.Child.Count = 0) Then
      Exit('nil') Else
      Exit(Parse(Node.Child[0]));
    End Else
     Exit(Node.Name);
   End;

   Visited.Add(Node.Name);

   if (Node.Typ = cetNone) Then
    if (Node.Child.Count = 0) Then
     Exit('nil') Else
     Exit(Parse(Node.Child[0]));

   { expression }
   if (Node.Typ = cetExpression) Then
   Begin
    Result := Node.Name;

    Case Node.Child.Count of
     0: Exit;
     1: Result += ' -> '+Parse(Node.Child[0]);
     else
      raise Exception.CreateFmt('DrawGraph::Parse() -> damaged graph! Node.Child.Count = %d', [Node.Child.Count]);
    End;
   End Else

   { condition }
   if (Node.Typ = cetCondition) Then
   Begin
    Str := Node.Name;

    Result := Str;
    Result += #13#10+Str+' -> '+Parse(Node.Child[0]);
    Result += #13#10+Str+' -> '+Parse(Node.Child[1]);
   End Else

   { return }
   if (Node.Typ = cetReturn) Then
   Begin
    Result := Node.Name;

    if (Node.Child.Count <> 0) Then
     Result += ' -> '+Parse(Node.Child[0]);
   End Else

   { throw }
   if (Node.Typ = cetThrow) Then
   Begin
    Result := Node.Name;

    if (Node.Child.Count <> 0) Then
     Result += ' -> '+Parse(Node.Child[0]);
   End Else

   { try }
   if (Node.Typ = cetTryCatch) Then
   Begin
    Result := Node.Name;

    Result += #13#10+Node.Name+' -> '+Parse(Node.Child[0]);
    Result += #13#10+Node.Name+' -> '+Parse(Node.Child[1]);
    Result += #13#10+Node.Name+' -> '+Parse(Node.Child[2]);
   End Else

   { bytecode }
   if (Node.Typ = cetBytecode) Then
   Begin
    Result := Node.Name;

    if (Node.Child.Count <> 0) Then
     Result += ' -> '+Parse(Node.Child[0]);
   End Else

    raise Exception.Create('DrawGraph::Parse() -> invalid Node.Typ');
  End;

  { ParseF }
  Procedure ParseF(Node: TCFGNode);
  Var I : Integer;
      NS: String;
  Begin
   if (Node = nil) Then
    Exit;

   if (Visited.IndexOf(Node.Name) <> -1) Then
    Exit;

   Visited.Add(Node.Name);

   NS := NodeToString(Node);

   if (Length(NS) <> 0) Then
    Str.Add(Node.Name+' [label="'+NS+'"];');

   For I := 0 To Node.Child.Count-1 Do
    ParseF(Node.Child[I]);
  End;

Begin
 Str     := TStringList.Create;
 Visited := TStringList.Create;

 Try
  Str.Add('digraph cfgraph {');

  Visited.Clear;
  ParseF(Graph.Root);

  Visited.Clear;
  Str.Add(Parse(Graph.Root));
  Str.Add('}');
 Finally
  Visited.Free;
  Str.SaveToFile('tree.dot');
  Str.Free;
 End;
End;

(* AnythingFromNodePointsAt *)
{
 Checks if any node coming from `rBeginNode` and ending at `rEndNode` points at `AtWhat`.
 Useful mainly/only for checking loops.
}
Function AnythingFromNodePointsAt(rBeginNode, rEndNode, AtWhat: TCFGNode): Boolean;
Var Visited: TStringList;

    // Visit
    Procedure Visit(Node: TCFGNode);
    Var Child: TCFGNode;
    Begin
     if (Node = rEndNode) Then
      Exit;

     if (Visited.IndexOf(Node.Name) <> -1) Then
      Exit;
     Visited.Add(Node.Name);

     if (Node = AtWhat) Then
     Begin
      Result := True;
      Exit;
     End;

     For Child in Node.Child Do
     Begin
      if (Child = AtWhat) Then
      Begin
       Result := True;
       Exit;
      End;

      Visit(Child);
     End;
    End;

Begin
 Result := False;

 Visited := TStringList.Create;
 Try
  Visit(rBeginNode);
 Finally
  Visited.Free;
 End;
End;

(* getVariableUseCount *)
Function getVariableUseCount(VariablePnt: Pointer; rBeginNode, rEndNode: TCFGNode): uint32;
Var Visited : TCFGNodeList;
    VarName : String;
    VarRange: TRange;

    // VisitExpression
    Procedure VisitExpression(Expr: PExpression);
    Var I: Integer;
    Begin
     if (Expr = nil) Then
      Exit;

     if (Expr^.Typ = mtVariable) Then
      if (Expr^.IdentName = VarName) Then
       Inc(Result);

     if (Expr^.Left <> nil) Then
      VisitExpression(Expr^.Left);

     if (Expr^.Right <> nil) Then
      VisitExpression(Expr^.Right);

     For I := 0 To High(Expr^.ParamList) Do
      VisitExpression(Expr^.ParamList[I]);
    End;

    // VisitNode
    Procedure VisitNode(Node: TCFGNode);
    Var Child: TCFGNode;
        Range: Boolean;
    Begin
     if (Node = rEndNode) Then
      Exit;

     if (Visited.IndexOf(Node) <> -1) Then
      Exit;
     Visited.Add(Node);

     if (Node.Value <> nil) Then
     Begin
      Range := inRange(Node.getToken^.Position, VarRange.PBegin, VarRange.PEnd);

      if (Range) Then
       VisitExpression(Node.Value);
     End;

     For Child in Node.Child Do
      VisitNode(Child);
    End;

Begin
 Result := 0;

 VarName  := TVariable(VariablePnt).RefSymbol.Name;
 VarRange := TVariable(VariablePnt).RefSymbol.Range;

 Visited := TCFGNodeList.Create;
 Try
  VisitNode(rBeginNode);
 Finally
  Visited.Free;
 End;
End;

(* isVariableModified *)
{
 Checks if variable `VariablePnt` is modified between nodes.
 `rEndNode` can be `nil`, if every node beginning from `rBeginNode` has to be checked.

 @TODO: what about variables passed-by-reference?
}
Function isVariableModified(VariablePnt: Pointer; rBeginNode, rEndNode: TCFGNode): Boolean;
Var Visited : TStringList;
    VarName : String;
    VarRange: TRange;

    // Visit
    Procedure Visit(Node: TCFGNode);
    Var Child: TCFGNode;
        Range: Boolean;
    Begin
     if (Node = rEndNode) Then
      Exit;

     if (Visited.IndexOf(Node.Name) <> -1) Then
      Exit;
     Visited.Add(Node.Name);

     if (Node.Value <> nil) Then // @TODO: those operators can be nested!
     Begin
      Range := inRange(Node.getToken^.Position, VarRange.PBegin, VarRange.PEnd);

      if (Range) and (Node.Value^.isVariableModified(VarName, False)) Then
      Begin
       Result := True;
       Exit;
      End;
     End;

     For Child in Node.Child Do
      Visit(Child);
    End;

Begin
 Result := False;

 VarName  := TVariable(VariablePnt).RefSymbol.Name;
 VarRange := TVariable(VariablePnt).RefSymbol.Range;

 Visited := TStringList.Create;
 Try
  Visit(rBeginNode);
 Finally
  Visited.Free;
 End;
End;

(* isVariableUsed *)
{
 Returns `true` if variable is used in expressions between specified nodes.
}
Function isVariableUsed(VariablePnt: Pointer; rBeginNode, rEndNode: TCFGNode): Boolean;
Var Visited : TStringList;
    VarName : String;
    VarRange: TRange;

    // isUsed
    Function isUsed(Node: TCFGNode; Expr: PExpression): Boolean;
    Var I    : Integer;
        Range: Boolean;
    Begin
     Result := False;

     if (Expr = nil) Then
      Exit(False);

     if (Expr^.Typ = mtVariable) Then
     Begin
      Range := inRange(Expr^.Token.Position, VarRange.PBegin, VarRange.PEnd);

      if (Range) and (Expr^.IdentName = VarName) Then
       Exit(True);
     End;

     if (Expr^.Typ = mtAssign) Then
     Begin
      {
       in expression "x = 10;", don't count "x" as an "used variable"
      }
      Result := isUsed(Node, Expr^.Right);
     End Else
     Begin
      if (Expr^.Left <> nil) Then
       Result := Result or isUsed(Node, Expr^.Left);

      if (Expr^.Right <> nil) Then
       Result := Result or isUsed(Node, Expr^.Right);

      if (not Result) Then
       For I := 0 To High(Expr^.ParamList) Do
        if (isUsed(Node, Expr^.ParamList[I])) Then
         Exit(True);
     End;
    End;

    // Visit
    Procedure Visit(Node: TCFGNode);
    Var Child: TCFGNode;
    Begin
     if (Node = rEndNode) Then
      Exit;

     if (Visited.IndexOf(Node.Name) <> -1) Then
      Exit;
     Visited.Add(Node.Name);

     if (isUsed(Node, Node.Value)) Then
     Begin
      Result := True;
      Exit;
     End;

     For Child in Node.Child Do
      Visit(Child);
    End;

Begin
 Result := False;

 VarName  := TVariable(VariablePnt).RefSymbol.Name;
 VarRange := TVariable(VariablePnt).RefSymbol.Range;

 Visited := TStringList.Create;
 Try
  Visit(rBeginNode);
 Finally
  Visited.Free;
 End;
End;

// -------------------------------------------------------------------------- //
(* TCFGNode.Create *)
Constructor TCFGNode.Create(fParent: TCFGNode; ffToken: PToken_P);
Begin
 Name := RandomSymbol;

 Typ    := cetNone;
 Value  := nil;
 fToken := ffToken;
 Parent := fParent;

 Child := TCFGNodeList.Create;
End;

(* TCFGNode.Create *)
Constructor TCFGNode.Create(fParent: TCFGNode; fTyp: TCFGNodeType; fValue: PExpression; ffToken: PToken_P);
Begin
 Name := RandomSymbol;

 Typ    := fTyp;
 Value  := fValue;
 fToken := ffToken;
 Parent := fParent;

 if (fToken = nil) Then
  if (Value <> nil) Then
   fToken := @Value^.Token;

 Child := TCFGNodeList.Create;
End;

(* TCFGNode.isThere *)
Function TCFGNode.isThere(const SearchType: TCFGNodeType): Boolean;
Var mChild: TCFGNode;
Begin
 Result := False;

 if (Typ = SearchType) Then
  Exit(True);

 For mChild in Child Do
  if (mChild.isThere(SearchType)) Then
   Exit(True);
End;

(* TCFGNode.getToken *)
Function TCFGNode.getToken: PToken_P;
Begin
 if (fToken <> nil) Then
  Result := fToken Else

 if (Value <> nil) Then
  Result := @Value^.Token Else

  Result := nil; // warning!
End;

// -------------------------------------------------------------------------- //
(* TCFGraph.Create *)
Constructor TCFGraph.Create;
Begin
 Root := nil;
 Last := nil;
End;

(* TCFGraph.AddNode *)
Procedure TCFGraph.AddNode(Node: TCFGNode);
Begin
 if (Node = nil) Then
  raise Exception.Create('TCFGraph.AddNode() -> Node = nil');

 if (Root = nil) Then
 Begin
  Root := Node;
  Last := Root;
 End Else
 Begin
  if (Last = nil) Then
   raise Exception.Create('TCFGraph.AddNode() -> Last = nil; damaged graph!');

  if (Last.Typ = cetExpression) and (Last.Child.Count <> 0) Then
   raise Exception.Create('TCFGraph.AddNode() -> expression node cannot have more than one child!');

  if (Last.Typ = cetCondition) and (Last.Child.Count >= 2) Then
   raise Exception.Create('TCFGraph.AddNode() -> condition node cannot have more than two children!');

  if (Last.Typ = cetReturn) and (Last.Child.Count <> 0) Then
   raise Exception.Create('TCFGraph.AddNode() -> return node cannot have more than one child!');

  Last.Child.Add(Node);
  Last := Node;
 End;
End;
End.
