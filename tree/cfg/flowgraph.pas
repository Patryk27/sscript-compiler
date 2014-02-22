(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 Control flow graph.
*)
Unit FlowGraph;

 Interface
 Uses FGL, Expression, Tokens;

 Type TVarRecArray = Array of TVarRec;
      PVarRecArray = ^TVarRecArray;

 Type TCFGNodeType = (cetNone, cetExpression, cetCondition, cetReturn, cetThrow, cetTryCatch, cetForeach, cetArrayInitializer, cetBytecode);

 Type TCFGNode = class;
      TCFGNodeList = specialize TFPGList<TCFGNode>;

 { TCFGNode }
 Type TCFGNode =
      Class
       Private { fields }
        Name  : String;
        fToken: PToken_P;

       Public { fields }
        Typ  : TCFGNodeType;
        Value: PExpressionNode;

        Parent: TCFGNode;
        Edges : TCFGNodeList;

        Bytecode: // if `Typ` is `cetBytecode`
        Record
         OpcodeName   : String;
         OpcodeArgList: PVarRecArray;

         LabelName: String;
        End;

        Foreach: // if `Typ` is `cetForeach`
        Record
         LoopVar, LoopIterVar, LoopExprHolder, LoopSizeHolder: TObject; // TVariable
         LoopVarSSAID                                        : uint32;
        End;

        ArrayInitializer: // if `Typ` is `cetArrayInitializer`
        Record
         VarSymbol: TObject; // TSymbol
         Values   : Array of PExpressionNode; // consecutive array values
         DimSizes : Array of uint32; // size of each array dimension
        End;

        isVolatile: Boolean; // is node volatile? (information for optimizers)

       Public { methods }
        Constructor Create(const fParent: TCFGNode; const fName: String; const ffToken: PToken_P=nil);
        Constructor Create(const fParent: TCFGNode; const fName: String; const fTyp: TCFGNodeType; const fValue: PExpressionNode; const ffToken: PToken_P=nil);

        Function SearchFor(const NodeType: TCFGNodeType): TCFGNode;

        Function getToken: PToken_P;
        Property getName: String read Name;
       End;

 { TCFGraph }
 Type TCFGraph =
      Class
       Public { fields }
        Root, Last: TCFGNode;

       Public { methods }
        Constructor Create;
        Procedure AddNode(Node: TCFGNode);

        Procedure Validate;
        Procedure CheckReturns(const CompilerPnt: Pointer; const isVoidOrNaked: Boolean);
       End;

 Procedure SaveGraph(const Graph: TCFGraph; const FileName: String);

 Function AnythingFromNodePointsAt(rBeginNode, rEndNode, AtWhat: TCFGNode): Boolean;

 Function getVariableCFGCost(Symbol: TObject; rBeginNode, rEndNode: TCFGNode): uint32;
 Function isVariableRead(const VariablePnt: TObject; const rBeginNode, rEndNode: TCFGNode): Boolean;

 Implementation
Uses Math, Classes, SysUtils, SSCompiler, Messages, symdef, ExpressionCompiler;

(* SaveGraph *)
Procedure SaveGraph(const Graph: TCFGraph; const FileName: String);
Var Str, Visited: TStringList;

  { NodeToString }
  Function NodeToString(const Node: TCFGNode): String;
  Begin
   if (Node.Typ = cetExpression) Then
    Result := ExpressionToString(PExpressionNode(Node.Value)) Else

   if (Node.Typ = cetCondition) Then
    Result := 'if ('+ExpressionToString(PExpressionNode(Node.Value))+')' Else

   if (Node.Typ = cetReturn) Then
    Result := 'return '+ExpressionToString(PExpressionNode(Node.Value)) Else

   if (Node.Typ = cetThrow) Then
    Result := 'throw '+ExpressionToString(PExpressionNode(Node.Value)) Else

   if (Node.Typ = cetTryCatch) Then
    Result := 'try' Else

   if (Node.Typ = cetForeach) Then
    Result := 'foreach('+TVariable(Node.Foreach.LoopVar).RefSymbol.Name+' in '+ExpressionToString(PExpressionNode(Node.Value))+')' Else

   if (Node.Typ = cetBytecode) Then
    Result := '<bytecode>' Else

    Result := '';

   Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  End;

  { Parse }
  Function Parse(const Node: TCFGNode): String;
  Var Str: String;
  Begin
   Result := '';

   if (Node = nil) Then
    Exit('nil');

   if (Visited.IndexOf(Node.Name) <> -1) Then
   Begin
    if (Node.Typ = cetNone) Then
    Begin
     if (Node.Edges.Count = 0) Then
      Exit('nil') Else
      Exit(Parse(Node.Edges[0]));
    End Else
     Exit(Node.Name);
   End;

   Visited.Add(Node.Name);

   { none }
   if (Node.Typ = cetNone) Then
   Begin
    if (Node.Edges.Count = 0) Then
     Result := 'nil' Else
     Result := {Node.Name+' -> '+}Parse(Node.Edges[0]);
   End Else

   { expression }
   if (Node.Typ = cetExpression) Then
   Begin
    Result := Node.Name;

    Case Node.Edges.Count of
     0: Exit;
     1: Result += ' -> '+Parse(Node.Edges[0]);
     else
      raise Exception.CreateFmt('DrawGraph::Parse() -> damaged graph! Node.Edges.Count = %d', [Node.Edges.Count]);
    End;
   End Else

   { condition }
   if (Node.Typ = cetCondition) Then
   Begin
    Str := Node.Name;

    Result := Str;
    Result += #13#10+Str+' -> '+Parse(Node.Edges[0]);
    Result += #13#10+Str+' -> '+Parse(Node.Edges[1]);
   End Else

   { return }
   if (Node.Typ = cetReturn) Then
   Begin
    Result := Node.Name;

    if (Node.Edges.Count <> 0) Then
     Result += ' -> '+Parse(Node.Edges[0]);
   End Else

   { throw }
   if (Node.Typ = cetThrow) Then
   Begin
    Result := Node.Name;

    if (Node.Edges.Count <> 0) Then
     Result += ' -> '+Parse(Node.Edges[0]);
   End Else

   { try }
   if (Node.Typ = cetTryCatch) Then
   Begin
    Result := Node.Name;

    Result += #13#10+Node.Name+' -> '+Parse(Node.Edges[0]);
    Result += #13#10+Node.Name+' -> '+Parse(Node.Edges[1]);
    Result += #13#10+Node.Name+' -> '+Parse(Node.Edges[2]);
   End Else

   { foreach }
   if (Node.Typ = cetForeach) Then
   Begin
    Result := Node.Name;

    Str := Parse(Node.Edges[0]);

    Result += #13#10+Node.Name+' -> '+Str;
    Result += #13#10+Str+' -> '+Node.Name;
    Result += #13#10+Node.Name+' -> '+Parse(Node.Edges[1]);
   End Else

   { bytecode }
   if (Node.Typ = cetBytecode) Then
   Begin
    Result := Node.Name;

    if (Node.Edges.Count <> 0) Then
     Result += ' -> '+Parse(Node.Edges[0]);
   End Else

    raise Exception.CreateFmt('DrawGraph::Parse() -> invalid Node.Typ = %d', [ord(Node.Typ)]);
  End;

  { ParseF }
  Procedure ParseF(const Node: TCFGNode);
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

   For I := 0 To Node.Edges.Count-1 Do
    ParseF(Node.Edges[I]);
  End;

Var DirName: String;
Begin
 DirName := ExtractFileDir(FileName);

 if (not DirectoryExists(DirName)) Then
  CreateDir(DirName);

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
  Str.SaveToFile(FileName);
  Str.Free;
 End;
End;

(* AnythingFromNodePointsAt *)
{
 Checks if any node coming from `rBeginNode` and ending at `rEndNode` points at `AtWhat`.
 Useful mainly/only for checking loops.
}
Function AnythingFromNodePointsAt(rBeginNode, rEndNode, AtWhat: TCFGNode): Boolean;
Var Visited: TCFGNodeList;

  { Visit }
  Procedure Visit(Node: TCFGNode);
  Var Edges: TCFGNode;
  Begin
   if (Node = nil) or (Node = rEndNode) or (Visited.IndexOf(Node) <> -1) Then
    Exit;
   Visited.Add(Node);

   if (Node = AtWhat) Then
   Begin
    Result := True; // @Note: "Result" here refers to "AnythingFromNodePointsAt"
    Exit;
   End;

   For Edges in Node.Edges Do
    Visit(Edges);
  End;

Begin
 Result := False;

 Visited := TCFGNodeList.Create;
 Try
  Visit(rBeginNode);
 Finally
  Visited.Free;
 End;
End;

(* getVariableCFGCost *)
{
 Returns cost of operations on the passed variable.

 Basically:
 var += expr; <- cost = 2 (var read + var write)
 var = expr; <- cost = 1 (var write)
 var = var*var; <- cost = 3 (var read + var read + var write)
 xyz = var; <- cost = 1 ('var' is read once)
 xyz = 10*var; <- cost = 1
 etc. etc.

 Passing by reference is not counted to cost.
}
Function getVariableCFGCost(Symbol: TObject; rBeginNode, rEndNode: TCFGNode): uint32;
Var Visited: TCFGNodeList;

  { VisitExpression }
  Procedure VisitExpression(Expr: PExpressionNode);
  Var I: Integer;
  Begin
   if (Expr = nil) Then
    Exit;

   if (Expr^.Typ = mtIdentifier) and (Expr^.Symbol = Symbol) Then
   Begin
    Inc(Result); // var read
   End Else

   if (Expr^.Typ = mtAssign) and (Expr^.Left^.Symbol = Symbol) Then
   Begin
    Inc(Result); // var write
    VisitExpression(Expr^.Right);
   End Else

   if (Expr^.Typ in MLValueOperators) and (Expr^.Left^.Symbol = Symbol) Then
   Begin
    Inc(Result, 2); // var read + var write
    VisitExpression(Expr^.Right);
   End Else

   Begin
    VisitExpression(Expr^.Left);
    VisitExpression(Expr^.Right);
   End;

   For I := 0 To High(Expr^.ParamList) Do
    VisitExpression(Expr^.ParamList[I]);
  End;

  { VisitNode }
  Procedure VisitNode(Node: TCFGNode);
  Var Edges: TCFGNode;
  Begin
   if (Node = rEndNode) or (Visited.IndexOf(Node) <> -1) Then
    Exit;
   Visited.Add(Node);

   VisitExpression(Node.Value);

   For Edges in Node.Edges Do
    VisitNode(Edges);
  End;

Begin
 Result := 0;

 Visited := TCFGNodeList.Create;
 Try
  VisitNode(rBeginNode);
 Finally
  Visited.Free;
 End;
End;

(* isVariableRead *)
{
 Returns `true` if specified variable is used in any expression between specified nodes, excluding assignments and assign-operators (`*=`, `-=`, `++`...)
}
Function isVariableRead(const VariablePnt: TObject; const rBeginNode, rEndNode: TCFGNode): Boolean;
Var Visited : TCFGNodeList;
    VarName : String;
    VarRange: TRange;

  { isRead }
  Function isRead(const Node: TCFGNode; const Expr: PExpressionNode): Boolean;
  Var I    : Integer;
      Range: Boolean;
  Begin
   Result := False;

   if (Expr = nil) Then
    Exit(False);

   if (Expr^.Typ = mtIdentifier) Then
   Begin
    Range := inRange(Expr^.Token.Position, VarRange.PBegin.Position, VarRange.PEnd.Position);

    if (Range) and (Expr^.IdentName = VarName) Then
     Exit(True);
   End;

   if (Expr^.Typ in MLValueOperators) Then
   Begin
    {
     in expressions like "x = 10;" don't count "x" as an "used variable"
    }
    Result := isRead(Node, Expr^.Right);
   End Else
   Begin
    if (Expr^.Left <> nil) Then
     Result := Result or isRead(Node, Expr^.Left);

    if (Expr^.Right <> nil) Then
     Result := Result or isRead(Node, Expr^.Right);

    if (not Result) Then
     For I := 0 To High(Expr^.ParamList) Do
      if (isRead(Node, Expr^.ParamList[I])) Then
       Exit(True);
   End;
  End;

  { Visit }
  Procedure Visit(Node: TCFGNode);
  Var Edges: TCFGNode;
  Begin
   if (Node = nil) or (Node = rEndNode) or (Visited.IndexOf(Node) <> -1) Then
    Exit;

   Visited.Add(Node);

   if (isRead(Node, Node.Value)) Then
   Begin
    Result := True; // @Note: "Result" here refers to "isVariableRead"
    Exit;
   End;

   if (Node.Typ = cetForeach) and (Node.Foreach.LoopVar = VariablePnt) Then
   Begin
    Result := True; // @Note: ditto
    Exit;
   End;

   For Edges in Node.Edges Do
    Visit(Edges);
  End;

Begin
 Result := False;

 VarName  := TVariable(VariablePnt).RefSymbol.Name;
 VarRange := TVariable(VariablePnt).RefSymbol.Range;

 Visited := TCFGNodeList.Create;
 Try
  Visit(rBeginNode);
 Finally
  Visited.Free;
 End;
End;

// -------------------------------------------------------------------------- //
(* TCFGNode.Create *)
Constructor TCFGNode.Create(const fParent: TCFGNode; const fName: String; const ffToken: PToken_P);
Begin
 Name := fName;

 Typ    := cetNone;
 Value  := nil;
 fToken := ffToken;
 Parent := fParent;

 Edges := TCFGNodeList.Create;
End;

(* TCFGNode.Create *)
Constructor TCFGNode.Create(const fParent: TCFGNode; const fName: String; const fTyp: TCFGNodeType; const fValue: PExpressionNode; const ffToken: PToken_P);
Begin
 Name := fName;

 Typ    := fTyp;
 Value  := fValue;
 fToken := ffToken;
 Parent := fParent;

 if (fToken = nil) Then
  if (Value <> nil) Then
   fToken := @Value^.Token;

 Edges := TCFGNodeList.Create;
End;

(* TCFGNode.isThere *)
Function TCFGNode.SearchFor(const NodeType: TCFGNodeType): TCFGNode;
Var Edge: TCFGNode;
Begin
 Result := nil;

 if (Typ = NodeType) Then
  Exit(self);

 For Edge in Edges Do
 Begin
  Result := Edge.SearchFor(NodeType);

  if (Result <> nil) Then
   Exit;
 End;
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

  if (Last.Typ = cetExpression) and (Last.Edges.Count <> 0) Then
   raise Exception.Create('TCFGraph.AddNode() -> expression node cannot have more than one edge!');

  if (Last.Typ = cetCondition) and (Last.Edges.Count >= 2) Then
   raise Exception.Create('TCFGraph.AddNode() -> condition node cannot have more than two edges!');

  if (Last.Typ = cetReturn) and (Last.Edges.Count <> 0) Then
   raise Exception.Create('TCFGraph.AddNode() -> return node cannot have more than one edge!');

  Last.Edges.Add(Node);
  Last := Node;
 End;
End;

(* TCFGraph.Validate *)
{ Validates the graph and - if needed - fixes things, that could break code generator or optimizers (like missing nodes etc.) }
Procedure TCFGraph.Validate;
Var VisitedNodes: TCFGNodeList;

  { FixTryCatch }
  Procedure FixTryCatch(const Node: TCFGNode);
  Var Edge: TCFGNode;
  Begin
   if (Node = nil) or (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Typ = cetTryCatch) and (Node.Edges.Count = 2) Then
   Begin
    (* @Note:

      In some specific cases, "try..catch" construction has only 2 edges, not 3; like here:

      function<void> foo()
      {
       try
       {
        a();
       } catch(msg)
       {
        b();
       }
      }
      (because no code appears after the try..catch construction)

      This could crash optimizer as well as the code generator (as they expect 'cetTryCatch'-typed nodes to have exactly 3 children), so we're just inserting a `nil` edge-node into this node.
    *)

    Node.Edges.Add(nil);
   End;

   For Edge in Node.Edges Do
    FixTryCatch(Edge);
  End;

  { FixForeach }
  Procedure FixForeach(const Node: TCFGNode);
  Var Edge: TCFGNode;
  Begin
   if (Node = nil) or (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Typ = cetForeach) and (Node.Edges.Count < 2) Then
    Node.Edges.Add(nil);

   For Edge in Node.Edges Do
    FixForeach(Edge);
  End;

Begin
 VisitedNodes := TCFGNodeList.Create;

 Try
  FixTryCatch(Root);

  VisitedNodes.Clear;
  FixForeach(Root);
 Finally
  VisitedNodes.Free;
 End;
End;

(* TCFGraph.CheckReturns *)
{
 Checks if every code path leads to a corresponding "return" statement; if not, displays appropiate compiler hint.
 Displays "unreachable code" messages as well when a code appears after some 'return' construction.
}
Procedure TCFGraph.CheckReturns(const CompilerPnt: Pointer; const isVoidOrNaked: Boolean);
Var Compiler        : TCompiler absolute CompilerPnt;
    isThereAnyReturn: Boolean = False;
    VisitedNodes    : TCFGNodeList;

  { Visit }
  Procedure Visit(Node: TCFGNode; const EndNode: TCFGNode);
  Var Edge: TCFGNode;
  Begin
   if (Node = nil) or (Node = EndNode) or (VisitedNodes.IndexOf(Node) <> -1) Then // if encountered nil node, end node or we're visiting one node for the second time, stop.
    Exit;
   VisitedNodes.Add(Node); // add node to the visited list

   { if 'return' }
   if (Node.Typ = cetReturn) Then
   Begin
    isThereAnyReturn := True;

    if (Node.Edges.Count = 1) Then // any code appearing after 'return' is 'unreachable'...
     if (Node.Edges[0] <> EndNode) and // ... if it isn't ending node
        (VisitedNodes.IndexOf(Node.Edges[0]) = -1) and // ... and if it hasn't been already visited
        (Node.Edges[0].Value <> nil) Then // ... and ofc. - if it's an expression
     Begin
      Compiler.CompileHint(Node.Edges[0].getToken, hUnreachableCode, []);
      VisitedNodes.Add(Node.Edges[0]);
     End;

    Exit;
   End;

   if (Node.Edges.Count = 0) and (Node.Value <> nil) Then // if it's an edge node with some expression and it isn't 'return', show warning
   Begin
    isThereAnyReturn := True; // otherwise the message below would be shown 2 times instead of one

    if (not isVoidOrNaked) Then
     Compiler.CompileWarning(Compiler.getScanner.next_pnt(-1), wNotEveryPathReturnsAValue, []);
   End;

   { if condition }
   if (Node.Typ = cetCondition) Then
   Begin
    Visit(Node.Edges[0], Node.Edges[2]);
    Visit(Node.Edges[1], Node.Edges[2]);
    Visit(Node.Edges[2], nil);
   End Else

   { if 'try..catch' }
   if (Node.Typ = cetTryCatch) Then
   Begin
    if (Node.Edges[0].SearchFor(cetReturn) <> nil) and (Node.Edges[1].SearchFor(cetReturn) <> nil) Then // if both nodes ("try" and "catch") return a value...
    Begin
     isThereAnyReturn := True;

     Node := Node.Edges[2];

     if (Node <> nil) and (Node.Value <> nil) Then
      Compiler.CompileHint(Node.getToken, hUnreachableCode, []);
    End Else
     Visit(Node.Edges[2], EndNode);
   End Else

   { if 'foreach' }
   if (Node.Typ = cetForeach) Then
   Begin
    Visit(Node.Edges[1], EndNode);
   End Else

    For Edge in Node.Edges Do // visit every edge
     Visit(Edge, EndNode);
  End;

Begin
 VisitedNodes := TCFGNodeList.Create;

 Try
  Visit(Root, nil);

  if (not isVoidOrNaked) and (not isThereAnyReturn) Then
   Compiler.CompileWarning(Compiler.getScanner.next_pnt(-1), wNotEveryPathReturnsAValue, []);
 Finally
  VisitedNodes.Free;
 End;
End;
End.
