(* TreeSimplify *)
Function TreeSimplify: Boolean;

  { Visit }
  Procedure Visit(const Node: TCFGNode);
  Var Edge: TCFGNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then // if node has been visited more than once, don't check it again
    Exit;
   VisitedNodes.Add(Node);

   TCompiler(CompilerPnt).fCurrentNode := Node;

   if (Node.Value <> nil) Then
    Result := Result or ExpressionCompiler.OptimizeExpression(TCompiler(CompilerPnt), Node.Value, [oTreeSimplification]);

   For Edge in Node.Edges Do
    Visit(Edge);
  End;

Begin
 Result := False;

 VisitedNodes.Clear;
 Visit(Func.FlowGraph.Root);
End;

(* OptimizeExpressions *)
Procedure OptimizeExpressions;
Var AnyChange: Boolean;

{ ConstantPropagation }
Procedure ConstantPropagation;
Type PVarData = ^TVarData;
     TVarData = Record
                 Symbol: Pointer;
                 SSAId : TSSAVarID;
                 Value : PExpressionNode;
                End;
Type TVarList = specialize TFPGList<PVarData>;
Var VarList: TVarList;

  // FindVarData
  Function FindVarData(const Symbol: Pointer; const SSAId: TSSAVarID): PVarData;
  Begin
   For Result in VarList Do
    if (Result^.Symbol = Symbol) and (Result^.SSAId = SSAId) Then
     Exit;

   Exit(nil);
  End;

  // VisitExpression
  Procedure VisitExpression(var Expr: PExpressionNode);
  Var Param  : PExpressionNode;
      VarData: PVarData;
  Begin
   if (Expr = nil) Then
    Exit;

   { var = constant }
   if (Expr^.Typ = mtAssign) and (Expr^.Right^.isConstant) Then
   Begin
    New(VarData);
    VarData^.Symbol := Expr^.Left^.Symbol;
    VarData^.SSAId  := Expr^.Left^.SSA;
    VarData^.Value  := Expr^.Right;
    VarList.Add(VarData);
   End Else

   { var }
   if (Expr^.Typ = mtIdentifier) and (Expr^.IdentType = mtNothing) Then
   Begin
    VarData := FindVarData(Expr^.Symbol, Expr^.SSA);

    if (VarData <> nil) Then
    Begin
     Expr^.Value     := VarData^.Value^.Value;
     Expr^.IdentType := VarData^.Value^.Typ;
     Expr^.Typ       := Expr^.IdentType;
     AnyChange       := True;
    End;
   End;

   if (not (Expr^.Typ in MLValueOperators)) Then
    VisitExpression(Expr^.Left);

   VisitExpression(Expr^.Right);

   For Param in Expr^.ParamList Do
    VisitExpression(Param);
  End;

  // Visit
  Procedure Visit(const Node: TCFGNode);
  Var Edge: TCFGNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then // if node has been visited more than once, don't check it again
    Exit;
   VisitedNodes.Add(Node);

   VisitExpression(Node.Value);

   For Edge in Node.Edges Do
    Visit(Edge);
  End;

Var Tmp: PVarData;
Begin
 VisitedNodes.Clear;
 VarList := TVarList.Create;
 Try
  Visit(Func.FlowGraph.Root);
 Finally
  For Tmp in VarList Do
   Dispose(Tmp);
  VarList.Free;
 End;
End;

{ ConstantFolding }
Procedure ConstantFolding;

  // Visit
  Procedure Visit(const Node: TCFGNode);
  Var Edge: TCFGNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then // if node has been visited more than once, don't check it again
    Exit;
   VisitedNodes.Add(Node);

   TCompiler(CompilerPnt).fCurrentNode := Node;

   if (Node.Value <> nil) Then
    AnyChange := AnyChange or ExpressionCompiler.OptimizeExpression(TCompiler(CompilerPnt), Node.Value, [oInsertConstants, oConstantFolding]);

   For Edge in Node.Edges Do
    Visit(Edge);
  End;

Begin
 VisitedNodes.Clear;
 Visit(Func.FlowGraph.Root);
End;

Var Comp: TCompiler absolute CompilerPnt;
Begin
 Repeat
  AnyChange := False;

  if (Comp.getBoolOption(opt__remove_dead)) Then
   RemoveUnusedAssigns;

  if (Comp.getBoolOption(opt__constant_propagation)) Then
   ConstantPropagation;

  if (Comp.getBoolOption(opt__constant_folding)) Then
   ConstantFolding;
 Until (not AnyChange); // repeat these two steps until no change is done.
End;
