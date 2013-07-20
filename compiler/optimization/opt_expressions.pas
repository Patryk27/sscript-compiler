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
  Procedure VisitExpression(Expr: PExpressionNode);
  Var Param  : PExpressionNode;
      VarData: PVarData;
  Begin
   if (Expr = nil) Then
    Exit;

   if (Expr^.Typ = mtAssign) and (Expr^.Right^.isConstant) Then
   Begin
    New(VarData);
    VarData^.Symbol := Expr^.Left^.Symbol;
    VarData^.SSAId  := Expr^.Left^.SSA;
    VarData^.Value  := Expr^.Right;
    VarList.Add(VarData);
   End Else

   if (Expr^.Typ = mtIdentifier) and (Expr^.IdentType = mtNothing) Then
   Begin
    VarData := FindVarData(Expr^.Symbol, Expr^.SSA);

    if (VarData <> nil) Then
    Begin
     Expr^.Value     := VarData^.Value^.Value;
     Expr^.IdentType := VarData^.Value^.Typ;
     AnyChange       := True;
    End;
   End;

   VisitExpression(Expr^.Left);
   VisitExpression(Expr^.Right);

   For Param in Expr^.ParamList Do
    VisitExpression(Param);
  End;

  // Visit
  Procedure Visit(Node: TCFGNode);
  Var Child: TCFGNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then // if node has been visited more than once, don't check it again
    Exit;
   VisitedNodes.Add(Node);

   VisitExpression(Node.Value);

   For Child in Node.Child Do
    Visit(Child);
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
  Procedure Visit(Node: TCFGNode);
  Var Child: TCFGNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then // if node has been visited more than once, don't check it again
    Exit;
   VisitedNodes.Add(Node);

   TCompiler(Compiler).fCurrentNode := Node;

   if (Node.Value <> nil) Then
    AnyChange := AnyChange or ExpressionCompiler.OptimizeExpression(TCompiler(Compiler), Node.Value, [oInsertConstants, oConstantFolding]);

   For Child in Node.Child Do
    Visit(Child);
  End;

Begin
 VisitedNodes.Clear;
 Visit(Func.FlowGraph.Root);
End;

{ TreeSimplify }
Procedure TreeSimplify;
  Procedure Visit(Node: TCFGNode);
  Var Child: TCFGNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then // if node has been visited more than once, don't check it again
    Exit;
   VisitedNodes.Add(Node);

   TCompiler(Compiler).fCurrentNode := Node;

   if (Node.Value <> nil) Then
    AnyChange := AnyChange or ExpressionCompiler.OptimizeExpression(TCompiler(Compiler), Node.Value, [oTreeSimplification]);

   For Child in Node.Child Do
    Visit(Child);
  End;

Begin
 VisitedNodes.Clear;
 Visit(Func.FlowGraph.Root);
End;

Var Comp: TCompiler absolute Compiler;
Begin
 Repeat
  AnyChange := False;

  if (Comp.getBoolOption(opt__remove_dead)) Then
   RemoveUnusedAssigns;

  if (Comp.getBoolOption(opt__constant_propagation)) Then
   ConstantPropagation;

  if (Comp.getBoolOption(opt__constant_folding)) Then
   ConstantFolding;

  if (Comp.getBoolOption(opt__tree_simplify)) Then
   TreeSimplify;
 Until (not AnyChange); // repeat these two steps until no change is done.
End;
