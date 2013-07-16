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

   // @TODO: `i++;`, `i += constant_value` and so on

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

{ RemoveUnusedAssigns }
Procedure RemoveUnusedAssigns;
  Procedure Visit(Node: TCFGNode);
  Var Child, Back : TCFGNode;
      CanBeRemoved: Boolean;
      Symbol      : TSymbol;
      Second      : PExpressionNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   TCompiler(Compiler).fCurrentNode := Node;

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

       if (not isVariableUsed(TSymbol(Second^.Left^.Symbol).mVariable, Back, Node)) Then // if variable's value is not used between assignments, we can remove that first assign
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
     RemapSSA(Back, Back, Func.FlowGraph.Root, True);
     Back.Typ   := cetNone;
     Back.Value := nil; // @TODO: Dispose?
     AnyChange  := True;
    End;
   End;

   For Child in Node.Child Do
    Visit(Child);
  End;

Begin
 VisitedNodes.Clear;
 Visit(Func.FlowGraph.Root);
End;

Begin
 Repeat
  AnyChange := False;
  ConstantPropagation;
  ConstantFolding;
  RemoveUnusedAssigns;
 Until (not AnyChange); // repeat these two steps until no change is done.
End;
