(* GenerateSSA *)
{
 Converts control flow graph to the single static assignment form.
}
Procedure GenerateSSA;

  { Stage1 }
  Procedure Stage1; // stage 1: generate SSA for variable assignments etc.
  {$I ssa_stage1.pas}
  Begin
   Execute;
  End;

  { Stage2 }
  Procedure Stage2; // stage 2: genretae SSA for the rest of the code
  {$I ssa_stage2.pas}
  Begin
   Execute;
  End;

Begin
 Stage1;
 Stage2;
End;

(* RemapSSA *)
{
 Removes all unreachable SSA definitions from specified range

 First, it gathers all the assignments (and so on) from nodes SSARemapFrom -> SSARemapTo, and
 in the second stage, it removes all the SSA uses of that variables in SSARemapBegin -> end of the control flow graph.

 Should be called when removing a node(s) or assignment(s) (including operators like `*=` or `++`, see: Expression.MLValueOperators).
}
Procedure RemapSSA(SSARemapFrom, SSARemapTo, SSARemapBegin{, SSARemapEnd}: TCFGNode; const VisitEndNode: Boolean=False);
Type PSSAData = ^TSSAData;
     TSSAData = Record
                 Symbol: Pointer;
                 SSA   : Integer;
                End;
Type TSSADataList = specialize TFPGList<PSSAData>;
Var Stage      : 1..2;
    SSADataList: TSSADataList;

  // ShouldBeRemoved
  Function ShouldBeRemoved(const Symbol: Pointer; const SSA: Integer): Boolean;
  Var Data: PSSAData;
  Begin
   Result := False;

   For Data in SSADataList Do
    if (Data^.Symbol = Symbol) and (Data^.SSA = SSA) Then
     Exit(True);
  End;

  // RemoveElement
  Type TArray = Array of LongWord;
  Procedure RemoveElement(var A: TArray; Index: Integer);
  Var Len, I: Integer;
  Begin
   Len := High(A);
   For I := Index+1 To Len Do
    A[I-1] := A[I];
   SetLength(A, Len);
  End;

  // VisitExpression
  Procedure VisitExpression(const Expr: PExpressionNode);
  Var Param: PExpressionNode;
      I    : Integer;
      Data : PSSAData;
  Begin
   if (Expr = nil) Then
    Exit;

   Case Stage of
    1:
     if (Expr^.Typ in MLValueOperators) Then
     Begin
      For I := 0 To High(Expr^.Left^.SSA.Values) Do
      Begin
       New(Data);
       Data^.Symbol := Expr^.Left^.Symbol;
       Data^.SSA    := Expr^.Left^.SSA.Values[I];
       SSADataList.Add(Data);
      End;
     End;

    2:
     if (Expr^.Typ = mtIdentifier) Then
     Begin
      I := 0;
      While (I < Length(Expr^.SSA.Values)) Do
      Begin
       if (ShouldBeRemoved(Expr^.Symbol, Expr^.SSA.Values[I])) Then
        RemoveElement(Expr^.SSA.Values, I) Else
        Inc(I);
      End;

      if (Length(Expr^.SSA.Values) = 0) Then
       DevLog(dvWarning, 'RemapSSA', 'SSA remapping may failed: some variable use (at line '+IntToStr(Expr^.Token.Line)+') has been left without a corresponding SSA ID; this may lead to undefined behavior unless optimizer takes care of it.');
     End;
   End;

   VisitExpression(Expr^.Left);
   VisitExpression(Expr^.Right);
   For Param in Expr^.ParamList Do
    VisitExpression(Param);
  End;

  // VisitNode
  Procedure VisitNode(const Node, EndNode: TCFGNode);
  Var Child: TCFGNode;
  Begin
   if (Node = nil) or (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node = EndNode) Then
   Begin
    Case VisitEndNode of
     True:
     Begin
      VisitExpression(Node.Value);
      Exit;
     End;

     False:
      Exit;
    End;
   End Else
    VisitExpression(Node.Value);

   For Child in Node.Child Do
    VisitNode(Child, EndNode);
  End;

Var Tmp: PSSAData;
Begin
 SSADataList := TSSADataList.Create;

 Try
  // stage 1: gather all the SSA assignments that will be removed when we remove nodes from SSARemapFrom -> SSARemapTo
  Stage := 1;
  VisitedNodes.Clear;
  VisitNode(SSARemapFrom, SSARemapTo);

  // stage 2: alter all nodes appearing after SSARemapBegin
  Stage := 2;
  VisitedNodes.Clear;
  VisitNode(SSARemapBegin, nil);
 Finally
  // stage 3: clear everything up
  For Tmp in SSADataList Do
   Dispose(Tmp);
  SSADataList.Free;
 End;
End;
