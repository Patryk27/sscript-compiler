{ ParseBreak }
Procedure ParseBreak;
Var I   : Integer;
    Node: TCFGNode;
Begin
 For I := High(Scope) Downto Low(Scope) Do
  if (Scope[I].Typ in [sFOR, sWHILE]) Then
  Begin
   Node := TCFGNode.Create(getCurrentNode, Parser.next_pnt);
   Node.Child.Add(Scope[I].LoopEnd);
   CFGAddNode(Node);

   Parser.eat(_SEMICOLON);
   Exit;
  End;

 CompileError(eNotAllowed, ['break']);
End;

{ ParseContinue }
Procedure ParseContinue;
Var I   : Integer;
    Node: TCFGNode;
Begin
 For I := High(Scope) Downto Low(Scope) Do
  if (Scope[I].Typ in [sFOR, sWHILE]) Then
  Begin
   Node := TCFGNode.Create(getCurrentNode, Parser.next_pnt);
   Node.Child.Add(Scope[I].LoopBegin);
   CFGAddNode(Node);

   Parser.eat(_SEMICOLON);
   Exit;
  End;

 CompileError(eNotAllowed, ['continue']);
End;
