// @TODO: they're a bit lame solutions, because they distracts optimizer from doing its job ;/

{ ParseBreak }
Procedure ParseBreak;
Var I   : Integer;
    Node: TCFGNode;
Begin
 For I := High(Scope) Downto Low(Scope) Do
  if (Scope[I].Typ in [sFOR, sFOREACH, sWHILE]) Then
  Begin
   Node := TCFGNode.Create(getCurrentNode, Scanner.next_pnt);

   With Node do
   Begin
    Typ := cetBytecode;

    With Bytecode do
    Begin
     OpcodeName := 'jmp';
     New(OpcodeArgList);
     SetLength(OpcodeArgList^, 1);
     OpcodeArgList^[0].VType  := vtPChar;
     OpcodeArgList^[0].VPChar := CopyStringToPChar(':'+Scope[I].LoopEnd.getName);
    End;
   End;

   CFGAddNode(Node);

   Scanner.eat(_SEMICOLON);
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
  if (Scope[I].Typ in [sFOR, sFOREACH, sWHILE]) Then
  Begin
   Node := TCFGNode.Create(getCurrentNode, Scanner.next_pnt);

   With Node do
   Begin
    Typ := cetBytecode;

    With Bytecode do
    Begin
     OpcodeName := 'jmp';
     New(OpcodeArgList);
     SetLength(OpcodeArgList^, 1);
     OpcodeArgList^[0].VType  := vtPChar;
     OpcodeArgList^[0].VPChar := CopyStringToPChar(':'+Scope[I].LoopBegin.getName);
    End;
   End;

   CFGAddNode(Node);

   Scanner.eat(_SEMICOLON);
   Exit;
  End;

 CompileError(eNotAllowed, ['continue']);
End;
