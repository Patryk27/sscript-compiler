// @TODO: they're lame solutions, because they distract optimizers from doing their job ;/

{ ParseBreak }
Procedure ParseBreak;
Var I   : Integer;
    Node: TCFGNode;
Begin
 For I := High(Scope) Downto Low(Scope) Do
  if (Scope[I].Typ in [sctFor, sctForeach, sctWhile]) Then
  Begin
   Node := getCurrentFunction.createNode(getCurrentNode, Scanner.next_pnt);

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

 CompileError(eNotAllowed, ['break']); // error: instruction not allowed
End;

{ ParseContinue }
Procedure ParseContinue;
Var I   : Integer;
    Node: TCFGNode;
Begin
 For I := High(Scope) Downto Low(Scope) Do
  if (Scope[I].Typ in [sctFor, sctForeach, sctWhile]) Then
  Begin
   Node := getCurrentFunction.createNode(getCurrentNode, Scanner.next_pnt);

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

 CompileError(eNotAllowed, ['continue']); // error: instruction not allowed
End;
