(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_TRY_CATCH;

 Interface
 Uses SysUtils;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses SSCompiler, FlowGraph, symdef, Opcodes, Tokens;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Symbol                  : TSymbol;
    Node, TryNode, CatchNode: TCFGNode;
Begin
With TCompiler(Compiler), Parser do
Begin
 NewScope(sTryCatch);

 (* parse 'try' *)
 TryNode := TCFGNode.Create(fCurrentNode, next_pnt);
 setNewRootNode(TryNode);
 ParseCodeBlock; // parse code block
 restorePrevRootNode;

 (* parse 'catch' *)
 Inc(CurrentDeep);

 eat(_CATCH); // `catch`
 eat(_BRACKET1_OP); // `(`

 Symbol           := TSymbol.Create(stVariable); // create new local symbol
 Symbol.Name      := read_ident; // [var name]
 Symbol.DeclToken := next_pnt;

 RedeclarationCheck(Symbol.Name); // redeclaration check

 With Symbol.mVariable Do
 Begin
  Typ        := TYPE_STRING;
  MemPos     := 0;
  Attributes := [vaDontAllocate, vaCatchVar, vaVolatile]; // we'll allocate this variable by ourselves, and also we don't let optimizer touch it - just to be sure.
 End;

 getCurrentFunction.SymbolList.Add(Symbol); // add symbol into the function's symbol list

 eat(_BRACKET1_CL); // `)`

 Symbol.Range := Parser.getCurrentRange(0);

 // parse 'catch' code block
 CatchNode := TCFGNode.Create(fCurrentNode, next_pnt);
 setNewRootNode(CatchNode);
 ParseCodeBlock; // parse code block
 restorePrevRootNode;

 (* do some CFG-magic *)
 Node := TCFGNode.Create(fCurrentNode, cetTryCatch, nil, TryNode.getToken);

 Node.Child.Add(TryNode);
 Node.Child.Add(CatchNode);

 TryNode.Parent   := Node;
 CatchNode.Parent := Node;

 CFGAddNode(Node);

 // ... and, of course, remove scope
 Dec(CurrentDeep);
 RemoveScope;
End;
End;

End.
