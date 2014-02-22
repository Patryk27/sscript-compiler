(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_TRY_CATCH;

 Interface
 Uses SysUtils;

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses SSCompiler, FlowGraph, symdef, Opcodes, Tokens;

(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Var Symbol                  : TSymbol;
    Node, TryNode, CatchNode: TCFGNode;
Begin
 With TCompiler(CompilerPnt), getScanner do
 Begin
  NewScope(sctTryCatch);

  (* parse 'try' *)
  TryNode := getCurrentFunction.createNode(fCurrentNode, next_pnt);
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
   Typ                        := TYPE_STRING;
   LocationData.Location      := vlStack;
   LocationData.StackPosition := 0;
   Attributes                 := [vaDontAllocate, vaCatchVar, vaVolatile]; // we're allocating this variable by ourselves and also we don't let optimizer touch it - just to be sure it doesn't decide to remove it or do something worse.
  End;

  getCurrentFunction.SymbolList.Add(Symbol); // add symbol into the function's symbol list

  eat(_BRACKET1_CL); // `)`

  Symbol.Range := getScanner.getCurrentRange(0);

  // parse 'catch' code block
  CatchNode := getCurrentFunction.createNode(fCurrentNode, next_pnt);
  setNewRootNode(CatchNode);
  ParseCodeBlock; // parse code block
  restorePrevRootNode;

  (* do some CFG-magic *)
  Node := getCurrentFunction.createNode(fCurrentNode, cetTryCatch, nil, TryNode.getToken);

  Node.Edges.Add(TryNode);
  Node.Edges.Add(CatchNode);

  TryNode.Parent   := Node;
  CatchNode.Parent := Node;

  CFGAddNode(Node);

  // ... and, of course, remove scope
  Dec(CurrentDeep);
  RemoveScope;
 End;
End;
End.
