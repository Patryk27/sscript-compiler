(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_IF;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses SSCompiler, ExpressionCompiler, Tokens, FlowGraph;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var BaseNode, onTrue, onFalse, onTrueLast, onFalseLast: TCFGNode;
Begin
With TCompiler(Compiler), Parser do
Begin
 eat(_BRACKET1_OP); // (

 BaseNode := TCFGNode.Create(fCurrentNode, cetCondition, MakeExpression(Compiler, [_BRACKET1_CL])); // 'if' main node

 onFalse     := TCFGNode.Create(BaseNode, next_pnt);
 onFalseLast := onFalse;

 (* parse 'true' *)
 onTrue := TCFGNode.Create(BaseNode, next_pnt);
 setNewRootNode(onTrue); // set new root node

 NewScope(sIF); // new scope
 Inc(CurrentDeep);

 ParseCodeBlock(True); // parse code block

 Dec(CurrentDeep); // remove scope
 RemoveScope;

 onTrueLast := getCurrentNode;

 restorePrevRootNode; // restore previous root node

 (* parse 'else' (`false`), if possible *)
 if (next_t = _ELSE) Then // parse 'else' block
 Begin
  onFalse := TCFGNode.Create(BaseNode, next_pnt);
  setNewRootNode(onFalse); // set new root node

  NewScope(sIF); // new scope
  Inc(CurrentDeep);

  eat(_ELSE);

  ParseCodeBlock(True); // parse code block

  Dec(CurrentDeep); // remove scope
  RemoveScope;

  onFalseLast := getCurrentNode;

  restorePrevRootNode; // restore previous root node
 End;

 BaseNode.Child.Add(onTrue);
 BaseNode.Child.Add(onFalse);

 CFGAddNode(BaseNode);

 fCurrentNode := TCFGNode.Create(BaseNode, next_pnt);

 BaseNode.Child.Add(fCurrentNode);
 onTrueLast.Child.Add(fCurrentNode);
 onFalseLast.Child.Add(fCurrentNode);
End;
End;
End.
