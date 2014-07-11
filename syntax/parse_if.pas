(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_IF;

 Interface

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses HLCompiler, Tokens, FlowGraph;

(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Var BaseNode, onTrue, onFalse, onTrueLast, onFalseLast: TCFGNode;
Begin
 With TCompiler(CompilerPnt), getScanner do
 Begin
  eat(_BRACKET1_OP); // (

  BaseNode := getCurrentFunction.createNode(fCurrentNode, cetCondition, readExpression([_BRACKET1_CL])); // 'if' main node

  onFalse     := getCurrentFunction.createNode(BaseNode, next_pnt);
  onFalseLast := onFalse;

  (* parse 'true' *)
  onTrue := getCurrentFunction.createNode(BaseNode, next_pnt);
  setNewRootNode(onTrue); // set new root node

  NewScope(sctIf); // new scope
  Inc(CurrentDeep);

  ParseCodeBlock(True); // parse code block

  Dec(CurrentDeep); // remove scope
  RemoveScope;

  onTrueLast := getCurrentNode;

  restorePrevRootNode; // restore previous root node

  (* parse 'else' (`false`), if possible *)
  if (next_t = _ELSE) Then // parse 'else' block
  Begin
   onFalse := getCurrentFunction.createNode(BaseNode, next_pnt);
   setNewRootNode(onFalse); // set new root node

   NewScope(sctIf); // new scope
   Inc(CurrentDeep);

   eat(_ELSE);

   ParseCodeBlock(True); // parse code block

   Dec(CurrentDeep); // remove scope
   RemoveScope;

   onFalseLast := getCurrentNode;

   restorePrevRootNode; // restore previous root node
  End;

  BaseNode.Edges.Add(onTrue);
  BaseNode.Edges.Add(onFalse);

  CFGAddNode(BaseNode);

  fCurrentNode := getCurrentFunction.createNode(BaseNode, next_pnt);

  BaseNode.Edges.Add(fCurrentNode);
  onTrueLast.Edges.Add(fCurrentNode);
  onFalseLast.Edges.Add(fCurrentNode);
 End;
End;
End.
