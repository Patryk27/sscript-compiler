(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_RETURN;

 Interface

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses HLCompiler, ExpressionCompiler, Tokens, FlowGraph;

(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Begin
 With TCompiler(CompilerPnt), getScanner do
 Begin
  if (next_t = _SEMICOLON) Then // `return;` (aka "void return")
  Begin
   CFGAddNode(getCurrentFunction.createNode(fCurrentNode, cetReturn, nil, next_pnt));
  End Else // `return expression;`
  Begin
   CFGAddNode(getCurrentFunction.createNode(fCurrentNode, cetReturn, MakeExpression(TCompiler(CompilerPnt))));
  End;
 End;
End;
End.
