(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_RETURN;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses SSCompiler, ExpressionCompiler, Tokens, FlowGraph;

(* Parse *)
Procedure Parse(Compiler: Pointer);
Begin
 With TCompiler(Compiler), getScanner do
 Begin
  if (next_t = _SEMICOLON) Then // `return;` (aka "void return")
  Begin
   CFGAddNode(TCFGNode.Create(fCurrentNode, cetReturn, nil, next_pnt));
  End Else // `return expression;`
  Begin
   CFGAddNode(TCFGNode.Create(fCurrentNode, cetReturn, MakeExpression(TCompiler(Compiler))));
  End;
 End;
End;
End.
