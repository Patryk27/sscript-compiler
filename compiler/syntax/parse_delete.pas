(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_DELETE;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, Expression, ExpressionCompiler, Tokens, cfgraph;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Expr: PExpressionNode;
Begin
With TCompiler(Compiler), Parser do
Begin
 While (true) Do
 Begin
  Expr := ExpressionCompiler.MakeExpression(Compiler, [_SEMICOLON, _COMMA]); // expression to be freed

  CFGAddNode(TCFGNode.Create(fCurrentNode, cetObjDelete, Expr));

  Dec(TokenPos);
  if (next_t = _SEMICOLON) Then
   Break Else
   eat(_COMMA);
 End;
 eat(_SEMICOLON);
End;
End;
End.
