(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_THROW;

 Interface
 Uses SysUtils;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses SSCompiler, ExpressionCompiler, Tokens, FlowGraph;

{ Parse }
Procedure Parse(Compiler: Pointer);
Begin
 With TCompiler(Compiler) do
  CFGAddNode(TCFGNode.Create(fCurrentNode, cetThrow, MakeExpression(Compiler)));
End;

End.
