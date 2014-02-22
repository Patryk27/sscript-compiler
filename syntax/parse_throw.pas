(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_THROW;

 Interface
 Uses SysUtils;

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses SSCompiler, ExpressionCompiler, Tokens, FlowGraph;

(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Begin
 With TCompiler(CompilerPnt) do
 Begin
  CFGAddNode(getCurrentFunction.createNode(fCurrentNode, cetThrow, MakeExpression(CompilerPnt)));
 End;
End;

End.
