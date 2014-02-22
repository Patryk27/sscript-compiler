(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_WHILE;

 Interface
 Uses SysUtils;

 Procedure Parse(const CompilerPnt: Pointer);
 Procedure Parse_DO_WHILE(const CompilerPnt: Pointer);

 Implementation
Uses SSCompiler, ExpressionCompiler, Tokens, Messages, Opcodes, FlowGraph;

(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Var ConditionNode, CondTrue, CondFalse, CondTrueLast: TCFGNode;
Begin
 With TCompiler(CompilerPnt), getScanner do
 Begin
  eat(_BRACKET1_OP); // (

  (* parse loop condition *)
  ConditionNode := getCurrentFunction.createNode(fCurrentNode, cetCondition, MakeExpression(CompilerPnt, [_BRACKET1_CL]));
  CondFalse     := getCurrentFunction.createNode(ConditionNode, next_pnt); // dummy

  ConditionNode.isVolatile := True;

  (* parse loop content *)
  CondTrue := getCurrentFunction.createNode(ConditionNode, next_pnt);
  setNewRootNode(CondTrue);

  NewScope(sctWhile, ConditionNode, CondFalse);
  Inc(CurrentDeep);
  ParseCodeBlock(True); // parse loop's content
  Dec(CurrentDeep);
  RemoveScope;

  CondTrueLast := getCurrentNode;

  restorePrevRootNode;

  (* do magic *)
  ConditionNode.Edges.Add(CondTrue);
  ConditionNode.Edges.Add(CondFalse);

  CFGAddNode(ConditionNode);

  fCurrentNode := getCurrentFunction.createNode(ConditionNode);

  ConditionNode.Edges.Add(fCurrentNode);
  CondTrueLast.Edges.Add(ConditionNode);
  CondFalse.Edges.Add(fCurrentNode);
 End;
End;

(* Parse_DO_WHILE *)
Procedure Parse_DO_WHILE(const CompilerPnt: Pointer);
Var BaseNode, BaseNodeLast, ConditionNode, CondFalse: TCFGNode;
Begin
 With TCompiler(CompilerPnt), getScanner do
 Begin
  (* parse loop content *)
  BaseNode      := getCurrentFunction.createNode(fCurrentNode, next_pnt);
  ConditionNode := getCurrentFunction.createNode(fCurrentNode, cetCondition, nil);
  CondFalse     := getCurrentFunction.createNode(ConditionNode, next_pnt); // dummy

  ConditionNode.isVolatile := True;

  setNewRootNode(BaseNode);

  NewScope(sctWhile, ConditionNode, CondFalse);
  Inc(CurrentDeep);
  ParseCodeBlock(True); // parse loop's content
  Dec(CurrentDeep);
  RemoveScope;

  BaseNodeLast := getCurrentNode;

  restorePrevRootNode;

  (* parse loop condition *)
  eat(_WHILE);
  eat(_BRACKET1_OP);

  ConditionNode.Value := MakeExpression(CompilerPnt, [_BRACKET1_CL]);

  (* do magic *)
  BaseNodeLast.Edges.Add(ConditionNode);

  ConditionNode.Edges.Add(BaseNode); // on true
  ConditionNode.Edges.Add(CondFalse); // on false

  CFGAddNode(BaseNode);

  fCurrentNode := getCurrentFunction.createNode(ConditionNode);

  ConditionNode.Edges.Add(fCurrentNode);
  CondFalse.Edges.Add(fCurrentNode);
 End;
End;
End.
