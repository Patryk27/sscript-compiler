(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_FOR;

 Interface
 Uses SysUtils;

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses HLCompiler, Expression, Tokens, Messages, Opcodes, FlowGraph;

(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Var Content, Condition, Step, EndingNode: TCFGNode;
Begin
 With TCompiler(CompilerPnt), getScanner do
 Begin
  eat(_BRACKET1_OP); // `(`

  Content    := getCurrentFunction.createNode(nil, next_pnt);
  EndingNode := getCurrentFunction.createNode(nil, next_pnt);

  Inc(CurrentDeep);

  (* parse init instruction *)
  ParsingFORInitInstruction := True;
  if (next_t in [_VAR, _SEMICOLON]) Then
   ParseToken Else
   CFGAddNode(getCurrentFunction.createNode(fCurrentNode, cetExpression, readExpression()));
  ParsingFORInitInstruction := False;

  (* parse condition *)
  if (next_t = _SEMICOLON) Then
  Begin
   eat(_SEMICOLON);

   Condition            := getCurrentFunction.createNode(fCurrentNode, cetCondition, TBooleanExpressionNode.Create(getExpressionCompiler, next(-1), True));
   Condition.isVolatile := True; // the branch optimizer would enter an infinite loop trying to optimize this
  End Else
  Begin
   Condition := getCurrentFunction.createNode(fCurrentNode, cetCondition, readExpression([_SEMICOLON]));
  End;

  (* parse step instruction *)
  if (next_t = _BRACKET1_CL) Then
  Begin
   eat(_BRACKET1_CL);
   Step := nil;
  End Else
  Begin
   Step := getCurrentFunction.createNode(fCurrentNode, cetExpression, readExpression([_BRACKET1_CL]));
  End;

  (* parse loop's content *)
  setNewRootNode(Content);

  if (Step = nil) Then
   NewScope(sctFor, Content, EndingNode) Else
   NewScope(sctFor, Step, EndingNode);

  ParseCodeBlock(True); // parse 'for' loop

  Dec(CurrentDeep);
  RemoveScope;

  if (Step <> nil) Then
  Begin
   Step.Parent := fCurrentNode;
   CFGAddNode(Step);
  End;

  CFGAddNode(Condition);

  restorePrevRootNode;

  (* do some control-flow-graph magic *)
  Content.Parent := Condition;

  CFGAddNode(Condition);

  EndingNode.Parent := fCurrentNode;
  fCurrentNode      := EndingNode;

  Condition.Edges.Add(Content); // on true
  Condition.Edges.Add(EndingNode); // on false
  Condition.Edges.Add(nil);
 End;
End;
End.
