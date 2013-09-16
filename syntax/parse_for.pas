(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_FOR;

 Interface
 Uses SysUtils;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses SSCompiler, ExpressionCompiler, Tokens, Messages, Opcodes, FlowGraph;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Content, Condition, Step, EndingNode: TCFGNode;
Begin
With TCompiler(Compiler), Parser do
Begin
 eat(_BRACKET1_OP); // `(`

 Content    := TCFGNode.Create(nil, next_pnt);
 EndingNode := TCFGNode.Create(nil, next_pnt);

 Inc(CurrentDeep);

 (* parse init instruction *)
 ParsingFORInitInstruction := True;
 if (next_t in [_VAR, _SEMICOLON]) Then
  ParseToken Else
  CFGAddNode(TCFGNode.Create(fCurrentNode, cetExpression, MakeExpression(Compiler)));
 ParsingFORInitInstruction := False;

 (* parse condition *)
 if (next_t = _SEMICOLON) Then
 Begin
  eat(_SEMICOLON);
  Condition            := TCFGNode.Create(fCurrentNode, cetCondition, MakeBoolExpression(True, next_pnt(-1)));
  Condition.isVolatile := True; // the branch optimizer would enter an infinite loop trying to optimize this
 End Else
  Condition := TCFGNode.Create(fCurrentNode, cetCondition, MakeExpression(Compiler, [_SEMICOLON]));

 (* parse step instruction *)
 if (next_t = _BRACKET1_CL) Then
 Begin
  eat(_BRACKET1_CL);
  Step := nil;
 End Else
  Step := TCFGNode.Create(fCurrentNode, cetExpression, MakeExpression(Compiler, [_BRACKET1_CL]));

 (* parse loop's content *)
 setNewRootNode(Content);

 if (Step = nil) Then
  NewScope(sFOR, Content, EndingNode) Else
  NewScope(sFOR, Step, EndingNode);

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

 Condition.Child.Add(Content); // on true
 Condition.Child.Add(EndingNode); // on false
 Condition.Child.Add(nil);
End;
End;
End.
