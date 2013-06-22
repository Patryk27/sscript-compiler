(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_FOR;

 Interface
 Uses SysUtils;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, Tokens, Messages, Opcodes, cfgraph;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Content, Condition, Step, EndingNode: TCFGNode;
Begin
With TCompiler(Compiler), Parser do
Begin
 eat(_BRACKET1_OP); // (

 Content    := TCFGNode.Create(nil, next_pnt);
 EndingNode := TCFGNode.Create(nil, next_pnt);

 Inc(CurrentDeep);

 (* parse init instruction *)
 ParsingFORInitInstruction := True; // don't even ask...
 if (next_t in [_VAR, _SEMICOLON]) Then
  ParseToken Else
  CFGAddNode(TCFGNode.Create(fCurrentNode, cetExpression, MakeExpression(Compiler)));
 ParsingFORInitInstruction := False;

 (* parse condition *)
 if (next_t = _SEMICOLON) Then
 Begin
  eat(_SEMICOLON);
  Condition := TCFGNode.Create(fCurrentNode, cetCondition, MakeBoolExpression(True, next_pnt(-1)));
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

 if (Step <> nil) Then // add step instruction
  CFGAddNode(Step);

 if (Condition = nil) Then // check condition
  CFGAddNode(Content) Else
  CFGAddNode(Condition);

 restorePrevRootNode;

 (* do some control-flow-graph magic *)
 if (Condition = nil) Then
  CFGAddNode(Content) Else
  CFGAddNode(Condition);

 EndingNode.Parent := fCurrentNode;
 fCurrentNode      := EndingNode;

 if (Condition <> nil) Then
 Begin
  Condition.Child.Add(Content); // on true
  Condition.Child.Add(EndingNode); // on false
  Condition.Child.Add(EndingNode);
 End;
End;
End;
End.
