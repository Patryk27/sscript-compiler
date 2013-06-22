(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_WHILE;

 Interface
 Uses SysUtils;

 Procedure Parse(Compiler: Pointer);
 Procedure Parse_DO_WHILE(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, Tokens, Messages, Opcodes, cfgraph;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var ConditionNode, CondTrue, CondFalse, CondTrueLast: TCFGNode;
Begin
With TCompiler(Compiler), Parser do
Begin
 eat(_BRACKET1_OP); // (

 (* parse loop condition *)
 ConditionNode := TCFGNode.Create(fCurrentNode, cetCondition, MakeExpression(Compiler, [_BRACKET1_CL]));
 CondFalse     := TCFGNode.Create(ConditionNode, next_pnt); // dummy

 (* parse loop content *)
 CondTrue := TCFGNode.Create(ConditionNode, next_pnt);
 setNewRootNode(CondTrue);

 NewScope(sWHILE, ConditionNode, CondFalse);
 Inc(CurrentDeep);
 ParseCodeBlock(True); // parse loop's content
 Dec(CurrentDeep);
 RemoveScope;

 CondTrueLast := getCurrentNode;

 restorePrevRootNode;

 (* do magic *)
 ConditionNode.Child.Add(CondTrue);
 ConditionNode.Child.Add(CondFalse);

 CFGAddNode(ConditionNode);

 fCurrentNode := TCFGNode.Create(ConditionNode);

 ConditionNode.Child.Add(fCurrentNode);
 CondTrueLast.Child.Add(ConditionNode);
 CondFalse.Child.Add(fCurrentNode);
End;
End;

{ Parse_DO_WHILE }
Procedure Parse_DO_WHILE(Compiler: Pointer);
Var BaseNode, BaseNodeLast, ConditionNode, CondFalse: TCFGNode;
Begin
With TCompiler(Compiler), Parser do
Begin
 (* parse loop content *)
 BaseNode      := TCFGNode.Create(fCurrentNode, next_pnt);
 ConditionNode := TCFGNode.Create(fCurrentNode, cetCondition, nil);
 CondFalse     := TCFGNode.Create(ConditionNode, next_pnt); // dummy

 setNewRootNode(BaseNode);

 NewScope(sWHILE, ConditionNode, CondFalse);
 Inc(CurrentDeep);
 ParseCodeBlock(True); // parse loop's content
 Dec(CurrentDeep);
 RemoveScope;

 BaseNodeLast := getCurrentNode;

 restorePrevRootNode;

 (* parse loop condition *)
 eat(_WHILE);
 eat(_BRACKET1_OP);

 ConditionNode.Value := MakeExpression(Compiler, [_BRACKET1_CL]);

 (* do magic *)
 BaseNodeLast.Child.Add(ConditionNode);

 ConditionNode.Child.Add(BaseNode); // on true
 ConditionNode.Child.Add(CondFalse); // on false

 CFGAddNode(BaseNode);

 fCurrentNode := TCFGNode.Create(ConditionNode);

 ConditionNode.Child.Add(fCurrentNode);
 CondFalse.Child.Add(fCurrentNode);
End;
End;
End.
