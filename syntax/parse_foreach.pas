(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_FOREACH;

 Interface

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses HLCompiler, ExpressionCompiler, FlowGraph, Messages, symdef, Tokens;

(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Var Foreach, Content, EndNode                  : TCFGNode;
    LoopIterVar, LoopExprHolder, LoopSizeHolder: TVariable;
    VarName                                    : String;
Begin
 With TCompiler(CompilerPnt), getScanner do
 Begin
  eat(_BRACKET1_OP); // `(`

  Foreach := getCurrentFunction.createNode(getCurrentNode, cetForeach, nil, next_pnt);
  Content := getCurrentFunction.createNode(Foreach, next_pnt);
  EndNode := getCurrentFunction.createNode(nil, next_pnt);

 // NewScope(sFOREACH, Content, EndNode);
  Inc(CurrentDeep);

  (* parse parenthesis *)
  ParsingForeachHeader := True;
  if (next_t in [_VAR, _SEMICOLON]) Then
  Begin
   ParseToken;
   Foreach.Foreach.LoopVar := getCurrentFunction.SymbolList.Last.mVariable;
  End Else
  Begin
   VarName                 := read_ident;
   Foreach.Foreach.LoopVar := findVariableCandidate(VarName, nil, next(0)); // @TODO: namespace res operator (`::`)

   if (Foreach.Foreach.LoopVar = nil) Then
   Begin
    CompileError(eUnknownVariable, [VarName]);
    Exit;
   End;
  End;

  ParsingForeachHeader := False;

  // create iteration variable
  LoopIterVar                 := TVariable.Create;
  LoopIterVar.Typ             := TYPE_INT;
  LoopIterVar.Attributes      += [vaVolatile];
  LoopIterVar.RefSymbol.Name  := '$foreach_'+Foreach.getName+'.iterator';
  LoopIterVar.RefSymbol.Range := TVariable(Foreach.Foreach.LoopVar).RefSymbol.Range;

  LoopExprHolder                 := TVariable.Create;
  LoopExprHolder.Typ             := TYPE_ANY;
  LoopExprHolder.Attributes      += [vaVolatile];
  LoopExprHolder.RefSymbol.Name  := '$foreach_'+Foreach.getName+'.exprholder';
  LoopExprHolder.RefSymbol.Range := TVariable(Foreach.Foreach.LoopVar).RefSymbol.Range;

  LoopSizeHolder                 := TVariable.Create;
  LoopSizeHolder.Typ             := TYPE_INT;
  LoopSizeHolder.Attributes      += [vaVolatile];
  LoopSizeHolder.RefSymbol.Name  := '$foreach_'+Foreach.getName+'.sizeholder';
  LoopSizeHolder.RefSymbol.Range := TVariable(Foreach.Foreach.LoopVar).RefSymbol.Range;

  Foreach.Foreach.LoopIterVar    := LoopIterVar;
  Foreach.Foreach.LoopExprHolder := LoopExprHolder;
  Foreach.Foreach.LoopSizeHolder := LoopSizeHolder;

  With getCurrentFunction.SymbolList do
  Begin
   Add(TSymbol.Create(stVariable, Foreach.Foreach.LoopIterVar));
   Add(TSymbol.Create(stVariable, Foreach.Foreach.LoopExprHolder));
   Add(TSymbol.Create(stVariable, Foreach.Foreach.LoopSizeHolder));
  End;

  eat(_IN); // `in`

  Foreach.Value := MakeExpression(CompilerPnt, [_BRACKET1_CL]);

  (* parse loop content *)
  setNewRootNode(Content);

  ParseCodeBlock(True); // parse 'foreach' loop

  Dec(CurrentDeep);
 // RemoveScope;

  restorePrevRootNode;

  (* do some control-flow-graph magic *)
  CFGAddNode(Foreach);
  Foreach.Edges.Add(Content);
  Foreach.Edges.Add(EndNode);

  EndNode.Parent := fCurrentNode;
  fCurrentNode   := EndNode;
 End;
End;
End.
