(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_VAR;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses SSCompiler, ExpressionCompiler, Tokens, symdef, Messages, Opcodes, FlowGraph;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Variable: TVariable;
    VarType : TType;
Begin
With TCompiler(Compiler), Parser do
Begin
 if (not ((CompilePass = _cp1) or (inFunction))) Then // variables are parsed in the first pass or inside a function
 Begin
  read_until(_SEMICOLON);
  Exit;
 End;

 eat(_LOWER); // <
 VarType := read_type; // [type]
 eat(_GREATER); // >

 { read variables }
 While (true) do
 Begin
  Variable     := TVariable.Create;
  Variable.Typ := VarType.Clone;

  With Variable.RefSymbol do
  Begin
   DeclToken     := next_pnt;
   DeclNamespace := getCurrentNamespace;
   DeclFunction  := getCurrentFunction;
   mCompiler     := Compiler;
   Range         := getCurrentRange;
   Name          := read_ident; // [identifier]

   RedeclarationCheck(Name); // redeclaration of the variable
  End;

  if (Variable.Typ.isVoid) Then // cannot create a void-variable
   CompileError(eVoidVar, [Variable.RefSymbol.Name]);

  if (Variable.Typ.isArray(False)) Then
   Variable.Attributes += [vaVolatile]; // arrays have to be volatile, because optimizer doesn't support arrays, thus weird things happen when it tries to optimize them :P

  { add variable into the function }
  getCurrentFunction.SymbolList.Add(TSymbol.Create(stVariable, Variable));

  if (ParsingForeachHeader) Then
  Begin
   eat(_IN);
   Dec(TokenPos);
   Break;
  End;

  if (next_t = _EQUAL) Then // var(...) name=value;
  Begin
   Dec(TokenPos);
   CFGAddNode(TCFGNode.Create(fCurrentNode, cetExpression, ExpressionCompiler.MakeExpression(Compiler, [_SEMICOLON, _COMMA])));
   Dec(TokenPos); // ExpressionCompiler 'eats' comma.
  End;

  if (next_t = _COMMA) Then // var(...) name1, name2, name3...
  Begin
   eat(_COMMA);
  End Else
  Begin
   semicolon;
   Break;
  End;
 End;
End;
End;
End.
