(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_CONST;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, Tokens, MTypes, symdef, Messages, Opcodes;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Variable: TVariable;
Begin
With TCompiler(Compiler), Parser do
Begin
 if not ((CompilePass = cp1) or (inFunction)) Then // `const` is parsed in second pass or inside function
 Begin
  read_until(_SEMICOLON);
  Exit;
 End;

 While (true) Do
 Begin
  Variable := TVariable.Create;
  Variable.Attributes += [vaConst, vaDontAllocate];

  With Variable.RefSymbol do
  Begin
   Range      := getCurrentRange;
   mCompiler  := Compiler;
   Visibility := getVisibility;

   DeclToken := next_pnt;
   Name      := read_ident; // [identifier]

   RedeclarationCheck(Name); // check for redeclaration of the constant
  End;

  eat(_EQUAL); // =

  Variable.Value          := PMExpression(ExpressionCompiler.MakeConstruction(Compiler, [_SEMICOLON, _COMMA], [oInsertConstants, oConstantFolding, oDisplayParseErrors]).Values[0]); // [constant value]
  Variable.Value^.VarName := Variable.RefSymbol.Name;

  With Variable.Value^ do
  Begin
   if (isConstantValue(Variable.Value^)) Then // is this a constant expression?
   Begin
    Variable.Typ := getTypeFromExpr(Variable.Value^);
   End Else // if it's not - show error
   Begin
    CompileError(eExpectedConstant, []);
    Variable.Value := EmptyExpression;
   End;
  End;

  if (inFunction) Then
   getCurrentFunction.SymbolList.Add(TLocalSymbol.Create(lsConstant, Variable)) { local constant } Else
   getCurrentNamespace.SymbolList.Add(TGlobalSymbol.Create(gsConstant, Variable)); { global constant }

  Dec(TokenPos); // ExpressionCompiler 'eats' comma.

  if (next_t = _COMMA) Then // const(...) name1=value1, name2=value2, name3=value3...
   read Else
   Begin
    semicolon;
    Break;
   End;
 End;
End;
End;
End.
