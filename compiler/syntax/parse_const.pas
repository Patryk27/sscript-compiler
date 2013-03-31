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
 While (true) Do
 Begin
  Variable := TVariable.Create;
  Variable.Attributes += [vaConst, vaDontAllocate];

  Variable.Deep       := 0;
  Variable.mCompiler  := Compiler;
  Variable.Visibility := getVisibility;

  Variable.DeclToken := next_pnt;
  Variable.Name      := read_ident; // [identifier]

  eat(_EQUAL); // =

  RedeclarationCheck(Variable.Name); // redeclaration of a constant

  Variable.Value          := PMExpression(ExpressionCompiler.MakeConstruction(Compiler, [_SEMICOLON, _COMMA], [oInsertConstants, oConstantFolding, oDisplayParseErrors]).Values[0]); // [constant value]
  Variable.Value^.VarName := Variable.Name;

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

  if (inFunction) Then // local constant
  Begin
   With getCurrentFunction do
   Begin
    SetLength(VariableList, Length(VariableList)+1);
    VariableList[High(VariableList)] := Variable;
   End;
  End Else // global constant
  Begin
   With getCurrentNamespace do
   Begin
    SetLength(SymbolList, Length(SymbolList)+1);
    SymbolList[High(SymbolList)] := TGlobalSymbol.Create;
    With SymbolList[High(SymbolList)] do
    Begin
     Typ       := gsConstant;
     mVariable := Variable;
    End;
   End;
  End;

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
