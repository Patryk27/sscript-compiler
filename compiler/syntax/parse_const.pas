(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_CONST;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, Tokens, MTypes, Messages, Opcodes;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Variable: TMVariable;
Begin
With TCompiler(Compiler) do
Begin
 Variable.isConst    := True;
 Variable.isParam    := False;
 Variable.Deep       := 0;
 Variable.mCompiler  := Compiler;
 Variable.Visibility := getVisibility;

 While (true) Do
 Begin
  Variable.DeclToken := getToken;
  Variable.Name      := read_ident; // [identifier]

  eat(_EQUAL); // =

  RedeclarationCheck(Variable.Name); // redeclaration of a constant

  Variable.Value          := PMExpression(ExpressionCompiler.MakeConstruction(Compiler, [_SEMICOLON, _COMMA], [oInsertConstants, oConstantFolding, oDisplayParseErrors]).Values[0]); // [constant value]
  Variable.Value^.VarName := Variable.Name;

  With Variable.Value^ do
  Begin
   if (isConstantValue(Variable.Value^)) Then  // is this a constant expression?
   Begin
    Variable.Typ := getTypeFromExpr(Variable.Value^);
   End Else // if it's not - show error
    CompileError(eExpectedConstant, []);
  End;

  if (inFunction) Then // local constant
  Begin
   With getCurrentFunctionPnt^ do
   Begin
    SetLength(VariableList, Length(VariableList)+1);
    VariableList[High(VariableList)] := Variable;
   End;
  End Else // global constant
  Begin
   With getCurrentNamespacePnt^ do
   Begin
    SetLength(GlobalList, Length(GlobalList)+1);
    With GlobalList[High(GlobalList)] do
    Begin
     Typ       := gdConstant;
     mVariable := Variable;
    End;
   End;
  End;

  setPosition(getPosition-1); // ExpressionCompiler 'eats' comma.

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
