(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_CONST;

 Interface

 Procedure Parse(Compiler: Pointer; isGlobal: Boolean);

 Implementation
Uses Compile1, ExpressionCompiler, Tokens, MTypes, Messages, Opcodes;

{ Parse }
Procedure Parse(Compiler: Pointer; isGlobal: Boolean);
Var Variable: TMVariable;
    VTyp    : TVType;
Begin
With TCompiler(Compiler) do
Begin
 Variable.RegChar := 'i';
 Variable.isConst := True;
 Variable.isParam := False;
 Variable.Deep    := 0;

 eat(_LOWER); // <
 Variable.Typ := read_type; // [type]
 eat(_GREATER); // >

 While (true) Do
 Begin
  Variable.Name := read_ident; // [identifier]

  eat(_EQUAL); // =

  Case isGlobal of
   True: if (findGlobalConstant(Variable.Name) <> -1) Then // redeclaration of global constant
          CompileError(eRedeclaration, [Variable.Name]);

   False: if (findVariable(Variable.Name) <> -1) Then // redeclaration of constant
           CompileError(eRedeclaration, [Variable.Name]);
  End;

  Variable.Value := PMExpression(ExpressionCompiler.MakeConstruction(Compiler, [_SEMICOLON, _COMMA], True, True).Values[0])^; // [constant value]

  With Variable.Value do
  Begin
   if (isConstValue(Variable.Value)) Then  // is this a constant expression?
   Begin
    VTyp := getTypeFromExpr(Variable.Value);

    Variable.RegChar := getTypePrefix(VTyp);

    if (not CompareTypes(Variable.Typ, VTyp)) Then // type check
     CompileError(eWrongType, [getTypeName(VTyp), getTypeName(Variable.Typ)]);
   End Else
    CompileError(eExpectedConstant, []);
  End;

  if (isGlobal) Then
  Begin
   Variable.Visibility := Visibility;

   SetLength(ConstantList, Length(ConstantList)+1);
   ConstantList[High(ConstantList)] := Variable;
  End Else
  Begin
   With FunctionList[High(FunctionList)] do // add this constant into the function
   Begin
    SetLength(VariableList, Length(VariableList)+1); // expand the array
    VariableList[High(VariableList)] := Variable;
   End;
  End;

  setPosition(getPosition-1); // ExpressionCompiler 'eats' comma.

  if (next_t = _COMMA) Then // const(...) name1, name2, name3...
   read Else
   Begin
    semicolon;
    Break;
   End;
 End;
End;
End;
End.
