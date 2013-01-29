(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_CONST;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses CompilerUnit, Compile1, ExpressionCompiler, Tokens, MTypes, Messages, Opcodes;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Variable: TMVariable;
    VTyp    : TVType;
Begin
With TCompiler(Compiler) do
Begin
 Variable.isConst := True;

 eat(_LOWER); // <
 Variable.Typ := read_type; // [type]
 eat(_GREATER); // >

 Variable.Name := read_ident; // [identifier]

 eat(_EQUAL); // =

 if (findVariable(Variable.Name) <> -1) Then // redeclaration of constant
  CompileError(eRedeclaration, [Variable.Name]);

 Variable.Value   := PMExpression(ExpressionCompiler.MakeConstruction(Compiler, [_SEMICOLON], True).Values[0])^; // [constant value]
 Variable.RegChar := 'i';

 With Variable.Value do // is this a constant expression?
 Begin
  if (Left <> nil) or (Right <> nil) Then
   CompileError(eExpectedConstant, []) Else
   Begin
    VTyp := getTypeFromExpr(Variable.Value);

    Variable.RegChar := getTypePrefix(VTyp);

    if (not CompareTypes(Variable.Typ, VTyp)) Then
     CompileError(eWrongType, [getTypeName(VTyp), getTypeName(Variable.Typ)]);
   End;
 End;

 With FunctionList[High(FunctionList)] do // add variable into the function
 Begin
  SetLength(VariableList, Length(VariableList)+1); // expand the array
  VariableList[High(VariableList)] := Variable;
 End;
End;
End;
End.
