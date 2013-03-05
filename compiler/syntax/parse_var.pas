(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_VAR;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses CompilerUnit, Compile1, ExpressionCompiler, Tokens, MTypes, Messages, Opcodes;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Variable: TMVariable;
    I, Pos  : Integer;
Label AllocateOntoTheStack;
Begin
With TCompiler(Compiler) do
Begin
 Variable.isConst   := False;
 Variable.mCompiler := Compiler;

 eat(_LOWER); // <
 Variable.Typ := read_type; // [type]
 eat(_GREATER); // >

 { read variables }
 While (true) do
 Begin
  Variable.DeclToken := getToken;
  Variable.Name      := read_ident; // [identifier]

  if (isTypeVoid(Variable.Typ)) Then // cannot create a void-variable
   CompileError(eVoidVar, [Variable.Name]);

  RedeclarationCheck(Variable.Name); // redeclaration of a variable

  if (getBoolOption(opt__register_alloc)) Then // can we allocate variables in registers?
  Begin
   With Variable do
   Begin
    RegID := findFreeRegister(getTypePrefix(Typ)); // find free register for our variable

    if (RegID = -1) Then // no free register found...
     goto AllocateOntoTheStack; // ... so allocate variable on the stack
   End;
  End Else
  Begin
  AllocateOntoTheStack:

   { find a stack position, where we can allocate this variable }
   Pos := 0;
   With getCurrentFunction do
    For I := Low(VariableList) To High(VariableList) Do
     if (VariableList[I].Deep <= CurrentDeep) and (VariableList[I].RegID <= 0) and (not VariableList[I].isParam) Then
      Inc(Pos);

   { ... and place it there }
   Variable.RegID := -Pos;
  End;

  Variable.Deep    := CurrentDeep;
  Variable.isParam := False;

  { insert variable into the function  }
  With getCurrentFunctionPnt^ do
  Begin
   SetLength(VariableList, Length(VariableList)+1); // expand the array
   VariableList[High(VariableList)] := Variable;
  End;

  if (next_t = _EQUAL) Then // var(...) name=value;
  Begin
   setPosition(getPosition-1);
   AddConstruction(ExpressionCompiler.MakeConstruction(Compiler, [_SEMICOLON, _COMMA]));
   setPosition(getPosition-1); // ExpressionCompiler 'eats' comma.
  End;

  if (next_t = _COMMA) Then // var(...) name1, name2, name3...
   read Else
   Begin
    semicolon;
    Break;
   End;
 End;
End;
End;
End.
