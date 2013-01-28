{
 var<type> var_name;
 var<type> var_name = var_value;
 var<type> var_name, another_variable, (...);
}
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
 eat(_LOWER); // <
 Variable.Typ := read_type; // [type]
 eat(_GREATER); // >

 While (true) do
 Begin
  Variable.Name := read_ident; // [identifier]

  if (isTypeVoid(Variable.Typ)) Then
   CompileError(eVarVoid, [Variable.Name]);

  if (findVariable(Variable.Name) <> -1) Then // redeclaration of variable
   CompileError(eRedeclaration, [Variable.Name]);

  if (_Or in Options) Then // allow to allocate variables in registers
  Begin
   With Variable do
   Begin
    RegChar := getTypePrefix(Typ);
    RegID   := findFreeRegister(RegChar); // find free register for our variable

    if (RegID = -1) Then // no free register found...
     goto AllocateOntoTheStack; // ... so allocate variable on the stack
   End;
  End Else
  Begin
  AllocateOntoTheStack:

   Variable.RegChar := getTypePrefix(Variable.Typ);

   Pos := 0;
   With FunctionList[High(FunctionList)] do
    For I := Low(VariableList) To High(VariableList) Do
     if (VariableList[I].Deep <= CurrentDeep) and (VariableList[I].RegID <= 0) and (not VariableList[I].isParam) Then
      Inc(Pos);
   Variable.RegID := -Pos;
  End;

  Variable.Deep    := CurrentDeep;
  Variable.isParam := False;

  With FunctionList[High(FunctionList)] do // add variable to the function
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
