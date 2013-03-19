(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_VAR;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses CompilerUnit, Compile1, ExpressionCompiler, Tokens, MTypes, symdef, Messages, Opcodes;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Variable: TVariable;
    VarType : TType;
Begin
With TCompiler(Compiler), Parser do
Begin
 eat(_LOWER); // <
 VarType := read_type; // [type]
 eat(_GREATER); // >

 { read variables }
 While (true) do
 Begin
  Variable           := TVariable.Create;
  Variable.mCompiler := Compiler;
  Variable.Typ       := VarType;
  Variable.DeclToken := next_pnt;
  Variable.Name      := read_ident; // [identifier]

  if (Variable.Typ.isVoid) Then // cannot create a void-variable
   CompileError(eVoidVar, [Variable.Name]);

  RedeclarationCheck(Variable.Name); // redeclaration of a variable

  Variable.MemPos := __allocate_var(getBoolOption(opt__register_alloc), Variable.Typ.RegPrefix);
  Variable.Deep   := CurrentDeep;

  { insert variable into the function  }
  With getCurrentFunction do
  Begin
   SetLength(VariableList, Length(VariableList)+1); // expand the array
   VariableList[High(VariableList)] := Variable;
  End;

  if (next_t = _EQUAL) Then // var(...) name=value;
  Begin
   Dec(TokenPos);
   AddConstruction(ExpressionCompiler.MakeConstruction(Compiler, [_SEMICOLON, _COMMA]));
   Dec(TokenPos); // ExpressionCompiler 'eats' comma.
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
