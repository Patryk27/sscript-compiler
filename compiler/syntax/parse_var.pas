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
 if not ((CompilePass = cp1) or (inFunction)) Then
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
   DeclToken := next_pnt;
   mCompiler := Compiler;
   Range     := getCurrentRange;
   Name      := read_ident; // [identifier]

   RedeclarationCheck(Name); // redeclaration of the variable
  End;

  if (Variable.Typ.isVoid) Then // cannot create a void-variable
   CompileError(eVoidVar, [Variable.RefSymbol.Name]);

  Variable.MemPos := __allocate_var(getBoolOption(opt__register_alloc), Variable.Typ.RegPrefix);

  { add variable into the function }
  getCurrentFunction.SymbolList.Add(TLocalSymbol.Create(lsVariable, Variable));

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
