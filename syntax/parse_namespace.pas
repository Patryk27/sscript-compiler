(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_NAMESPACE;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses SSCompiler, symdef, Tokens, Messages;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var nName: String;
    Deep : Integer;
Begin
With TCompiler(Compiler), Parser do
Begin
 Deep := CurrentDeep;

 (* if first pass *)
 if (CompilePass = _cp1) Then
 Begin
  nName := read_ident; // namespace's name
  RedeclarationCheck(nName, True); // redeclaration check

  CurrentNamespace := findNamespace(nName);

  if (CurrentNamespace = nil) Then // new namespace
  Begin
   NamespaceList.Add(TNamespace.Create);

   CurrentNamespace := NamespaceList.Last;
   With NamespaceList.Last do
   Begin
    With RefSymbol do
    Begin
     Name       := nName;
     mCompiler  := Compiler;
     DeclToken  := next_pnt(-1);
    End;

    SymbolList := TSymbolList.Create;
   End;
  End;

  CurrentNamespace.RefSymbol.Visibility := getVisibility;

  if (next_t <> _BRACKET3_OP) Then
   CompileError(eExpected, ['{', next.Value]);
 End Else

 (* if second pass *)
 if (CompilePass = _cp2) Then
 Begin
  CurrentNamespace := findNamespace(read_ident);

  if (CurrentNamespace = nil) Then
   CompileError(eInternalError, ['CurrentNamespace = nil']);
 End;

 { parse namespace }
 Repeat
  if (next_t = _NAMESPACE) Then // for now it's impossible to declare a namespace inside another one
  Begin
   read;
   CompileError(eNotAllowed, ['namespace']);
  End;

  ParseToken;
 Until (CurrentDeep = Deep);

 CurrentNamespace := getDefaultNamespace;
End;
End;
End.
