(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_NAMESPACE;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, MTypes, symdef, Tokens, Messages;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var nName     : String;
    Deep, I   : Integer;
    Namespaces: TMIntegerArray;
Begin
With TCompiler(Compiler), Parser do
Begin
 // make a backup of current namespaces (as we'll restore them when we'll finish compiling this namespace)
 SetLength(Namespaces, Length(SelectedNamespaces));
 For I := Low(Namespaces) To High(Namespaces) Do
  Namespaces[I] := SelectedNamespaces[I];

 Deep := CurrentDeep;

 (* if first pass *)
 if (CompilePass = cp1) Then
 Begin
  nName := read_ident; // _IDENTIFIER
  RedeclarationCheck(nName, True); // redeclaration check

  CurrentNamespace := findNamespace(nName);

  if (CurrentNamespace = -1) Then // new namespace
  Begin
   NamespaceList.Add(TNamespace.Create);

   CurrentNamespace := NamespaceList.Count-1;
   With NamespaceList.Last do
   Begin
    Name       := nName;
    mCompiler  := Compiler;
    DeclToken  := next_pnt(-1);
    SymbolList := TGlobalSymbolList.Create;
   End;
  End;

  NamespaceList[CurrentNamespace].Visibility := getVisibility;

  if (next_t <> _BRACKET3_OP) Then
   CompileError(eExpected, ['{', next.Value]);
 End Else

 (* if second pass *)
 if (CompilePass = cp2) Then
 Begin
  CurrentNamespace := findNamespace(read_ident);

  if (CurrentNamespace = -1) Then
   CompileError(eInternalError, ['CurrentNamespace = -1']);
 End;

 { set this new namespace as first on the list }
 SetLength(SelectedNamespaces, Length(SelectedNamespaces)+1);
 For I := High(SelectedNamespaces) Downto Low(SelectedNamespaces) Do
  SelectedNamespaces[I] := SelectedNamespaces[I-1];
 SelectedNamespaces[0] := CurrentNamespace;

 (* for each pass *)

 { compile namespace }
 Repeat
  if (next_t = _NAMESPACE) Then // for now, it's impossible to declare a namespace inside another one
  Begin
   read;
   CompileError(eNotAllowed, ['namespace']);
  End;

  ParseToken;
 Until (CurrentDeep = Deep);

 CurrentNamespace := 0;

 { restore namespaces }
 SetLength(SelectedNamespaces, Length(Namespaces));
 For I := Low(Namespaces) To High(Namespaces) Do
  SelectedNamespaces[I] := Namespaces[I];
End;
End;
End.
