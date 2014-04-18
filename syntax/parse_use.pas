{ Parse_USE }
Procedure Parse_USE;
Var Name               : String;
    Namespace          : TNamespace;
    NamespaceVisibility: PNamespaceVisibility;
Begin
 With Scanner do
 Begin
  if (not ((CompilePass = _cp2) or (inFunction))) Then // "use" clauses are parsed in the second pass or inside a function
  Begin
   read_until(_SEMICOLON);
   Exit;
  End;

  While (true) Do
  Begin
   // next namespace
   Name      := read_ident;
   Namespace := findNamespace(Name);

   if (Namespace = nil) Then
   Begin
    CompileError(eUnknownNamespace, [Name]);
   End Else
   Begin
    New(NamespaceVisibility);
    NamespaceVisibility^.Namespace := Namespace;
    NamespaceVisibility^.Range     := getCurrentRange;

    NamespaceVisibilityList.Add(NamespaceVisibility);
   End;

   // check
   if (next_t = _SEMICOLON) Then
    Break Else
    eat(_COMMA);
  End;

  semicolon;
 End;
End;
