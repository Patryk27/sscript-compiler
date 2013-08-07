{ Parse_USE }
Procedure Parse_USE;
Var Name               : String;
    Namespace          : TNamespace;
    NamespaceVisibility: PNamespaceVisibility;
Begin
 With Parser do
 Begin
 if (not ((CompilePass = _cp2) or (inFunction))) Then // "use" clauses are parsed in the second pass or inside a function
 Begin
  read_until(_SEMICOLON);
  Exit;
 End;

  Repeat
   // next namespace
   Name := read_ident;

   Namespace := findNamespace(Name);
   if (Namespace = nil) Then
   Begin
    CompileError(eUnknownNamespace, [Name]);
    Exit;
   End;

   New(NamespaceVisibility);
   NamespaceVisibility^.Namespace := Namespace;
   NamespaceVisibility^.Range     := getCurrentRange;

   NamespaceVisibilityList.Add(NamespaceVisibility);

   // check
   if (next_t = _SEMICOLON) Then
    Break Else
    eat(_COMMA);
  Until (False);

  semicolon;
 End;
End;
