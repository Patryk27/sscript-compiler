{ Parse_USE }
Procedure Parse_USE;
Var Namespace: Integer;
    Name     : String;

  Procedure Add(const Namespace: Integer);
  Begin
   SetLength(SelectedNamespaces, Length(SelectedNamespaces)+1);
   SelectedNamespaces[High(SelectedNamespaces)] := Namespace;
  End;

Begin
 if (next_t = _SEMICOLON) Then // `use;` sets the global (default) namespace
 Begin
  SetLength(SelectedNamespaces, 1);
  SelectedNamespaces[0] := 0;
  Exit;
 End;

 Repeat
  // next namespace
  Name := read_ident;

  Namespace := findNamespace(Name);
  if (Namespace = -1) Then
   CompileError(eUnknownNamespace, [Name]) Else
   Begin
    // add it into the list
    Add(Namespace);
   End;

  // check
  if (next_t = _SEMICOLON) Then
   Break Else
   eat(_COMMA);
 Until (False);

 semicolon;
End;
