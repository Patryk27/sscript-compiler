{ Parse_USE }
Procedure Parse_USE;
Var Namespace: Integer;
    Name     : String;

  // Add
  Procedure Add(const Namespace: Integer);
  Begin
   SetLength(SelectedNamespaces, Length(SelectedNamespaces)+1);
   SelectedNamespaces[High(SelectedNamespaces)] := Namespace;
  End;

  // notDuplicated
  Function notDuplicated(const Namespace: Integer): Boolean;
  Var NS: Integer;
  Begin
   Result := True;

   For NS in SelectedNamespaces Do
    if (NS = Namespace) Then
     Exit(False);
  End;

Begin
 With Parser do
 Begin
  if not ((CompilePass = _cp3) or (inFunction)) Then // `use` is parsed in the third pass or inside function
  Begin
   read_until(_SEMICOLON);
   Exit;
  End;

  if (next_t = _SEMICOLON) Then // `use;` sets the global (default) namespace
  Begin
   SetLength(SelectedNamespaces, 1);
   SelectedNamespaces[0] := 0;
   eat(_SEMICOLON);
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
     if (notDuplicated(Namespace)) Then
      Add(Namespace);
    End;

   // check
   if (next_t = _SEMICOLON) Then
    Break Else
    eat(_COMMA);
  Until (False);

  semicolon;
 End;
End;
