Procedure __insert_constants(const ShowErrors: Boolean);

{ Parse }
Procedure Parse(Expr: PMExpression);
Var I, VarID, NamespaceID: Integer;
Begin
 if (Expr^.Typ = mtArrayElement) Then
  Exit;

 { function call }
 if (Expr^.Typ = mtFunctionCall) Then
 Begin
  For I := Low(Expr^.ParamList) To High(Expr^.ParamList) Do
   Parse(Expr^.ParamList[I]);
  Exit;
 End;

 if (Expr^.Left = nil) and (Expr^.Right = nil) Then
 Begin
  { variable }
  if (Expr^.Typ = mtVariable) Then
  Begin
   VarID       := Compiler.findLocalVariable(Expr^.Value); // is it a local variable?
   NamespaceID := -1;

   if (VarID = -1) Then // local variable not found
   Begin
    Compiler.findGlobalVariableCandidate(Expr^.Value, Expr^.Namespaces, VarID, NamespaceID, @Expr^.Token); // so - is it a global variable?

    if (VarID = -1) Then
    Begin
     if (ShowErrors) Then
      Compiler.CompileError(Expr^.Token, eUnknownVariable, [Expr^.Value]);
     Exit;
    End;
   End;

   if (NamespaceID = -1) Then
   Begin
    { local constant }
    With Compiler.getCurrentFunction.SymbolList[VarID] do
    Begin
     if (Typ <> lsConstant) Then // is it a constant?
     Begin
      if (ShowErrors) Then
       Compiler.CompileError(Expr^.Token, eNotAConstant, [Expr^.Value]);
      Exit;
     End;

     Expr^ := mVariable.Value^;
     Exit;
    End;
   End Else
   Begin
    { global constant }
    With Compiler.NamespaceList[NamespaceID].SymbolList[VarID] do
    Begin
     if (Typ <> gsConstant) Then // is it a constant?
     Begin
      if (ShowErrors) Then
       Compiler.CompileError(Expr^.Token, eNotAConstant, [Expr^.Value]);
      Exit;
     End;

     Expr^ := mVariable.Value^;
     Exit;
    End;
   End;
  End;
 End;

 if (Expr^.Left <> nil) Then
  Parse(Expr^.Left);

 if (Expr^.Right <> nil) Then
  Parse(Expr^.Right);
End;

Begin
 Parse(Tree);
End;
