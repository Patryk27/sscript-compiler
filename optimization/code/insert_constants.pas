Procedure __insert_constants(const ShowErrors: Boolean);

(* Parse *)
Procedure Parse(const Expr: PExpressionNode);
Var Param: PExpressionNode;
Begin
 if (Expr = nil) or (Expr^.Typ = mtArrayElement) Then
  Exit;

 if (Expr^.Typ = mtIdentifier) Then // if identifier
 Begin
  if (Expr^.Symbol = nil) Then // if unknown symbol
  Begin
   if (ShowErrors) Then
    Compiler.CompileError(Expr^.Token, eUnknownVariable, [Expr^.IdentName]);
   Exit;
  End;

  With TSymbol(Expr^.Symbol) do
  Begin
   if (Typ <> stConstant) Then // is it a constant?
   Begin
    if (ShowErrors) Then
     Compiler.CompileError(Expr^.Token, eNotAConstant, [Expr^.IdentName]);
    Exit;
   End;

   AnyChange := True;
   Expr^     := mVariable.Value^;
   Exit;
  End;
 End;

 Parse(Expr^.Left);
 Parse(Expr^.Right);
 For Param in Expr^.ParamList Do
  Parse(Param);
End;

Begin
 Parse(Tree);
End;
