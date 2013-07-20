Procedure __tree_simplification(const ShowErrors: Boolean);

{ isPrimaryAssign }
Function isPrimaryAssign(const Node: PExpressionNode): Boolean;
Begin
 Result := (Node <> nil) and (Node^.Typ = mtIdentifier) and (Node^.Symbol <> nil);
End;

(* Parse *)
Procedure Parse(const Expr: PExpressionNode);
Var Left, Right, Param: PExpressionNode;
    Simplify1         : TSimplify1Data;
Begin
 if (Expr = nil) Then
  Exit;

 Left  := Expr^.Left;
 Right := Expr^.Right;

 { 'x = x op y' -> 'x op= y' }
 For Simplify1 in Simplify1Data Do
  if (Expr^.Typ = mtAssign) and (isPrimaryAssign(Left)) and
     (Right^.Typ = Simplify1.Pre) and (Right^.Left^.Symbol = Left^.Symbol) Then
  Begin
   Expr^.Typ   := Simplify1.Post;
   Expr^.Right := Right^.Right;
   AnyChange   := True;
   Break;
  End;

 Parse(Expr^.Left);
 Parse(Expr^.Right);
 For Param in Expr^.ParamList Do
  Parse(Param);
End;

Begin
 Parse(Tree);
End;
