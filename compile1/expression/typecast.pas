Procedure ParseTypeCast;
Var TypeID, Typ: Integer;
Begin
 TypeID := Expr^.Value;

 if (TypeID = -1) Then
  Exit;

 if (Compiler.isTypeVoid(TypeID)) Then
  Error(eVoidCasting, []);

 Typ := Parse(Left, 1);
 With Compiler do
 Begin
  if (Left^.ResultOnStack) Then
   PutOpcode(o_pop, ['e'+getTypePrefix(TypeID)+'1']) Else
   PutOpcode(o_mov, ['e'+getTypePrefix(TypeID)+'1', 'e'+getTypePrefix(Typ)+'1']);
 End;

 Result := TypeID;
End;
