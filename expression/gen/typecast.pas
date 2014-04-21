Procedure ParseTypeCast;
Var TypeID, Typ: TType;
Begin
 TypeID := getType(Expr^.Value);

 if (TypeID = nil) Then
  Exit;

 if (TypeID.isVoid) Then
  Error(eVoidCasting, []);

 Typ := Parse(Left, 1);

 if (not Typ.CanBeCastedTo(TypeID)) Then
  Error({Left^.Token,}eInvalidConversion, [Typ.asString, TypeID.asString]);

 With Compiler do
 Begin
  if (Left^.ResultOnStack) Then
  Begin
   PutOpcode(o_pop, ['e'+TypeID.RegPrefix+'1']);
   Dec(PushedValues);
  End Else
  Begin
   PutOpcode(o_mov, ['e'+TypeID.RegPrefix+'1', 'e'+Typ.RegPrefix+'1']);
  End;
 End;

 Result := TypeID;
End;
