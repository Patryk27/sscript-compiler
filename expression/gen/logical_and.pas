Procedure ParseLogicalAND;
Var TypeLeft, TypeRight: TType;
    ShortCircuit       : TShortCircuit;
    ShortCircuitLabel  : String;
Begin
 With Compiler do
  if (getBoolOption(opt__short_circuit, True)) Then
  Begin
   ShortCircuit      := scAND;
   ShortCircuitLabel := ExprLabel+'__treeexit_'+IntToStr(SomeCounter);
   Inc(SomeCounter);
  End Else
   ShortCircuit := scNone;

 Result := CompileSimple(TypeLeft, TypeRight, False, ShortCircuit, ShortCircuitLabel);

 if (Result = nil) Then
  Exit;

 With Compiler do
 Begin
  if (not Result.isBool) Then
   Error(eUnsupportedOperator, [TypeLeft.asString, getDisplay(Expr), TypeRight.asString]) Else
   PutOpcode(o_and, ['eb1', 'eb2']);

  if (ShortCircuit <> scNone) Then
   PutLabel(ShortCircuitLabel);
 End;
End;
