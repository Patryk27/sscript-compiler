(* TIdentifierExpressionNode.Create *)
Constructor TIdentifierExpressionNode.Create(const fExprCompiler: TExpressionCompiler; const fToken: TToken_P; const fName: String; const fSymbol: TObject);
Begin
 inherited Create(fExprCompiler, fToken);

 Name   := fName;
 Symbol := fSymbol;

 if (Symbol <> nil) and (not (Symbol is TSymbol)) Then
  raise EExpressionNodeException.CreateFmt('Expected a TSymbol class instance! (got: %s)', [Symbol.ClassName]);
End;

(* TIdentifierExpressionNode.Evaluate *)
Function TIdentifierExpressionNode.Evaluate: TExpressionNode;
Var Sym: TSymbol;
Begin
 // get symbol instance
 Sym := TSymbol(Symbol);

 // check if it's a constant
 if (Sym.Typ = stConstant) Then
 Begin
  Result := Sym.mVariable.Value.Clone();
 End Else

 // otherwise just return a clone
 Begin
  Result := Clone();
 End;
End;

(* TIdentifierExpressionNode.Clone *)
Function TIdentifierExpressionNode.Clone: TIdentifierExpressionNode;
Begin
 Result := TIdentifierExpressionNode.Create(ExprCompiler, Token, Name, Symbol);
End;

(* TIdentifierExpressionNode.findIdentifier *)
Function TIdentifierExpressionNode.findIdentifier(const fSymbol: TObject): Boolean;
Begin
 Result := (fSymbol = Symbol);
End;

(* TIdentifierExpressionNode.isLValue *)
Function TIdentifierExpressionNode.isLValue: Boolean;
Begin
 Result := True;
End;

(* TIdentifierExpressionNode.getType *)
Function TIdentifierExpressionNode.getType: TObject;
Begin
 Result := TSymbol(Symbol).mVariable.Typ;
End;

(* TIdentifierExpressionNode.getLocation *)
{
 Returns identifier location in form: '~symboladdress:stackfix'.
 It's automatically changed to valid variable location on the end of the function compilation.
}
Function TIdentifierExpressionNode.getLocation: String;
Begin
 Result := Format('~%d:%d', [PtrUInt(getVariable), ExprCompiler.getUsedStackSlots]);
End;

(* TIdentifierExpressionNode.__compile *)
Function TIdentifierExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Begin
 // error: symbol not found (should not happen)
 if (Symbol = nil) Then
  raise EExpressionNodeException.Create('Symbol = nil');

 // special case: constant variable reference
 if (TSymbol(Symbol).Typ = stConstant) Then
 Begin
  Result := getVariable.Value.__compile(CanBeImmediate);
  Exit;
 End;

 // small optimization: check if we can't use register which already holds our variable value
 if (CanBeImmediate) Then
 Begin
  if (ExprCompiler.FindRegisterValidatedToVariable(getVariable, Result)) Then
   Exit;
 End;

 // save result
 Result := SaveResult(CreateCompileResult(getLocation, getVariable.Typ, [ecrImmediate]), CanBeImmediate);
 Result.Attributes += [ecrHoldsVariable];

 if (Result.Kind = ecRegister) Then
 Begin
  ExprCompiler.ValidateRegisterToVariable(Result.RegisterChar, Result.RegisterID, getVariable);
 End;
End;
