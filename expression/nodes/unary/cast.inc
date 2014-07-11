(* TCastExpressionNode.Create *)
Constructor TCastExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode; const fType: TObject);
Begin
 inherited Create(fExprCompiler, fToken, fChild);

 if (fType is TType) Then
  mType := fType Else
  raise EExpressionNodeException.CreateFmt('Invalid fType class instance! (got %s, expected TType)', [fType.ClassName]);
End;

(* TCastExpressionNode.Clone *)
Function TCastExpressionNode.Clone: TCastExpressionNode;
Begin
 Result := TCastExpressionNode.Create(ExprCompiler, Token, Child, mType);
End;

(* TCastExpressionNode.__compile *)
Function TCastExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Var Expression: TExpressionCompileResult;
    ExprType  : TType;
    CastType  : TType;
Begin
 // compile expression
 Expression := Child.Compile(True);
 ExprType   := Expression.getType;

 // get new type (after the casting)
 CastType := TType(mType);

 // do type-check
 if (CastType.isVoid) Then
  CompileError(getToken, eVoidCasting, []);

 if (not ExprType.CanBeCastedTo(CastType)) Then
  CompileError(getToken, eInvalidConversion, [ExprType.asString, CastType.asString]);

 // do casting in bytecode
 if (ExprType.RegPrefix = CastType.RegPrefix) Then
 Begin
  if (CanBeImmediate) Then
   Result := Expression Else
   Result := SaveResult(Expression, False);
 End Else
 Begin
  Result := ExprCompiler.AllocateCompileResult(CastType);

  PutOpcode(o_mov, [Result.getResult, Expression.getResult]);
 End;
End;
