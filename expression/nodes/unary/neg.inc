(* TNegExpressionNode.__compile *)
Function TNegExpressionNode.__compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult;
Begin
 // compile expression
 Result := Child.Compile;

 // pop result
 PopResult(Result);

 // do type check
 if (not Result.getType.isNumerical) Then
  CompileError(getToken, eInvalidUnaryOperator, ['-', Result.getTypeName]);

 // put opcode
 PutOpcode(o_neg, [Result.getResult]);
End;

(* TNegExpressionNode.Evaluate *)
Function TNegExpressionNode.Evaluate: TExpressionNode;
Var Value: TMixedValue;
    Expr : TExpressionNode;
Begin
 Result := nil;

 Expr := Child.Evaluate();

 // - constant
 if (Expr is TConstantExpressionNode) Then
 Begin
  Value := -TConstantExpressionNode(Expr).getMixedValue;

  if (Value.Kind <> mvUnknown) Then
  Begin
   Result := TConstantExpressionNode.Create(ExprCompiler, Token, Value);
   Expr.Free;
  End;
 End;

 if (Result = nil) Then
  Result := TNegExpressionNode.Create(ExprCompiler, Token, Expr);
End;

(* TNegExpressionNode.Clone *)
Function TNegExpressionNode.Clone: TNegExpressionNode;
Begin
 Result := TNegExpressionNode.Create(ExprCompiler, Token, Child);
End;
