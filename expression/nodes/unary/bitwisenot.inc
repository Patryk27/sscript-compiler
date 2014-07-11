(* TBitwiseNotExpressionNode.__compile *)
Function TBitwiseNotExpressionNode.__compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult;
Var Typ: TType;
Begin
 // compile expression
 Result := Child.Compile;

 // pop result
 PopResult(Result);

 // do type check
 Typ := Result.getType;

 if (not (Typ.isChar or Typ.isInt)) Then
  CompileError(getToken, eInvalidUnaryOperator, ['~', Result.getTypeName]);

 // put opcode
 PutOpcode(o_not, [Result.getResult]);
End;

(* TBitwiseNotExpressionNode.Clone *)
Function TBitwiseNotExpressionNode.Clone: TBitwiseNotExpressionNode;
Begin
 Result := TBitwiseNotExpressionNode.Create(ExprCompiler, Token, Child);
End;
