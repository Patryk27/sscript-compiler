(* TLogicalNotExpressionNode.__compile *)
Function TLogicalNotExpressionNode.__compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult;
Var Tmp: TExpressionCompileResult;
Begin
 // compile expression
 Result := Child.Compile;

 // pop result
 PopResult(Result);

 // do type check
 if (not (Result.getType.isBool or Result.getType.isInt)) Then
  CompileError(getToken, eInvalidUnaryOperator, ['!', Result.getTypeName]);

 // put opcode
 PutOpcode(o_not, [Result.getResult]);

 // do casting, if child was an 'int': !int -> bool
 if (Result.getType.isInt) Then
 Begin
  ExprCompiler.ReleaseRegister(Result);
  Tmp := ExprCompiler.AllocateCompileResult(TYPE_BOOL);

  PutOpcode(o_mov, [Tmp.getResult, Result.getResult]);

  Result := Tmp;
 End;
End;

(* TLogicalNotExpressionNode.Clone *)
Function TLogicalNotExpressionNode.Clone: TLogicalNotExpressionNode;
Begin
 Result := TLogicalNotExpressionNode.Create(ExprCompiler, Token, Child);
End;
