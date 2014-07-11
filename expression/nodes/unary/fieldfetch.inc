(* TFieldFetchExpressionNode.Create *)
Constructor TFieldFetchExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode; const fFieldName: String);
Begin
 inherited Create(fExprCompiler, fToken, fChild);

 FieldName := fFieldName;
End;

(* TFieldFetchExpressionNode.Clone *)
Function TFieldFetchExpressionNode.Clone: TFieldFetchExpressionNode;
Begin
 Result := TFieldFetchExpressionNode.Create(ExprCompiler, Token, Child.Clone(), FieldName);
End;

(* TFieldFetchExpressionNode.__compile *)
Function TFieldFetchExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Var Expression: TExpressionCompileResult;
Begin
 // clean result
 CleanResult(Result);

 // compile expression
 Expression := Child.Compile(True);

 ExprCompiler.ReleaseRegister(Expression);

 // show that field couldn't have been found (no OOP in SScript for now)
 CompileError(getToken, eFieldNotFound, [FieldName, Expression.getTypeName]);

 // show mercy and hint
 if (FieldName = 'length') Then
  CompileHint(getToken, hDidYouMean, ['length()']);
End;
