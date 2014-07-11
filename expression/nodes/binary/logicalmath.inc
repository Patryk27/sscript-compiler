(* TLogicalMathExpressionNode.getOpcode *)
Function TLogicalMathExpressionNode.getOpcode: TOpcodeKind;
Const Table: Array[TLogicalMathExpressionNodeKind] of TOpcodeKind = (o_or, o_and);
Begin
 Result := Table[Op];
End;

(* TLogicalMathExpressionNode.getStringOp *)
Function TLogicalMathExpressionNode.getStringOp: String;
Const Table: Array[TLogicalMathExpressionNodeKind] of String = ('||', '&&');
Begin
 Result := Table[Op];
End;

(* TLogicalMathExpressionNode.Create *)
Constructor TLogicalMathExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TLogicalMathExpressionNodeKind);
Begin
 inherited Create(fExprCompiler, fToken, fLeft, fRight);

 Op := fOp;
End;

(* TLogicalMathExpressionNode.Optimize *)
Function TLogicalMathExpressionNode.Optimize: TExpressionNode;
Var MathNode: TLogicalMathExpressionNode;
Begin
 MathNode := TLogicalMathExpressionNode.Create(ExprCompiler, Token, Left.Optimize(), Right.Optimize(), Op);
 Result   := MathNode;

 // swap nodes if right is larger
 if (MathNode.getRight.getCost > MathNode.getLeft.getCost) Then
 Begin
  MathNode.SwapNodes;
 End;
End;

(* TLogicalMathExpressionNode.Evaluate *)
Function TLogicalMathExpressionNode.Evaluate: TExpressionNode;
Var LValue, RValue, OutValue: TMixedValue;
    LNode, RNode            : TExpressionNode;
Begin
 Result := nil;

 LNode := Left.Evaluate();
 RNode := Right.Evaluate();

 // we can only further evaluate expression if we have two constants
 if (LNode is TConstantExpressionNode) and (RNode is TConstantExpressionNode) Then
 Begin
  LValue := TConstantExpressionNode(LNode).getMixedValue;
  RValue := TConstantExpressionNode(RNode).getMixedValue;

  // ensure they are bools
  if (LValue.Kind = mvBool) and (RValue.Kind = mvBool) Then
  Begin
   // do math
   Case Op of
    leOr : OutValue := LValue or RValue;
    leAnd: OutValue := LValue and RValue;
   End;

   // if succeeded, create result node and free the temporary ones
   if (OutValue.Kind <> mvUnknown) Then
   Begin
    Result := TConstantExpressionNode.Create(ExprCompiler, Token, OutValue);

    LNode.Free;
    RNode.Free;
   End;
  End;
 End;

 if (Result = nil) Then
  Result := TLogicalMathExpressionNode.Create(ExprCompiler, Token, LNode, RNode, Op);
End;

(* TLogicalMathExpressionNode.Clone *)
Function TLogicalMathExpressionNode.Clone: TLogicalMathExpressionNode;
Begin
 Result := TLogicalMathExpressionNode.Create(ExprCompiler, Token, Left.Clone(), Right.Clone(), Op);
End;

(* TLogicalMathExpressionNode.__compile *)
Function TLogicalMathExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Var LType, RType: TType;
    LArg, RArg  : TExpressionCompileResult;
Begin
 CleanResult(Result);

 // compile child nodes releasing registers
 CompileChildNodes(LArg, RArg, True);

 // do type check
 LType := LArg.getType;
 RType := RArg.getType;

 if (LType.isBool) and (RType.isBool) Then
 Begin
  PutOpcode(getOpcode, [LArg.getResult, RArg.getResult]);
  Result := SaveResult(LArg, True);
 End Else

 // invalid types
 Begin
  CompileError(getToken, eInvalidUnaryOperator, [LArg.getTypeName, getStringOp, RArg.getTypeName]);
 End;
End;
