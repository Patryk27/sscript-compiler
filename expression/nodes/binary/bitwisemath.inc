(* TBitwiseMathExpressionNode.getOpcode *)
Function TBitwiseMathExpressionNode.getOpcode: TOpcodeKind;
Const Table: Array[TBitwiseMathExpressionNodeKind] of TOpcodeKind = (o_or, o_and, o_xor, o_shl, o_shr);
Begin
 Result := Table[Op];
End;

(* TBitwiseMathExpressionNode.getOpcode *)
Function TBitwiseMathExpressionNode.getStringOp: String;
Const Table: Array[TBitwiseMathExpressionNodeKind] of String = ('|', '&', '^', '<<', '>>');
Begin
 Result := Table[Op];
End;

(* TBitwiseMathExpressionNode.Create *)
Constructor TBitwiseMathExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TBitwiseMathExpressionNodeKind);
Begin
 inherited Create(fExprCompiler, fToken, fLeft, fRight);

 Op := fOp;
End;

(* TBitwiseMathExpressionNode.Optimize *)
Function TBitwiseMathExpressionNode.Optimize: TExpressionNode;
Var MathNode: TBitwiseMathExpressionNode;
Begin
 MathNode := TBitwiseMathExpressionNode.Create(ExprCompiler, Token, Left.Optimize(), Right.Optimize(), Op);
 Result   := MathNode;

 // swap nodes for bitwise or/and/xor, if the right node is larger: a|(a&b) -> (a&b)|a
 if (MathNode.Op in [beOr, beAnd, beXor]) and
    (MathNode.getRight.getCost > MathNode.getLeft.getCost) Then
 Begin
  MathNode.SwapNodes;
 End;
End;

(* TBitwiseMathExpressionNode.Clone *)
Function TBitwiseMathExpressionNode.Clone: TBitwiseMathExpressionNode;
Begin
 Result := TBitwiseMathExpressionNode.Create(ExprCompiler, Token, Left, Right, Op);
End;

(* TBitwiseMathExpressionNode.__compile *)
Function TBitwiseMathExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;

  { CheckType }
  Function CheckType(const Typ: TType): Boolean;
  Begin
   Result := (Typ.isBool or Typ.isChar or Typ.isInt);
  End;

Var LType, RType: TType;
    LArg, RArg  : TExpressionCompileResult;
Begin
 CleanResult(Result);

 // compile child nodes releasing registers
 CompileChildNodes(LArg, RArg, True);

 // do type check
 LType := LArg.getType;
 RType := RArg.getType;

 if (CheckType(LType) and CheckType(RType)) Then
 Begin
  PutOpcode(getOpcode, [LArg.getResult, RArg.getResult]);
  Result := SaveResult(LArg, True);
 End Else

 // invalid types
 Begin
  CompileError(getToken, eInvalidUnaryOperator, [LArg.getTypeName, getStringOp, RArg.getTypeName]);
 End;
End;
