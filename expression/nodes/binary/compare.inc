// @TODO: short circuit

(* TComprasionExpressionNode.CheckTypes *)
Function TComprasionExpressionNode.CheckTypes(const fLType, fRType: TObject): Boolean;
Var LType: TType absolute fLType;
    RType: TType absolute fRType;
Begin
 if (LType = nil) or (RType = nil) Then
  Exit(False);

 // valid: object <op> object (compares their addresses)
 if (LType.isObject) and (RType.isObject) Then
 Begin
  Result := True;
 End Else

 // valid simple <op> simple
 if (LType.isSimple) and (LType.RegPrefix = RType.RegPrefix) Then
 Begin
  Result := True;
 End Else

 // valid: int/float <op> int/float
 if (LType.isNumerical) and (RType.isNumerical) Then
 Begin
  Result := True;
 End Else

 // valid: int/char <op> int/char
 if (LType.isSimple) and (RType.isSimple) and
    (LType.RegPrefix in ['i', 'c']) and (RType.RegPrefix in ['i', 'c']) Then
 Begin
  Result := True;
 End Else

 // otherwise comparsion is not valid
 Begin
  Result := False;
 End;
End;

(* TComprasionExpressionNode.getOpcode *)
Function TComprasionExpressionNode.getOpcode: TOpcodeKind;
Const Table: Array[TComprasionExpressionNodeKind] of TOpcodeKind = (o_if_l, o_if_le, o_if_e, o_if_ge, o_if_g, o_if_ne);
Begin
 Result := Table[Op];
End;

(* TComprasionExpressionNode.getStringOp *)
Function TComprasionExpressionNode.getStringOp: String;
Const Table: Array[TComprasionExpressionNodeKind] of String = ('<', '<=', '==', '>=', '>', '!=');
Begin
 Result := Table[Op];
End;

(* TComprasionExpressionNode.Create *)
Constructor TComprasionExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TComprasionExpressionNodeKind);
Begin
 inherited Create(fExprCompiler, fToken, fLeft, fRight);

 Op := fOp;
End;

(* TComparsionExpressionNode.Evaluate *)
Function TComprasionExpressionNode.Evaluate: TExpressionNode;
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

  // do math
  Case Op of
   ceLower       : OutValue := (LValue < RValue);
   ceLowerEqual  : OutValue := (LValue <= RValue);
   ceEqual       : OutValue := (LValue = RValue);
   ceGreater     : OutValue := (LValue > RValue);
   ceGreaterEqual: OutValue := (LValue >= RValue);
   ceDifferent   : OutValue := (LValue <> RValue);
  End;

  // if succeeded, create result node and free the temporary ones
  if (OutValue.Kind <> mvUnknown) Then
  Begin
   Result := TConstantExpressionNode.Create(ExprCompiler, Token, OutValue);

   LNode.Free;
   RNode.Free;
  End;
 End;

 if (Result = nil) Then
  Result := TComprasionExpressionNode.Create(ExprCompiler, Token, LNode, RNode, Op);
End;

(* TComparsionExpressionNode.Clone *)
Function TComprasionExpressionNode.Clone: TComprasionExpressionNode;
Begin
 Result := TComprasionExpressionNode.Create(ExprCompiler, Token, Left.Clone(), Right.Clone(), Op);
End;

(* TComprasionExpressionNode.__compile *)
Function TComprasionExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Var LType, RType: TType;
    LArg, RArg  : TExpressionCompileResult;
Begin
 // compile child nodes releasing registers
 CompileChildNodes(LArg, RArg, True, True);

 // do type check
 LType := LArg.getType;
 RType := RArg.getType;

 if (not CheckTypes(LType, RType)) Then
  CompileError(Token, eInvalidUnaryOperator, [LArg.getTypeName, getStringOp, RArg.getTypeName]);

 // put opcode
 PutOpcode(getOpcode, [LArg.getResult, RArg.getResult]);

 // return result
 if (ExprCompiler.getDepth = 1) Then
 Begin
  Result := CreateCompileResult('if', TYPE_BOOL, [ecrImmediate]);
 End Else
 Begin
  Result := ExprCompiler.AllocateCompileResult(TYPE_BOOL, True);
  PutOpcode(o_mov, [Result.getResult, 'if']);
 End;
End;
