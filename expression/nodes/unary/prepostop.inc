(* TPrePostOpExpressionNode.getOpcode *)
Function TPrePostOpExpressionNode.getOpcode: TOpcodeKind;
Const Table: Array[TPrePostOpExpressionNodeKind] of TOpcodeKind = (o_add, o_sub, o_add, o_sub);
Begin
 Result := Table[Op];
End;

(* TPrePostOpExpressionNode.getStringOp *)
Function TPrePostOpExpressionNode.getStringOp: String;
Const Table: Array[TPrePostOpExpressionNodeKind] of String = ('++', '--', '++', '--');
Begin
 Result := Table[Op];
End;

(* TPrePostOpExpressionNode.isPre *)
Function TPrePostOpExpressionNode.isPre: Boolean;
Begin
 Result := (Op in [pePreInc, pePreDec]);
End;

(* TPrePostOpExpressionNode.isPost *)
Function TPrePostOpExpressionNode.isPost: Boolean;
Begin
 Result := (Op in [pePostInc, pePostDec]);
End;

(* TPrePostOpExpressionNode.__compile *)
Function TPrePostOpExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Var Arg: TExpressionCompileResult;
    Typ: TType;
Begin
 // check if child is an l-value
 if (not Child.isLValue) Then
  CompileError(getToken, eLValueExpected, []);

 // __compile child (sacrifice it to satan, in fact)
 Arg := Child.Compile(True);

 // fetch type
 Typ := Arg.getType;

 // do type check
 if (not Typ.isNumerical) Then
  CompileError(getToken, eInvalidUnaryOperator, [Arg.getTypeName, getStringOp]);

 // save result
 if (isPre) Then
 Begin
  Result := SaveResult(Arg, True);
 End Else
 Begin
  {
   @Note:
    A useful optimization can be done here - namely:

    Expression "a++" has bassicaly the same outcome as "++a" if its result is not used anywhere, right?
    So "a++;" (notice the semicolon) and "++a;" do the same thing - increase the value of variable 'a';
    Of course not in cases like: "x = a++" (when the result is used).

    Thus just check if current node deep is equal to one - if it is, the result of post inc/dec will
    not be used anywhere (we have pure "var++;" expression) and this operator can be optimized.
  }

  Result := SaveResult(Arg, ExprCompiler.getDepth = 1);
 End;

 // put opcode
 PutOpcode(getOpcode, [Arg.getResult, 1]);
End;

(* TPrePostOpExpressionNode.Create *)
Constructor TPrePostOpExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode; const fOp: TPrePostOpExpressionNodeKind);
Begin
 inherited Create(fExprCompiler, fToken, fChild);

 Op := fOp;
End;

(* TPrePostOpExpressionNode.Clone *)
Function TPrePostOpExpressionNode.Clone: TPrePostOpExpressionNode;
Begin
 Result := TPrePostOpExpressionNode.Create(ExprCompiler, Token, Child, Op);
End;
