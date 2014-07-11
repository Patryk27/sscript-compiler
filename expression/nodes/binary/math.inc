(* TMathExpression.getOpcodeKind *)
Function TMathExpressionNode.getOpcode: TOpcodeKind;
Const Table: Array[TMathExpressionNodeKind] of TOpcodeKind = (o_add, o_sub, o_mul, o_div, o_mod);
Begin
 Result := Table[Op];
End;

(* TMathExpressionNode.getStringOp *)
Function TMathExpressionNode.getStringOp: String;
Const Table: Array[TMathExpressionNodeKind] of String = ('+', '-', '*', '/', '%');
Begin
 Result := Table[Op];
End;

(* TMathExpressionNode.Create *)
Constructor TMathExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TMathExpressionNodeKind);
Begin
 inherited Create(fExprCompiler, fToken, fLeft, fRight);

 Op := fOp;
End;

(* TMathExpressionNode.Optimize *)
Function TMathExpressionNode.Optimize: TExpressionNode;
Const ValidTypes: Set of TMixedValueKind = [mvChar, mvInt, mvFloat];
Var LType, RType: TMixedValueKind;
    MathNode    : TMathExpressionNode;
Begin
 MathNode := TMathExpressionNode.Create(ExprCompiler, Token, Left.Optimize(), Right.Optimize(), Op);
 Result   := MathNode;

 // do quick type-check
 LType := Left.getPredictedType();
 RType := Right.getPredictedType();

 if (not ((LType in ValidTypes) and (RType in ValidTypes))) Then
  Exit;

 // swap nodes for numeric addition and subtraction if the right node is larger: a + a*b -> a*b + a
 if (MathNode.Op in [meAdd, meMul]) and
    (MathNode.getRight.getCost > MathNode.getLeft.getCost) Then
 Begin
  MathNode.SwapNodes;
 End;

 // exprA - exprA -> 0
{ if (Op = meSub) and (Result.Left.isEqual(Result.Right)) Then
 Begin
  @TODO
 End; }

 // @TODO: +0, -0, *0, *-1 and the other cases
End;

(* TMathExpressionNode.Evaluate *)
Function TMathExpressionNode.Evaluate: TExpressionNode;
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
   meAdd: OutValue := LValue + RValue;
   meSub: OutValue := LValue - RValue;
   meMul: OutValue := LValue * RValue;
   meMod: OutValue := LValue mod RValue;

   meDiv:
   Begin
    if (RValue.isZero) Then
     CompileError(getToken, eDivByZero, []);

    OutValue := LValue / RValue;
   End;
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
  Result := TMathExpressionNode.Create(ExprCompiler, Token, LNode, RNode, Op);
End;

(* TMathExpressionNode.Clone *)
Function TMathExpressionNode.Clone: TMathExpressionNode;
Begin
 Result := TMathExpressionNode.Create(ExprCompiler, Token, Left.Clone(), Right.Clone(), Op);
End;

(* TMathExpressionNode.__compile *)
Function TMathExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Var LType, RType: TType;
    LArg, RArg  : TExpressionCompileResult;
    RegID       : uint8;

  { ReleaseRegisters }
  Procedure ReleaseRegisters(const ReleaseLeft: Boolean=True; const ReleaseRight: Boolean=True);
  Begin
   if (ReleaseLeft) Then
    ExprCompiler.ReleaseRegister(LArg);

   if (ReleaseRight) Then
    ExprCompiler.ReleaseRegister(RArg);
  End;

Label InvalidOperator;
Begin
 CleanResult(Result);

 // compile child nodes
 CompileChildNodes(LArg, RArg, False);

 // type check
 LType := LArg.getType;
 RType := RArg.getType;

 // special case: modulo is defined only for ints and chars
 if (Op = meMod) and (not ((LType.isInt or RType.isChar) and (RType.isInt or RType.isChar))) Then
  goto InvalidOperator;

 // int <op> int, float <op> float, int <op> char, char <op> int
 if ((LType.isNumerical and RType.isNumerical) and (LType.RegPrefix = RType.RegPrefix)) or
    (LType.isChar and RType.isInt) or
    (LType.isInt and RType.isChar) Then
 Begin
  ReleaseRegisters(False, True);

  PutOpcode(getOpcode, [LArg.getResult, RArg.getResult]);
  Result := LArg;
 End Else

 // int <op> float
 if (LType.isInt) and (RType.isFloat) Then
 Begin
  {
   @Note: we can do a simple small optimization here.

   Because:
   > int+float = float+int
   > int*float = float*int

   It is possible to swap the arguments and skip casting.
   But only if second argument is not an immediate value, otherwise we could overwrite some variable value.
  }
  if (Op in [meAdd, meMul]) and (not RArg.isImmediate) Then
  Begin
   ReleaseRegisters(True, False);

   PutOpcode(getOpcode, [RArg.getResult, LArg.getResult]);

   Result := RArg;
   Exit;
  End;

  // cast int -> float
  if (ExprCompiler.FindFreeRegister('f', RegID)) Then
  Begin
   Result := CreateCompileResult('ef'+IntToStr(RegID), RArg.Typ, []);
  End Else
  Begin
   raise EExpressionNodeException.Create('Couldn''t have found any free float register to do casting!');
  End;

  // do float math
  PutOpcode(o_mov, [Result.getResult, LArg.getResult]);
  PutOpcode(getOpcode, [Result.getResult, RArg.getResult]);
  Result := SaveResult(Result, CanBeImmediate);
  ReleaseRegisters;
 End Else

 // float <op> int
 if (LType.isFloat) and (RType.isInt) Then
 Begin
  ReleaseRegisters(False, True);
  PutOpcode(getOpcode, [LArg.getResult, RArg.getResult]);
  Result := LArg;
 End Else

 // char/string + string/char
 if (LType.isString or LType.isChar) and (RType.isString or RType.isChar) and (Op = meAdd) Then
 Begin
  // string + char/string
  if (LType.isString) Then
  Begin
   ReleaseRegisters(False, True);
   Result := LArg;
  End Else

  // char + string
  Begin
   {
    @Note:
      It's impossible (and senseless) to do `strjoin(char reg, ...)`, so we need to
      cast that char reg to a string reg at first and the do the string joining.
   }
   ReleaseRegisters(True, True);

   Result := ExprCompiler.AllocateCompileResult(TYPE_STRING);
   PutOpcode(o_mov, [Result.getResult, LArg.getResult]);
  End;

  PutOpcode(o_strjoin, [Result.getResult, RArg.getResult]);
 End Else

 // invalid
 Begin
 InvalidOperator:

  CompileError(getToken, eInvalidBinaryOperator, [LArg.getTypeName, getStringOp, RArg.getTypeName]);
  ReleaseRegisters;
 End;

 Result := SaveResult(Result, True);
End;
