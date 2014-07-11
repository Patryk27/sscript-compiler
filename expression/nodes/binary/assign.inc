(* TAssignExpressionNode.getOpcode *)
Function TAssignExpressionNode.getOpcode(const ReplaceAddToStrjoin: Boolean): TOpcodeKind;
Const Table: Array[TAssignExpressionNodeKind] of TOpcodeKind = (o_mov, o_add, o_sub, o_mul, o_div, o_mod, o_shl, o_shr, o_or, o_and, o_xor);
Begin
 Result := Table[Op];

 if (ReplaceAddToStrjoin) and (Result = o_add) Then
  Result := o_strjoin;
End;

(* TAssignExpressionNode.getStringOp *)
Function TAssignExpressionNode.getStringOp: String;
Const Table: Array[TAssignExpressionNodeKind] of String = ('=', '+=', '-=', '*=', '/=', '%=', '<<=', '>>=', '|=', '&=', '^=');
Begin
 Result := Table[Op];
End;

(* TAssignExpressionNode.__compile_variable *)
Function TAssignExpressionNode.__compile_variable(const CanBeImmediate: Boolean; out LArg, RArg: TExpressionCompileResult; out VarName: String): TExpressionCompileResult;
Var VarRegReleased: Boolean = False;

    LType: TType;

    Variable: TVariable;
    VarNode : TIdentifierExpressionNode = nil;
Begin
 CleanResult(Result);

 VarNode  := TIdentifierExpressionNode(Left);
 Variable := VarNode.getSymbol.mVariable;

 {
  @Note:
   We can temporarily extend our register set if:
   - we're doing a regular assign ('=') to a variable
   - that variable is located in a register
   - and it's not used on the right side of the equation
 }
 if (Op = aeAssign) and
    (Variable.LocationData.Location = vlRegister) and
    (not Right.findIdentifier(VarNode.getSymbol)) Then
 Begin
  ExprCompiler.ReleaseRegister(Variable.Typ.RegPrefix, Variable.LocationData.RegisterID);
  VarRegReleased := True;
 End;

 // compile right side
 RArg := Right.Compile(True);
 PopResult(RArg);

 // compile left side
 With TIdentifierExpressionNode(Left) do
 Begin
  Result := CreateCompileResult(getLocation, getVariable.Typ, [ecrImmediate, ecrHoldsVariable]);

  VarName  := getName;
  LArg.Typ := getType;
  LType    := TType(getType);
 End;

 PutOpcode(getOpcode(LType.isString), [Result.getResult, RArg.getResult]);
 ExprCompiler.InvalidateRegister(Result);

 // mark the register back as occupied
 if (VarRegReleased) Then
 Begin
  ExprCompiler.OccupyRegister(Variable.Typ.RegPrefix, Variable.LocationData.RegisterID);
 End;
End;

(* TAssignExpressionNode.__compile_array *)
Function TAssignExpressionNode.__compile_array(const CanBeImmediate: Boolean; out LArg, RArg: TExpressionCompileResult; out VarName: String): TExpressionCompileResult;
Var ValueType, ArrayType: TType;

    IndexNode: TArrayElementExpressionNode;
    Index    : TExpressionCompileResult;

    TmpValue: TExpressionCompileResult;

    OpGet, OpSet: TOpcodeKind;
Begin
 CleanResult(Result);

 // compile right side
 RArg := Right.Compile(True);
 PopResult(RArg);

 // fetch index node pointer
 IndexNode := TArrayElementExpressionNode(Left);

 // compile that nodes
 IndexNode.__prepare;
 IndexNode.__compile_indexes(Index);

 VarName  := IndexNode.ArrayVarName;
 LArg.Typ := IndexNode.ValueTypeObj;

 // put opcodes
 ArrayType := TType(IndexNode.ArrayTypeObj);
 ValueType := TType(IndexNode.ValueTypeObj);

 if (ArrayType.isString) and (ValueType.isChar) Then
 Begin
  OpGet := o_strget;
  OpSet := o_strset;
 End Else
 Begin
  OpGet := o_arget1;
  OpSet := o_arset1;
 End;

 if (Op = aeAssign) Then
 Begin
  PutOpcode(OpSet, [IndexNode.ValueRef.getResult, Index.getResult, RArg.getResult]);

  // set result
  Result := RArg;
 End Else
 Begin
  TmpValue := ExprCompiler.AllocateCompileResult(LArg.Typ);

  // load value from array
  PutOpcode(OpGet, [IndexNode.ValueRef.getResult, Index.getResult, TmpValue.getResult]);

  // do math
  PutOpcode(getOpcode(OpGet = o_strget), [TmpValue.getResult, RArg.getResult]);

  // store value back to the array
  PutOpcode(OpSet, [IndexNode.ValueRef.getResult, Index.getResult, TmpValue.getResult]);

  // set result
  Result := TmpValue;

  // release register
  ExprCompiler.ReleaseRegister(RArg);
 End;

 // release registers
 ExprCompiler.ReleaseRegister(Index);
 ExprCompiler.ReleaseRegister(IndexNode.ValueRef);
End;

(* TAssignExpressionNode.Create *)
Constructor TAssignExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TAssignExpressionNodeKind);
Begin
 inherited Create(fExprCompiler, fToken, fLeft, fRight);

 Op := fOp;
End;

(* TAssignExpressionNode.Clone *)
Function TAssignExpressionNode.Clone: TAssignExpressionNode;
Begin
 Result := TAssignExpressionNode.Create(ExprCompiler, Token, Left.Clone(), Right.Clone(), Op);
End;

(* TAssignExpressionNode.__compile *)
Function TAssignExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Var LType, RType: TType;
    LArg, RArg  : TExpressionCompileResult;
    VarName     : String;

  { InvalidOperator }
  Procedure InvalidOperator;
  Begin
   CompileError(getToken, eInvalidBinaryOperator, [LArg.getTypeName, getStringOp, RArg.getTypeName]);
  End;

Begin
 CleanResult(Result);

 // if variable
 if (Left is TIdentifierExpressionNode) Then
 Begin
  Result := __compile_variable(CanBeImmediate, LArg, RArg, VarName);
 End Else

 // if array
 if (Left is TArrayElementExpressionNode) Then
 Begin
  Result := __compile_array(CanBeImmediate, LArg, RArg, VarName);
 End Else

 // fail otherwise
 Begin
  CompileError(getToken, eLValueExpected, []);
  Exit;
 End;

 // fetch types
 LType := LArg.getType;
 RType := RArg.getType;

 // do type consistency check
 if (not RType.CanBeAssignedTo(LType)) Then
 Begin
  CompileError(getToken, eWrongTypeInAssign, [VarName, RArg.getTypeName, LArg.getTypeName]);
 End Else

 // do type-operator check
 if (LType.isString) Then
 Begin
  // only string = string and string += string are valid
  if (not (Op in [aeAssign, aeAddAssign])) Then
   InvalidOperator;
 End;
End;
