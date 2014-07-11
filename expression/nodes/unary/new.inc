(* TNewExpressionNode.Create *)
Constructor TNewExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode; const fType: TObject);
Begin
 inherited Create(fExprCompiler, fToken, fChild);

 if (fType is TType) Then
  mType := fType Else
  raise EExpressionNodeException.CreateFmt('Invalid fType class instance! (got %s, expected TType)', [fType.ClassName]);
End;

(* TNewExpressionNode.Clone *)
Function TNewExpressionNode.Clone: TNewExpressionNode;
Begin
 Result := TNewExpressionNode.Create(ExprCompiler, Token, Child, mType);
End;

(* TNewExpressionNode.__compile *)
Function TNewExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Var PushedIndexCount: uint8 = 0;

    ArrayType: TType;
    Index    : TExpressionCompileResult;
    Node     : TArrayElementExpressionNode;
Begin
 // clean result
 CleanResult(Result);

 // check if we can compile this at all
 if (mType = nil) Then
 Begin
  // error message had been displayed earlier
  Exit;
 End;

 // fetch array primary type
 ArrayType := TType(mType).Clone;

 While (ArrayType.isArray(False)) Do
  ArrayType := ArrayType.getLowerArray;

 // check if user accidentally doesn't want to create an 'any' array
 if (ArrayType.isAny) Then
 Begin
  CompileError(getToken, eAnyArray, []);
  Exit;
 End;

 // push values
 Node := TArrayElementExpressionNode(getChild);

 While (Node <> nil) Do
 Begin
  // new type[size][]
  if (Node.getRight = nil) Then
  Begin
   PutOpcode(o_push, ['0']);
  End Else

  // new type[size]
  Begin
   // compile index
   Index := MoveResultAtStack(Node.getRight.Compile(True));

   // ensure the subscript is an integer value
   if (not Index.getType.isInt) Then
    CompileError(Node.getRight.getToken, eInvalidArraySubscript, [TType(mType).asString, Index.getType.asString]);
  End;

  // proceed further
  Node := Node.getLeft as TArrayElementExpressionNode;

  Inc(ArrayType.ArrayDimCount);
  Inc(PushedIndexCount);
 End;

 // prepare type
 ArrayType.RefSymbol.Name := '';
 ArrayType.RegPrefix      := 'r';
 ArrayType.ArrayPrimitive := TType(mType);

 // allocate result
 Result := ExprCompiler.AllocateCompileResult(ArrayType);

 // decrement used stack slots (that's what 'arcrt' opcode will do when run)
 ExprCompiler.DecUsedStackSlots(PushedIndexCount);

 // put opcode
 PutOpcode(o_arcrt, [Result.getResult, TType(mType).InternalID, PushedIndexCount]);
End;
