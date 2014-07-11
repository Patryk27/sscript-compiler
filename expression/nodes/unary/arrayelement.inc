(* TArrayElementExpressionNode.__prepare *)
Procedure TArrayElementExpressionNode.__prepare;
Begin
 // find array origin (a variable/function call/...)
 ArrayRefNode := self;
 While (ArrayRefNode is TArrayElementExpressionNode) Do
  ArrayRefNode := TArrayElementExpressionNode(ArrayRefNode).getLeft;

 // get array variable name
 if (ArrayRefNode is TIdentifierExpressionNode) Then
  ArrayVarName := TIdentifierExpressionNode(ArrayRefNode).getName Else
  ArrayVarName := 'an immediate array';

 // compile array reference
 ValueRef     := ArrayRefNode.Compile(True);
 ArrayTypeObj := ValueRef.getType;
 PopResult(ValueRef);
End;

(* TArrayElementExpressionNode.__compile_indexes *)
{
 Pushes array indexes onto the stack.
 Do NOT forget about releasing the value (array) reference and index in the expression compiler after putting the arget/arset opcode!
}
Procedure TArrayElementExpressionNode.__compile_indexes(out Index: TExpressionCompileResult);
Var NodeList: Array of TArrayElementExpressionNode;
    NodeID  : int32;

    CurrentNode: TBinaryExpressionNode;
    IndexNode  : TArrayElementExpressionNode;

    ValueType: TType;

    Tmp: TExpressionCompileResult;
Begin
 // get current array type
 ValueType := TType(ArrayTypeObj);

 // prepare node list
 CurrentNode := self;
 SetLength(NodeList, 0);

 While (CurrentNode is TArrayElementExpressionNode) Do
 Begin
  SetLength(NodeList, Length(NodeList)+1);
  NodeList[High(NodeList)] := TArrayElementExpressionNode(CurrentNode);

  CurrentNode := TBinaryExpressionNode(CurrentNode.getLeft);
 End;

 // fetch lower arrays
 For NodeID := High(NodeList) Downto Low(NodeList) Do
 Begin
  CurrentNode := NodeList[NodeID];

  // cast current node
  IndexNode := TArrayElementExpressionNode(CurrentNode);

  // leave if nothing left to compile
  if (IndexNode.getRight = nil) Then
   Exit;

  // compile index
  Index := IndexNode.getRight.Compile(True);
  PopResult(Index);

  // fetch lower array
  if (NodeID > 0) Then
  Begin
   // special branch for immediates (so that possible variable reference doesn't get overwritten: 'arget1(var, ..., var)')
   if (ValueRef.isImmediate) Then
   Begin
    Tmp := ExprCompiler.AllocateCompileResult(ValueRef.getType.getLowerArray);
    PutOpcode(o_arget1, [ValueRef.getResult, Index.getResult, Tmp.getResult]);
    ValueRef := Tmp;
   End Else
   Begin
    PutOpcode(o_arget1, [ValueRef.getResult, Index.getResult, Tmp.getResult]);
   End;
  End;

  // release index
  ExprCompiler.ReleaseRegister(Index);

  // check for valid subscript use
  if (not ValueType.isArray) or (not Index.getType.isInt) Then
  Begin
   Tmp.Typ := ValueType;

   CompileError(IndexNode.getToken, eInvalidArraySubscript, [Tmp.getTypeName, Index.getTypeName]);
   Exit;
  End;

  // decrease array type
  ValueType := ValueType.getLowerArray;
 End;

 // save value type
 ValueTypeObj := ValueType;
End;

(* TArrayElementExpressionNode.__compile_array *)
Function TArrayElementExpressionNode.__compile_array(const CanBeImmediate: Boolean): TExpressionCompileResult;
Var Opcode: TOpcodeKind;
    Index : TExpressionCompileResult;
    Ret   : TType;
Begin
 // compile indexes
 __compile_indexes(Index);

 // release registers
 ExprCompiler.ReleaseRegister(ValueRef);
 ExprCompiler.ReleaseRegister(Index);

 // allocate result
 Result := ExprCompiler.AllocateCompileResult(ValueTypeObj);

 // put opcode
 if (TType(ArrayTypeObj).isString) and (Result.getType.isChar) Then
  Opcode := o_strget Else
  Opcode := o_arget1;

 PutOpcode(Opcode, [ValueRef.getResult, Index.getResult, Result.getResult]);

 // 'arget1(array, index, string reg)' returns the *original* string, not its copy - if such condition happens, we must manually clone it so it doesn't get overwritten later
 Ret := Result.getType;

 if (Opcode = o_arget1) and (Ret.isString) and (Ret.ArrayDimCount = 1) Then
  PutOpcode(o_strclone, [Result.getResult]);
End;

(* TArrayElementExpressionNode.Clone *)
Function TArrayElementExpressionNode.Clone: TArrayElementExpressionNode;
Begin
 Result := TArrayElementExpressionNode.Create(ExprCompiler, Token, Left.Clone(), Right.Clone());
End;

(* TArrayElementExpressionNode.isLValue *)
Function TArrayElementExpressionNode.isLValue: Boolean;
Begin
 Result := True;
End;

(* TArrayElementExpressionNode.__compile *)
Function TArrayElementExpressionNode.__compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult;
Begin
 // prepare
 __prepare;

 // call handler
 Result := __compile_array(CanBeImmediate);
End;
