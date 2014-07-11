(* TConstantExpressionNode.Create *)
{
 Creates a constant expression node from TMixedValue.
}
Constructor TConstantExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fValue: TMixedValue);
Begin
 inherited Create(fExprCompiler, fToken);

 Value := fValue;

 Case Value.Kind of
  mvBool  : Typ := TYPE_BOOL;
  mvChar  : Typ := TYPE_CHAR;
  mvInt   : Typ := TYPE_INT;
  mvFloat : Typ := TYPE_FLOAT;
  mvString: Typ := TYPE_STRING;

  else
   raise EExpressionNodeException.Create('Cannot create a constant expression node: invalid TMixedValue kind!');
 End;
End;

(* TConstantExpressionNode.Create *)
Constructor TConstantExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const Node: Serialization.TNode);
Begin
 // fetch value kind
 Value.Kind := TMixedValueKind(Node[0].getInt);

 // fetch value
 Case Value.Kind of
  mvBool  : Value.Value.vBool := Node[1].getBool;
  mvChar  : Value.Value.vChar := Node[1].getChar;
  mvInt   : Value.Value.vInt  := Node[1].getInt;
  mvFloat : Value.Value.vFloat := Node[1].getFloat;
  mvString: Value.Value.vString := Node[1].getString;
 End;

 // set the rest of the fields
 Create(fExprCompiler, fToken, Value);
End;

(* TConstantExpressionNode.Clone *)
Function TConstantExpressionNode.Clone: TConstantExpressionNode;
Begin
 Result := TConstantExpressionNode.Create(ExprCompiler, Token, Value);
End;

(* TConstantExpressionNode.getPredictedType *)
Function TConstantExpressionNode.getPredictedType: TMixedValueKind;
Begin
 Result := Value.Kind;
End;

(* TConstantExpressionNode.getValue *)
Function TConstantExpressionNode.getValue: String;
Const BoolTable: Array[Boolean] of String = ('false', 'true');
Begin
 Case Value.Kind of
  mvBool  : Result := BoolTable[Value.Value.vBool];
  mvChar  : Result := '#'+IntToStr(ord(Value.Value.vChar));
  mvInt   : Result := IntToStr(Value.Value.vInt);
  mvFloat : Result := FloatToStr(Value.Value.vFloat);
  mvString: Result := '"'+Value.Value.vString+'"';

  Else
   raise EExpressionNodeException.Create('Invalid MixedValue kind!');
 End;
End;

(* TConstantExpressionNode.getSerializedForm *)
{
 Returns serialized form of this constant node.
}
Function TConstantExpressionNode.getSerializedForm: String;
Begin
 Result := Format('(%d$%s)', [ord(Value.Kind), getValue]);
End;

(* TConstantExpressionNode.__compile *)
Function TConstantExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Begin
 Result.Kind  := ecVariant;
 Result.Value := Value.getValue;
 Result.Typ   := Typ;

 Result := SaveResult(Result, CanBeImmediate);
End;

// -------------------------------------------------------------------------- //
(* TGenericConstantExpressionNode.Create *)
Constructor TGenericConstantExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fValue: T);
Begin
 Value := fValue;

 inherited Create(fExprCompiler, fToken, Value);
End;
