(* __string_char_get *)
Procedure __string_char_get;
Var IndexType: TType;
Begin
 // parse the left side (to ei1, preferably)
 IndexType := Parse(Right, 1, 'i');

 // ensure it's int
 if (not IndexType.isInt) Then
 Begin
  Error(Right^.Token, eInvalidArraySubscript, ['string', IndexType.asString]);
  Exit;
 End;

 // repop
 RePop(Right, IndexType, 1);

 // put opcode
 if (FinalRegID > 0) Then
 Begin
  Compiler.PutOpcode(o_strget, ['es1', 'ei1', getFinalReg('c')]);

  FinalRegDone := True;
 End Else
 Begin
  Compiler.PutOpcode(o_strget, ['es1', 'ei1', 'ec1']);
 End;

 Result := TYPE_CHAR;
End;

(* __array_get *)
Procedure __array_get(const ArrayType: TType);
Var Typ, IndexType: TType;
    IndexCount    : Integer;
    OutReg        : String;
    Index         : PExpressionNode;
Begin
 Typ := ArrayType.Clone;

 // push indexes onto the stack
 IndexCount := 0;
 Index      := Expr;

 Repeat
  IndexType := Parse(Index^.Right);

  // make sure array subscript is an integer value
  if (not IndexType.isInt) or (Typ.ArrayDimCount = 0) Then
  Begin
   Error(Index^.Right^.Token, eInvalidArraySubscript, [Typ.asString, IndexType.asString]);
   Exit;
  End;

  Index := Index^.Left;

  Dec(Typ.ArrayDimCount);
  Inc(IndexCount);
 Until (Index^.Typ <> mtArrayElement);

 // special case - if we have just "array[index]", we can use "arget1" opcode
 if (IndexCount = 1) Then
 Begin
  RePop(Expr^.Right, TYPE_INT, 1);
 End;

 // output type change
 if (Typ.ArrayDimCount = 0) Then
 Begin
  Typ.RegPrefix := Typ.ArrayBase.RegPrefix;
 End;

 if (Typ.isString and (Typ.ArrayDimCount = 1)) Then // `string`
  Typ := TYPE_STRING;

 if (Typ.isString and (Typ.ArrayDimCount = 0)) Then // `string`
  Typ := TYPE_CHAR;

 // fetch value
 if (FinalRegID > 0) Then
 Begin
  OutReg       := getFinalReg(Typ.RegPrefix);
  FinalRegDone := True;
 End Else
 Begin
  OutReg := 'e'+Typ.RegPrefix+'1';
 End;

 if (IndexCount = 1) Then
 Begin
  Compiler.PutOpcode(o_arget1, ['e'+ArrayType.RegPrefix+'1', 'ei1', OutReg]);
  // PushedValues has been decremented in the "RePop" routine above
 End Else
 Begin
  Compiler.PutOpcode(o_arget, ['e'+ArrayType.RegPrefix+'1', IndexCount, OutReg]);
  Dec(PushedValues, IndexCount);
 End;

 // set result value
 Result := Typ;
End;

(* ParseArrayElement *)
Procedure ParseArrayElement;
Var ArrayType: TType;
    Origin   : PExpressionNode;
Begin
 // find array origin (a variable, function call, type cast...)
 Origin := Expr;
 While (Origin^.Typ = mtArrayElement) Do
  Origin := Origin^.Left;

 // load array/string reference (preferably to the er1/es1 register)
 ArrayType := Parse(Origin, 1);
 RePop(Origin, ArrayType, 1);

 // call the appropriate code generator (as there's a difference between "string[index]" and "intarray[index]" in opcodes)
 if (ArrayType.isString) and (ArrayType.ArrayDimCount = 1) Then
  __string_char_get() Else
  __array_get(ArrayType);
End;
