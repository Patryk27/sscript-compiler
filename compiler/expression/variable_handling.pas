{ __variable_setvalue_reg }
(*
 Sets a variable's value to the value stored in register identified by `RegChar`+`RegID`.

 When eg.a variable is located in `ei3` and it has ID 1, calling:
  __variable_setvalue_reg(1, 4, 'i');
 Will add opcode:
  mov(ei3, ei4)
*)
Function __variable_setvalue_reg(_var: TRVariable; RegID: Byte; RegChar: Char): TType;
Var RegStr: String;
Begin
 Result := _var.Typ;
 RegStr := 'e'+RegChar+IntToStr(RegID);

 Compiler.PutOpcode(o_mov, [_var.PosStr, RegStr]);
End;

{ __variable_getvalue_reg }
(*
 Loads a variable's value onto the register identified by `RegChar`+`RegID`.
 See description above for info about `PushedValues`.

 Example:
 We have a string variable (with ID 2) loaded onto the `es4` and we want to load it's value into `es1`:
  __variable_getvalue_reg(2, 4, 's');
 This will add one opcode:
  mov(es1, es4)
*)
Function __variable_getvalue_reg(_var: TRVariable; RegID: Byte; RegChar: Char): TType;
Var RegStr: String;
Begin
 Result := _var.Typ;
 RegStr := 'e'+RegChar+IntToStr(RegID);

 With Compiler do
  if (_var.isConst) and (_var.Value <> nil) Then
   PutOpcode(o_mov, [RegStr, getValueFromExpression(_var.Value)]) Else
   PutOpcode(o_mov, [RegStr, _var.PosStr]);
End;

{ __variable_getvalue_stack }
(*
 Pushes variable's value onto the stack.
 See examples and descriptions above.
*)
Function __variable_getvalue_stack(_var: TRVariable): TType;
Begin
 Result := _var.Typ;

 With Compiler do
  if (_var.isConst) Then
   PutOpcode(o_push, [getValueFromExpression(_var.Value)]) Else
   PutOpcode(o_push, [_var.PosStr]);

 Inc(PushedValues);
End;

{ __variable_getvalue_array_reg }
(*
 See description of @__variable_getvalue_reg
*)
Function __variable_getvalue_array_reg(_var: TRVariable; RegID: Byte; RegChar: Char; ArrayElements: PExpressionNode): TType;
Var RegStr: String;
Begin
 RegStr := 'e'+RegChar+IntToStr(RegID);

 With Compiler do
 Begin
  if (not _var.Typ.isArray) Then
   Exit(TYPE_ANY);

  if (ArrayElements = nil) Then
   Error(eInternalError, ['ArrayElements = nil']);

  if (ArrayElements^.Typ = mtIdentifier) Then // @what?!
   Exit(_var.Typ); // return variable's type and exit procedure

  Result := Parse(ArrayElements);

  if (ArrayElements^.ResultOnStack) Then
  Begin
   PutOpcode(o_pop, [RegStr]);
   Dec(PushedValues);
  End Else
   PutOpcode(o_mov, [RegStr, 'e'+Result.RegPrefix+'1']);
 End;

 ArrayElements^.ResultOnStack := False;
End;

{ __variable_setvalue_array_reg }
(*
 See description of @__variable_setvalue_reg
*)
Function __variable_setvalue_array_reg(_var: TRVariable; RegID: Byte; RegChar: Char; ArrayElements: PExpressionNode): TType;
Var RegStr    : String;
    TmpType   : TType;
    TmpExpr   : PExpressionNode;
    IndexCount: Integer;
    Variable  : TRVariable;
Begin
 Result := _var.Typ;
 RegStr := 'e'+RegChar+IntToStr(RegID); { get full register name }

 With Compiler do
 Begin
  if (not _var.Typ.isArray) Then
   Exit(TYPE_ANY);

  if (ArrayElements = nil) Then
   Error(eInternalError, ['ArrayElements = nil']);

  { find variable }
  TmpExpr := ArrayElements;
  While (TmpExpr^.Typ <> mtIdentifier) Do
   TmpExpr := TmpExpr^.Left;
  Variable := getVariable(TmpExpr);

  { parse indexes }
  IndexCount := 0;

  Repeat
   TmpType := Parse(ArrayElements^.Right);
   With Compiler do // array subscript must be an integer value
    if (not TmpType.isInt) Then
    Begin
     Error(eInvalidArraySubscript, [Variable.Typ.asString, TmpType.asString]);
     Exit;
    End;

   ArrayElements := ArrayElements^.Left;
   Inc(IndexCount);
  Until (ArrayElements^.Typ = mtIdentifier);

  { set new value }
  PutOpcode(o_arset, [Variable.PosStr, IndexCount, RegStr]);
  Dec(PushedValues, IndexCount);
 End;
End;
