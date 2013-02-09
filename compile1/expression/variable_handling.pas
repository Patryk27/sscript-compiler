{ __variable_setvalue_reg }
(*
 Sets a variable's value to the value stored in register identified by `RegChar`+`RegID`.

 When eg.a variable is located in `ei3` and it has ID 1, calling:
  __variable_setvalue_reg(1, 4, 'i');
 Will add opcode:
  mov(ei3, ei4)

 `PushedValues` is also important, because when a variable is allocated on the stack and something is pushed onto it, eg. pseudocode:
   push(ei3)
   __variable_setvalue_reg(...)
  We need to take care of that `push`, because we could overwrite not our variable, but some value on the stack.
*)
Procedure __variable_setvalue_reg(VarID: Integer; RegID: Byte; RegChar: Char);
Var RegStr: String;
Begin
 RegStr := 'e'+RegChar+IntToStr(RegID); { get full register name (ei1, es3 etc.) }

 With Compiler.getCurrentFunction.VariableList[VarID] do
 Begin
  if (isConst) Then
   Error(eInternalError, ['Cannot change a constant value']);

  if (RegID > 0) Then // variable is in the register
   Compiler.PutOpcode(o_mov, ['e'+RegChar+IntToStr(RegID), RegStr]) Else
   Compiler.PutOpcode(o_mov, ['['+IntToStr(RegID-PushedValues)+']', RegStr]); // variable is on the stack
 End;
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
Function __variable_getvalue_reg(VarID: Integer; RegID: Byte; RegChar: Char): TVType;
Var RegStr: String;
Begin
 RegStr := 'e'+RegChar+IntToStr(RegID); { get full register name }

 With Compiler do
  With getCurrentFunction.VariableList[VarID] do
  Begin
   Result := Typ;

   if (isConst) Then
   Begin
    PutOpcode(o_mov, [RegStr, getValueFromExpression(Compiler, @Value)]);
   End Else
    if (RegID > 0) Then // variable is in the register
     PutOpcode(o_mov, [RegStr, 'e'+RegChar+IntToStr(RegID)]) Else
     PutOpcode(o_mov, [RegStr, '['+IntToStr(RegID-PushedValues)+']']); // variable is on the stack
  End;
End;

{ __variable_getvalue_stack }
(*
 Pushes variable's value onto the stack.
 See examples and descriptions above.
*)
Function __variable_getvalue_stack(VarID: Integer): TVType;
Begin
 With Compiler do
  With getCurrentFunction.VariableList[VarID] do
  Begin
   Result := Typ;

   if (isConst) Then
   Begin
    PutOpcode(o_push, [getValueFromExpression(Compiler, @Value)]);
   End Else
    if (RegID > 0) Then // variable is in the register
     PutOpcode(o_push, ['e'+RegChar+IntToStr(RegID)]) Else
     PutOpcode(o_push, ['['+IntToStr(RegID-PushedValues)+']']); // variable is on the stack
  End;

 Inc(PushedValues);
End;

{ __variable_getvalue_array_reg }
(*
 See description of `__variable_getvalue_reg`
*)
Function __variable_getvalue_array_reg(VarID: Integer; RegID: Byte; RegChar: Char; ArrayElements: PMExpression): TVType;
Var RegStr: String;
Begin
 RegStr := 'e'+RegChar+IntToStr(RegID); { get full register name }

 With Compiler do
  With getCurrentFunction.VariableList[VarID] do
  Begin
   if (not isTypeArray(Typ)) Then
    Exit(TYPE_ANY);

   if (ArrayElements = nil) Then
    Error(eInternalError, ['ArrayElements = nil']);

   if (ArrayElements^.Typ = mtVariable) Then
    Exit(Typ); // return variable's type and exit procedure

   Result := Parse(ArrayElements);

   if (ArrayElements^.ResultOnStack) Then
    PutOpcode(o_pop, [RegStr]) Else
    PutOpcode(o_mov, [RegStr, 'e'+getTypePrefix(Result)+'1']);
  End;

 ArrayElements^.ResultOnStack := False;
End;

{ __variable_setvalue_array_reg }
(*
 See description of `__variable_setvalue_reg`
*)
Function __variable_setvalue_array_reg(VarID: Integer; RegID: Byte; RegChar: Char; ArrayElements: PMExpression): TVType;
Var RegStr    : String;
    TmpType   : TVType;
    TmpExpr   : PMExpression;
    IndexCount: Integer;
    Variable  : TRVariable;
Begin
 RegStr := 'e'+RegChar+IntToStr(RegID); { get full register name }

 With Compiler do
  With getCurrentFunction.VariableList[VarID] do
  Begin
   if (not isTypeArray(Typ)) Then
    Exit(TYPE_ANY);

   if (ArrayElements = nil) Then
    Error(eInternalError, ['ArrayElements = nil']);

   { find variable }
   TmpExpr := ArrayElements;
   While (TmpExpr^.Typ <> mtVariable) Do
    TmpExpr := TmpExpr^.Left;
   Variable := getVariable(TmpExpr);

   { parse indexes }
   IndexCount := 0;

   Repeat
    TmpType := Parse(ArrayElements^.Right);
    With Compiler do // array subscript must be an integer value
     if (not isTypeInt(TmpType)) Then
     Begin
      Error(eInvalidArraySubscript, [getTypeName(Variable.Typ), getTypeName(TmpType)]);
      Exit;
     End;

    ArrayElements := ArrayElements^.Left;
    Inc(IndexCount);
   Until (ArrayElements^.Typ = mtVariable);

   { set new value }
   PutOpcode(o_arset, ['e'+getTypePrefix(Variable.Typ)+'1', IndexCount, RegStr]);
  End;
End;
