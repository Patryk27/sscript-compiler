(* TCallExpressionNode.CompileParameterList *)
Procedure TCallExpressionNode.CompileParameterList(const FunctionName: String; const fParamList: Pointer);
Var ParamList: TFunctionParamList;
    ArgID    : int32;
    Argument : TExpressionNode;

    RequiredArgCount: uint8 = 0;

    Arg: TExpressionCompileResult;
    Typ: TType;
Begin
 // fetch function parameter list
 ParamList := PFunctionParamList(fParamList)^;

 // get the minimum number of passed arguments
 For ArgID := Low(ParamList) To High(ParamList) Do
 Begin
  if (ParamList[ArgID].DefaultValue <> nil) and (not ParamList[ArgID].isVar) Then
   Break Else
   Inc(RequiredArgCount);
 End;

 // check argument count
 if (not (ArgumentList.Count in [RequiredArgCount..Length(ParamList)])) Then
 Begin
  CompileError(getToken, eWrongArgumentCount, [FunctionName, RequiredArgCount, ArgumentList.Count]);
  Exit;
 End;

 // compile from the last argument to the first
 For ArgID := High(ParamList) Downto Low(ParamList) Do
 Begin
  // parse argument or default value
  if (ArgID >= ArgumentList.Count) Then
   Argument := ParamList[ArgID].DefaultValue Else
   Argument := ArgumentList[ArgID];

  // lvalue required?
  if (ParamList[ArgID].isVar) and (not Argument.isLValue) Then
  Begin
   CompileError(Argument.getToken, eLValueExpected, []);
  End;

  // special case: by-ref-parameter and default value
  if (ParamList[ArgID].isVar) and (ParamList[ArgID].DefaultValue <> nil) Then
   Arg := ParamList[ArgID].DefaultValue.Compile(True) Else
   Arg := Argument.Compile(True);

  // fetch type
  Typ := Arg.getType;

  // do push
  PutOpcode(o_push, [Arg.getResult]);
  ExprCompiler.IncUsedStackSlots;

  // release used register (if any)
  ExprCompiler.ReleaseRegister(Arg);

  // type check
  if (not Typ.CanBeAssignedTo(ParamList[ArgID].Typ)) Then
  Begin
   CompileError(Argument.getToken, eWrongTypeInCall, [FunctionName, ArgID+1, Typ.asString, ParamList[ArgID].Typ.asString]);
  End;
 End;
End;

(* TCallExpressionNode.CleanAfterCall *)
{
 Cleans stack after a bytecode call.

 @TODO: foo(var int x, var int y, var int z) -> those variables can be loaded using 'pop's - no need to do 3 movs and 1 sub.
}
Procedure TCallExpressionNode.CleanAfterCall(const fParamList: Pointer);
Var ParamList: TFunctionParamList;

    IdentNode: TIdentifierExpressionNode;
    ArrayNode: TArrayElementExpressionNode;

    Argument: String;
    ArgID   : int32;

    Index: TExpressionCompileResult;
Begin
 // fetch function parameter list
 ParamList := PFunctionParamList(fParamList)^;

 // iterate arguments
 For ArgID := Low(ParamList) To High(ParamList) Do
 Begin
  Argument := Format('[-%d]', [ArgID]);

  if (ParamList[ArgID].isVar) and
     (ArgID < ArgumentList.Count) Then
  Begin
   ArrayNode := TArrayElementExpressionNode(ArgumentList[ArgID]);
   IdentNode := TIdentifierExpressionNode(ArgumentList[ArgID]);

   // array element: call(var[3])
   if (ArgumentList[ArgID] is TArrayElementExpressionNode) Then
   Begin
    ArrayNode.__prepare;
    ArrayNode.__compile_indexes(Index);

    PutOpcode(o_arset1, [ArrayNode.ValueRef.getResult, Index.getResult, Format('[-%d]', [ArgID])]);

    ExprCompiler.ReleaseRegister(Index);
    ExprCompiler.ReleaseRegister(ArrayNode.ValueRef);
   End Else

   // identifier: call(var)
   if (ArgumentList[ArgID] is TIdentifierExpressionNode) Then
   Begin
    PutOpcode(o_mov, [IdentNode.getLocation, Argument]);
   End;
  End;
 End;

 // clean stack
 PutOpcode(o_sub, ['stp', Length(ParamList)]);
 ExprCompiler.DecUsedStackSlots(Length(ParamList));
End;

(* TCallExpressionNode.__compile *)
Function TCallExpressionNode.__compile(const CanBeImmediate: Boolean): TExpressionCompileResult;

  { RegularCall }
  Procedure RegularCall;
  Var ParamList: TFunctionParamList;

      Ident: TIdentifierExpressionNode;
      Func : TFunction;
      Arg  : TExpressionCompileResult;
  Begin
   // get identifier expression node
   Ident := TIdentifierExpressionNode(Child);

   // fetch function object instance
   Func := Ident.getSymbol.mFunction;

   // compile parameter list
   ParamList := Func.ParamList;
   CompileParameterList(Func.RefSymbol.getFullName('::'), @ParamList);

   // put opcode
   if (Length(Func.LabelName) > 0) Then
    PutOpcode(o_call, [':'+Func.LabelName]) Else
    PutOpcode(o_call, [':$function.'+IntToStr(PtrUInt(Func))]);

   // clean after call
   CleanAfterCall(@ParamList);

   // fill the return structure
   Result.Typ := Func.Return;

   if (not Result.getType.isVoid) Then
   Begin
    {
     @Note:
       We're ommiting generation of an unnecessary 'mov' if the function result
       wouldn't be used anyway. That is eg. "foo();" <- no need to load result
       to a separate register, just return e#0 and profit.
    }
    if (ExprCompiler.getDepth > 1) Then
    Begin
     Result := ExprCompiler.AllocateCompileResult(Func.Return);
     PutOpcode(o_mov, [Result.getResult, 'e'+Func.Return.RegPrefix+'0']);
    End Else
    Begin
     Result := CreateCompileResult('e'+Func.Return.RegPrefix+'0', Func.Return, [ecrImmediate]);
    End;
   End;
  End;

  { MethodCall }
  Procedure MethodCall;
  Var UnknownMethod: Boolean = False;

      Field: TFieldFetchExpressionNode;

      Expr    : TExpressionCompileResult;
      ExprType: TType;

      Arg    : TExpressionCompileResult;
      ArgType: TType;
  Begin
   // @Note: it's of course fake OOP

   // fetch field
   Field := TFieldFetchExpressionNode(Child);

   // compile expression (on which the field has been applied)
   Expr     := Field.getChild.Compile(True);
   ExprType := Expr.getType;

   // release register
   ExprCompiler.ReleaseRegister(Expr);

   // arrays
   if (ExprType.isArray) Then
   Begin
    // int array.length()
    if (Field.getFieldName = 'length') Then
    Begin
     // allocate result
     Result := ExprCompiler.AllocateCompileResult(TYPE_INT);

     // check argument count
     if (ArgumentList.Count <> 0) Then
      CompileError(getToken, eWrongArgumentCount, ['length', 0, ArgumentList.Count]);

     // put opcode
     if (ExprType.isString) and (ExprType.ArrayDimCount = 1) Then
     Begin
      PutOpcode(o_strlen, [Expr.getResult, Result.getResult]);
     End Else
     Begin
      PutOpcode(o_arlen, [Expr.getResult, Result.getResult]);
     End;
    End Else

    // void array.resize(int newsize)
    if (ExprType.isArray(False)) and (Field.getFieldName = 'resize') Then
    Begin
     // set result
     Result.Typ := TYPE_VOID;

     // check argument count
     if (ArgumentList.Count <> 1) Then
      CompileError(getToken, eWrongArgumentCount, ['resize', 1, ArgumentList.Count]);

     // compile argument
     Arg     := ArgumentList[0].Compile(True);
     ArgType := Arg.getType;

     // release argument
     ExprCompiler.ReleaseRegister(Arg);

     // do type check
     if (not ArgType.isInt) Then
      CompileError(ArgumentList[0].getToken, eWrongTypeInCall, ['resize', 1, ArgType.asString, 'int']);

     // put opcode
     PutOpcode(o_arres, [Expr.getResult, Arg.getResult]);
    End Else

    Begin
     UnknownMethod := True;
    End;
   End Else
   Begin
    UnknownMethod := True;
   End;

   // unknown
   if (UnknownMethod) Then
   Begin
    CompileError(getToken, eMethodNotFound, [Field.getFieldName, ExprType.asString]);
   End;
  End;

  { OtherCall }
  Procedure OtherCall;
  Var CalleeName: String;

      CalleeType: TType;
      Callee    : TExpressionCompileResult;

      ReturnType: TType;
      ParamList : TFunctionParamList;
  Begin
   // compile callee
   Callee     := Child.Compile(True);
   CalleeType := Callee.getType;

   // do type check
   if (not CalleeType.isFunctionPointer) Then
   Begin
    CompileError(getToken, eCannotBeUsedAsFunction, [CalleeType.asString]);
    Exit;
   End;

   // get callee name
   if (Child is TIdentifierExpressionNode) Then
    CalleeName := TIdentifierExpressionNode(Child).getSymbol.getFullName('::') Else
    CalleeName := 'function expression';

   // fetch parameter list and return type
   ParamList  := CalleeType.FuncParams;
   ReturnType := CalleeType.FuncReturn;

   // compile parameter list
   CompileParameterList(CalleeName, @ParamList);

   // put opcode
   PutOpcode(o_acall, [Callee.getResult]);

   // release callee register
   ExprCompiler.ReleaseRegister(Callee);

   // clean after call
   CleanAfterCall(@ParamList);

   // fill return structure
   Result.Typ := ReturnType;

   if (not ReturnType.isVoid) Then
   Begin
    if (ExprCompiler.getDepth > 1) Then
    Begin
     Result := ExprCompiler.AllocateCompileResult(ReturnType);
     PutOpcode(o_mov, [Result.getResult, 'e'+ReturnType.RegPrefix+'0']);
    End Else
    Begin
     Result := CreateCompileResult('e'+ReturnType.RegPrefix+'0', ReturnType, [ecrImmediate]);
    End;
   End;
  End;

Var Ident: TIdentifierExpressionNode;
Begin
 // clean result
 CleanResult(Result);

 // regular function call
 Ident := TIdentifierExpressionNode(Child);

 if (Child is TIdentifierExpressionNode) and (Ident.getSymbol <> nil) and (Ident.getSymbol.Typ = stFunction) Then
  RegularCall() Else

 // method call
 if (Child is TFieldFetchExpressionNode) Then
  MethodCall() Else

 // array-call, cast-call, var-call (...)
  OtherCall();
End;

(* TCallExpressionNode.Create *)
Constructor TCallExpressionNode.Create(const fExprCompiler: TExpressionCompiler; const fToken: TToken_P; const Callee: TExpressionNode);
Begin
 inherited Create(fExprCompiler, fToken, Callee);

 ArgumentList := TCallArgumentList.Create;
End;

(* TCallExpressionNode.Evaluate *)
Function TCallExpressionNode.Evaluate: TCallExpressionNode;
Var Tmp: TExpressionNode;
    I  : int32;
Begin
 Result := Clone();

 With Result do
 Begin
  For I := 0 To ArgumentList.Count-1 Do
  Begin
   Tmp := ArgumentList[I].Evaluate();

   ArgumentList[I].Free;
   ArgumentList[I] := Tmp;
  End;
 End;
End;

(* TCallExpressionNode.Clone *)
Function TCallExpressionNode.Clone: TCallExpressionNode;
Var I: int32;
Begin
 Result := TCallExpressionNode.Create(ExprCompiler, Token, Child);

 For I := 0 To ArgumentList.Count-1 Do
  Result.AddArgument(ArgumentList[I].Clone());
End;

(* TCallExpressionNode.getCost *)
Function TCallExpressionNode.getCost: uint32;
Var I: int32;
Begin
 Result := inherited getCost;

 For I := 0 To ArgumentList.Count-1 Do
  Result += ArgumentList[I].getCost;
End;

(* TCallExpressionNode.AddArgument *)
Procedure TCallExpressionNode.AddArgument(const Expr: TExpressionNode);
Begin
 ArgumentList.Add(Expr);
End;
