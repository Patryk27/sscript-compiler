Var ret_stp_sub: Integer = 0;

{ ParseConstruction }
Procedure ParseConstruction(ID: Integer);

   // ParseUntil
  Procedure ParseUntil(M: TMConstructionType);
  Begin
   Inc(c_ID);
   While (CList[c_ID].Typ <> M) Do // parse loop
   Begin
    ParseConstruction(c_ID);
    Inc(c_ID);
   End;
  End;

  // RemoveRedundantPush
  Procedure RemoveRedundantPush;
  Begin
   With TCompiler(Compiler) do
    if (OpcodeList.Last^.Opcode = o_push) Then
     With OpcodeList.Last^ do
      OpcodeList.Delete(OpcodeList.Count-1);
  End;

// function body
Var Str1, Str2, Str3, Str4: String;
    Expr                  : PMExpression;
    EType                 : TType;
    Item                  : PMOpcode;

    PrevDoNotGenerateCode: Boolean;

    if_remove_true, if_remove_false: Boolean; // used for `--remove-unreachable` optimization in `if` construction
Begin
 With TCompiler(Compiler) do
 Begin
  if (ID > High(CList)) Then
  Begin
   DevLog('Info: ParseConstruction() -> (ID > High(CList)); leaving function...');
   Exit;
  End;

  if (CList[ID].Token <> nil) Then
   With CList[ID].Token^ do
    PutOpcode(o_loc_line, [Line]); // write line location

  PrevDoNotGenerateCode := DoNotGenerateCode;

  With CList[ID] do
  Begin
   Case Typ of
 (* ctNone *)
    ctNone: ;

 (* ctJump *)
    ctJump:
    Begin
     PutOpcode(o_jmp, [PChar(Values[0])]);
    End;

 (* ctLabel *)
    ctLabel:
    Begin
     if (findLabel(PChar(Values[0])) <> -1) Then
      CompileError(PToken_P(Values[1])^, eRedeclaration, [PChar(Values[0])]);

     New(Item);
     With Item^ do
     Begin
      Name    := PChar(Values[0]);
      isLabel := True;
     End;
     OpcodeList.Add(Item);
    End;

 (* ctExpression *)
    ctExpression:
    Begin
     ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
     RemoveRedundantPush;
    End;

 (* ctReturn *)
    ctReturn:
    Begin
     if (ret_stp_sub > 0) Then // there are some unused data on the stack, that we need to remove
     Begin
      PutOpcode(o_sub, ['stp', ret_stp_sub]);
      ret_stp_sub := 0;
     End;

     EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);

     if (not EType.CanBeAssignedTo(Func.Return)) Then // type check
      CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, Func.Return.asString]);

     if (PMExpression(Values[0])^.ResultOnStack) Then // function's result must be in the `e_1` register, not on the stack
      PutOpcode(o_pop, ['e'+Func.Return.RegPrefix+'1']);

     if (not Func.isNaked) Then
      PutOpcode(o_jmp, [':'+Func.MangledName+'_end']) Else
      PutOpcode(o_ret);
    End;

 (* ctVoidReturn *)
    ctVoidReturn:
    Begin
     if (ret_stp_sub > 0) Then // there is unused data on the stack, that we need to remove
     Begin
      PutOpcode(o_sub, ['stp', ret_stp_sub]);
      ret_stp_sub := 0;
     End;

     if (not Func.Return.isVoid) Then // type check
      CompileError(PToken_P(Values[0])^, eWrongType, ['void', Func.Return.asString]);
     PutOpcode(o_ret);
    End;

 (* ctInlineBytecode *)
    ctInlineBytecode: PutOpcode(PChar(Values[0]), PVarRecArray(Values[1])^, Values[2]);

 (* ctFOR *)
    ctFOR:
    Begin
     Str1 := PChar(Values[2]);
     Str2 := Str1+'condition';
     Str3 := Str1+'step';
     Str4 := Str1+'end';

     { condition }
     PutLabel(Str2);
     if (Values[0] <> nil) Then
     Begin
      EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
      if (not (EType.isBool or EType.isInt)) Then
       CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'bool']);
     End Else
      PutOpcode(o_push, ['true']);

     { condition check }
     PutOpcode(o_pop, ['if']);
     PutOpcode(o_fjmp, [':'+Str4]);

     ParseUntil(ctFOR_end);

     { step }
     PutLabel(Str3);
     if (Values[1] <> nil) Then
     Begin
      ExpressionCompiler.CompileConstruction(Compiler, Values[1]);
      RemoveRedundantPush;
     End;

     PutOpcode(o_jmp, [':'+Str2]);

     PutLabel(Str4); { end }
    End;

 (* ctIF *)
    ctIF:
    Begin
     Str1 := PChar(Values[1]);
     Str2 := Str1+'true';
     Str3 := Str1+'false';
     Str4 := Str1+'end';

     if_remove_false := False;
     if_remove_true  := False;

     { condition }
     Expr  := Values[0];
     EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
     if (not (EType.isBool or EType.isInt)) Then
      CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'bool']);

     if (getBoolOption(opt__remove_unreachable)) Then // can try to optimize?
      if (isConstantValue(Expr^)) Then // can be optimized?
      Begin
       if (Expr^.Value) Then
        if_remove_false := True { remove the `else` part } Else
        if_remove_true := True;
      End;

     { jump }
     PutOpcode(o_pop, ['if']);

     if not (if_remove_false or if_remove_true) Then
      PutOpcode(o_fjmp, [':'+Str3]);

     { on true }
     DoNotGenerateCode := if_remove_true;
     PutLabel(Str2);
     ParseUntil(ctIF_end);
     if (not if_remove_false) Then
      PutOpcode(o_jmp, [':'+Str4]); // if the `else` construction is removed, there'll be no need to jump over it.
     DoNotGenerateCode := PrevDoNotGenerateCode;

     { on false (the `else` construction) }
     DoNotGenerateCode := if_remove_false;
     PutLabel(Str3);
     if (CList[c_ID+1].Typ = ctIF_else) Then // compile 'else'
     Begin
      Inc(c_ID); // skip `ctIF_else`
      ParseUntil(ctIF_end);
     End;
     DoNotGenerateCode := PrevDoNotGenerateCode;

     PutLabel(Str4);
    End;

 (* ctWHILE *)
    ctWHILE:
    Begin
     Str1 := PChar(Values[1]);
     Str2 := Str1+'condition';
     Str3 := Str1+'end';

     { condition (loop begin) }
     PutLabel(Str2);
     EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
     if (not (EType.isBool or EType.isInt)) Then
      CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'bool']);

     { condition check }
     PutOpcode(o_pop, ['if']);
     PutOpcode(o_fjmp, [':'+Str3]);

     { loop body }
     ParseUntil(ctWHILE_end);

     PutOpcode(o_jmp, [':'+Str2]);

     { loop end }
     PutLabel(Str3);
    End;

 (* ct_DO_WHILE *)
    ct_DO_WHILE:
    Begin
     Str1 := PChar(Values[0]);
     Str2 := Str1+'begin';
     Str3 := Str1+'end';

     { loop begin }
     PutLabel(Str2);

     { parse loop }
     ParseUntil(ct_DO_WHILE_end);

     { condition }
     With CList[c_ID] do
     Begin
      EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
      if (not (EType.isBool or EType.isInt)) Then
       CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'bool']);
     End;

     { condition check }
     PutOpcode(o_pop, ['if']);
     PutOpcode(o_tjmp, [':'+Str2]);

     { loop end }
     PutLabel(Str3);
    End;

 (* ctDELETE *)
    ctDELETE:
    Begin
     EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
     if (not EType.isObject) Then
      CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'object']);

     PutOpcode(o_pop, ['er1']);
     PutOpcode(o_objfree, ['er1']);
    End;

 (* ctTHROW *)
    ctTHROW:
    Begin
     EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
     if (not EType.isString) Then
     Begin
      CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'string']);
      Exit;
     End;

     PutOpcode(o_pop, ['es1']);
     PutOpcode(o_push, ['es1']);
     PutOpcode(o_icall, ['"vm.throw"']);
    End;

 (* ctTRY *)
    ctTRY:
    Begin
     { save current exception handler and set the new one }
     PutOpcode(o_icall, ['"vm.save_exception_state"']);
     PutOpcode(o_push, ['@'+PChar(Values[0])]);
     PutOpcode(o_icall, ['"vm.set_exception_handler"']);

     { parse `try` block }
     ParseUntil(ctCatch);
     PutOpcode(o_jmp, [':'+PChar(Values[0])+'_end']);

     { parse `catch` block }
     PutLabel(PChar(Values[0]))^.isPublic := True;
     PutOpcode(o_icall, ['"vm.restore_exception_state"']); // restore previous exception state
     PutOpcode(o_icall, ['"vm.get_last_exception"']); // get exception
     Inc(ret_stp_sub);

     ParseUntil(ctCATCH_END);
     PutOpcode(o_sub, ['stp', 1]); // remove the exception message variable holder
     Dec(ret_stp_sub);
     PutLabel(PChar(Values[0])+'_end');
    End;

    else
     CompileError(Token, eInternalError, ['Unexpected construction: `'+GetEnumName(TypeInfo(TMConstructionType), ord(Typ))+'`']);
   End;
  End;
 End;
End;
