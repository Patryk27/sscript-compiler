Var ret_stp_sub: int16 = 0;

{ GenerateLabelName }
Function GenerateLabelName: String;
Begin
 With TCompiler(Compiler) do
 Begin
  Result := getCurrentFunction.LabelName+'_l_'+IntToStr(SomeCounter);
  Inc(SomeCounter);
 End;
End;

{ RemoveRedundantMovPush }
Procedure RemoveRedundantMovPush(const Expr: PExpressionNode);
Var Opcode: PMOpcode;
Begin
 With TCompiler(Compiler) do
  if (Expr^.ResultOnStack) Then
   if (OpcodeList.Last^.Opcode = o_push) Then
   Begin
    Opcode := OpcodeList.Last;

    OpcodeList.Remove(OpcodeList.Last);

    if (OpcodeList.Last^.Opcode = o_mov) and
       (OpcodeList.Last^.Args[0] = Opcode^.Args[0]) Then
        OpcodeList.Remove(OpcodeList.Last);
   End;
End;

{ Generate }
Procedure Generate(Node: TCFGNode);
Var Child   : TCFGNode;
    Expr    : PExpressionNode;
    ExprType: TType;

    LabelName, LabelFalse, LabelOut, Pos1, Pos2: String;

    PrevDoNotGenerateCode: Boolean;

    ArgList: PVarRecArray;
    I      : Integer;
    Symbol : TSymbol;

    { foreach }
    ForeachVar, ForeachIterator, ForeachExprHolder, ForeachSizeHolder: TVariable;

    { array initializer }
    ArrayVar        : TVariable;
    ArrayElementType: TType;
    ArrayValues     : Array of PExpressionNode;
    ArrayDimSizes   : Array of uint32;
    ArrayTmpDims    : Array of uint32;
    ArrayElementID  : uint32;

    // CompileArrayInitializer
    Procedure CompileArrayInitializer(const Dim: uint8);
    Var I: uint8;
    Begin
     With TCompiler(Compiler) do
     Begin
      if (ArrayDimSizes[Dim] = 0) Then
      Begin
       ExprType := CompileExpression(Compiler, ArrayValues[ArrayElementID]); // compile expression

       if (not (type_equal(ExprType, ArrayElementType))) Then // check types
        CompileError(eWrongType, [ExprType.asString, ArrayElementType.asString]);

 //      if (ArrayValues[ArrayElementID].ResultOnStack) Then
       PutOpcode(o_pop, ['e'+ExprType.RegPrefix+'1']);

       For I := Dim-1 Downto 0 Do
        PutOpcode(o_push, [ArrayTmpDims[I]]);

       PutOpcode(o_arset, [ArrayVar.getAllocationPos(-ArrayVar.Typ.ArrayDimCount), ArrayVar.Typ.ArrayDimCount, 'e'+ExprType.RegPrefix+'1']);

       Inc(ArrayElementID);
      End Else
      Begin
       For I := 0 To ArrayDimSizes[Dim]-1 Do
       Begin
        ArrayTmpDims[Dim] := I;
        CompileArrayInitializer(Dim+1);
       End;
      End;
     End;
    End;
Begin
 if (Node = nil) Then
  Exit;

 if (VisitedNodes.IndexOf(Node) <> -1) Then // if node has been visited more than once, don't compile it again - place a jump instead
 Begin
  TCompiler(Compiler).PutOpcode(o_jmp, [':'+Node.getName]);
  Exit;
 End;
 VisitedNodes.Add(Node);

 PrevDoNotGenerateCode := TCompiler(Compiler).DoNotGenerateCode;

 TCompiler(Compiler).fCurrentNode := Node;

 if (AnythingFromNodePointsAt(Func.FlowGraph.Root, nil, Node) and (Node.Typ <> cetBytecode)) Then
  TCompiler(Compiler).PutLabel(Node.getName);

 Try
  With TCompiler(Compiler) do
   Case Node.Typ of
  { cetNone }
    cetNone:
    Begin
     For Child in Node.Child Do
      Generate(Child);
    End;

  { cetExpression }
    cetExpression:
    Begin
     if (Node.Value <> nil) Then
     Begin
      ExpressionCompiler.CompileExpression(Compiler, Node.Value);
      RemoveRedundantMovPush(Node.Value);
     End;

     if (Node.Child.Count <> 0) Then
      Generate(Node.Child.First);
    End;

  { cetCondition }
    cetCondition:
    Begin
     LabelFalse := GenerateLabelName;
     LabelOut   := GenerateLabelName;

     // compile condition
     Expr     := Node.Value;
     ExprType := ExpressionCompiler.CompileExpression(Compiler, Expr);
     if (not (ExprType.isBool or ExprType.isInt)) Then // condition must be a bool or an int
      CompileError(Expr^.Token, eWrongType, [ExprType.asString, 'bool']);

     VisitedNodes.Add(Node.Child[2]);

     PutOpcode(o_pop, ['if']);
     PutOpcode(o_fjmp, [':'+LabelFalse]);

     // compile 'true'
     Generate(Node.Child[0]);
     PutOpcode(o_jmp, [':'+LabelOut]);

     // compile 'false' (`else`; if possible)
     PutLabel(LabelFalse);
     Generate(Node.Child[1]);

     VisitedNodes.Remove(Node.Child[2]);

     PutLabel(LabelOut);
     Generate(Node.Child[2]);
    End;

 { cetReturn }
   cetReturn:
   Begin
    if (ret_stp_sub > 0) Then // there are some unused data on the stack, that we need to remove before leaving function; in fact, we could just check, how deep in "catch" construction we are, but this solution is simpler (and faster)
    Begin
     PutOpcode(o_sub, ['stp', ret_stp_sub]);
     ret_stp_sub := 0;
    End;

    if (Node.Value = nil) Then // return;
    Begin
     if (not Func.Return.isVoid) Then
      CompileError(Node.getToken, eWrongType, ['void', Func.Return.asString]);

     if (not Func.isNaked) Then
      PutOpcode(o_jmp, [':'+Func.LabelName+'_end']) Else
      PutOpcode(o_ret);
    End Else // return expression;
    Begin
     Expr     := Node.Value;
     ExprType := ExpressionCompiler.CompileExpression(Compiler, Expr);

     if (not ExprType.CanBeAssignedTo(Func.Return)) Then // type check
      CompileError(Expr^.Token, eWrongType, [ExprType.asString, Func.Return.asString]);

     if (Expr^.ResultOnStack) Then // function's result must be in the `e_1` register, not on the stack
      PutOpcode(o_pop, ['e'+Func.Return.RegPrefix+'1']);

     if (not Func.isNaked) Then
      PutOpcode(o_jmp, [':'+Func.LabelName+'_end']) Else
      PutOpcode(o_ret);
    End;

    DoNotGenerateCode := getBoolOption(opt__remove_dead); // code after 'return' is generated but deleted, as it would be never executed anyway
    For Child in Node.Child Do
     Generate(Child);
   End;

 { cetThrow }
   cetThrow:
   Begin
    Expr     := Node.Value;
    ExprType := ExpressionCompiler.CompileExpression(Compiler, Expr);

    if (not ExprType.isString) Then // type check
    Begin
     CompileError(Expr^.Token, eWrongType, [ExprType.asString, 'string']);
     Exit;
    End;

    PutOpcode(o_pop, ['es1']);
    PutOpcode(o_push, ['es1']);
    PutOpcode(o_icall, ['"vm.throw"']);

    For Child in Node.Child Do
     Generate(Child);
   End;

 { cetTryCatch }
   cetTryCatch:
   Begin
    LabelName := Node.getName+'_trycatch';

    { save current exception handler and set the new one }
    PutOpcode(o_icall, ['"vm.save_exception_state"']);
    PutOpcode(o_push, ['@'+LabelName]);
    PutOpcode(o_icall, ['"vm.set_exception_handler"']);

    { parse `try` block }
    Generate(Node.Child[0]);
    PutOpcode(o_jmp, [':'+LabelName+'_end']);

    { parse `catch` block }
    PutLabel(LabelName)^.isPublic := True;
    PutOpcode(o_icall, ['"vm.restore_exception_state"']); // restore previous exception state
    PutOpcode(o_icall, ['"vm.get_last_exception"']); // get exception

    Inc(ret_stp_sub);
    Generate(Node.Child[1]);
    PutOpcode(o_sub, ['stp', 1]); // remove the exception message variable holder
    Dec(ret_stp_sub);

    PutLabel(LabelName+'_end');
    Generate(Node.Child[2]);
   End;

 { cetForeach }
   cetForeach:
   Begin
    LabelName         := Node.getName+'_foreach_loop_';
    ForeachVar        := Node.Foreach.LoopVar as TVariable;
    ForeachIterator   := Node.Foreach.LoopIterVar as TVariable;
    ForeachExprHolder := Node.Foreach.LoopExprHolder as TVariable;
    ForeachSizeHolder := Node.Foreach.LoopSizeHolder as TVariable;

    { compile foreach expression }
    Expr     := Node.Value;
    ExprType := CompileExpression(Compiler, Expr);

    if (type_equal(ForeachVar.Typ, ExprType)) Then
    Begin
     CompileError(Expr^.Token, eInvalidForeach, []);
    End Else
    if (not ExprType.isArray) Then
    Begin
     CompileError(Expr^.Token, eWrongType, [ExprType.asString, 'array']); // error: array is required for a foreach-expression
    End Else
    Begin
     if (not type_equal(ForeachVar.Typ, ExprType.getLowerArray)) Then
      CompileError(Node.getToken, eWrongType, [ForeachVar.Typ.asString, ExprType.getLowerArray.asString]);
    End;

    { get and save foreach expression length }
    if (Expr^.ResultOnStack) Then
     PutOpcode(o_pop, [ForeachExprHolder.getAllocationPos]) Else
     PutOpcode(o_mov, [ForeachExprHolder.getAllocationPos, 'e'+ExprType.RegPrefix+'1']);

    if (ExprType.RegPrefix = 's') Then
    Begin
     PutOpcode(o_strlen, [ForeachExprHolder.getAllocationPos, ForeachSizeHolder.getAllocationPos]);
     PutOpcode(o_mov, [ForeachIterator.getAllocationPos, 1]); // strings are iterated from 1
    End Else
    Begin
     PutOpcode(o_arlen, [ForeachExprHolder.getAllocationPos, 1, ForeachSizeHolder.getAllocationPos]);
     PutOpcode(o_mov, [ForeachIterator.getAllocationPos, 0]);
    End;

    PutLabel(LabelName+'content');

    if (ExprType.RegPrefix = 's') Then
     PutOpcode(o_if_le, [ForeachIterator.getAllocationPos, ForeachSizeHolder.getAllocationPos]) Else
     PutOpcode(o_if_l, [ForeachIterator.getAllocationPos, ForeachSizeHolder.getAllocationPos]);
    PutOpcode(o_fjmp, [':'+LabelName+'end']);

    PutOpcode(o_push, [ForeachIterator.getAllocationPos]);

    Pos1 := ForeachExprHolder.getAllocationPos(-1);
    Pos2 := ForeachVar.getAllocationPos(-1);

    PutOpcode(o_arget, [Pos1, 1, Pos2]); // arget(ForeachExprHolder, 1, ForeachVar)

    Generate(Node.Child[0]);

    PutOpcode(o_add, [ForeachIterator.getAllocationPos, 1]); // ForeachIterator++
    PutOpcode(o_jmp, [':'+LabelName+'content']); // jump to the beginning of the loop

    PutLabel(LabelName+'end');
    Generate(Node.Child[1]);
   End;

 { cetArrayInitializer }
   cetArrayInitializer:
   Begin
    ArrayVar         := (Node.ArrayInitializer.VarSymbol as TSymbol).mVariable;
    ArrayElementType := ArrayVar.Typ.ArrayBase;
    ArrayValues      := Node.ArrayInitializer.Values;
    ArrayDimSizes    := Node.ArrayInitializer.DimSizes;

    SetLength(ArrayTmpDims, 256);

    { allocate array (in bytecode) }
    For I := High(ArrayDimSizes) Downto Low(ArrayDimSizes) Do
    Begin
     if (ArrayDimSizes[I] = 0) Then
      Continue;

     PutOpcode(o_push, [ArrayDimSizes[I]]);
    End;

    PutOpcode(o_arcrt, ['er1', ArrayVar.Typ.InternalID, ArrayVar.Typ.ArrayDimCount]);
    PutOpcode(o_mov, [ArrayVar.getAllocationPos, 'er1']);

    { compile initializer }
    ArrayElementID := 0;
    CompileArrayInitializer(0);

    { compile further nodes }
    if (Node.Child.Count > 0) Then
     Generate(Node.Child[0]);
   End;

 { cetBytecode }
   cetBytecode:
   Begin
    if (Node.Bytecode.OpcodeName = '') Then
     PutLabel(Node.Bytecode.LabelName) Else
     Begin
      ArgList := Node.Bytecode.OpcodeArgList;

      For I := Low(ArgList^) To High(ArgList^) Do
      Begin
       if (Pos('localvar.', ArgList^[I].VPChar) > 0) Then
        For Symbol in Func.SymbolList Do
         With Symbol do
          if (Typ = stVariable) Then
           ArgList^[I].VPChar := CopyStringToPChar(StringReplace(ArgList^[I].VPChar, 'localvar.'+IntToStr(LongWord(mVariable)), mVariable.getAllocationPos, [rfReplaceAll]));
      End;

      PutOpcode(Node.Bytecode.OpcodeName, ArgList^, Node.getToken);
     End;

    For Child in Node.Child Do
     Generate(Child);
   End;

    else
     CompileError(eInternalError, ['Generate() -> unexpected Node.Typ = '+IntToStr(ord(Node.Typ))]);
   End;
 Finally
  TCompiler(Compiler).DoNotGenerateCode := PrevDoNotGenerateCode;
 End;
End;

// -------------------------------------------------------------------------- //
{ tree optimizations }
{$I opt_deadcode.pas}
{$I opt_expressions.pas}
{$I opt_branch_simplification.pas}

{ variable allocator }
{$I variable_allocator.pas}
// -------------------------------------------------------------------------- //

(* AddPrologCode *)
Procedure AddPrologCode;
Var Symbol  : TSymbol;
    StackReg: PStackSavedReg;
    StDec   : uint16 = 0;
Begin
 With TCompiler(Compiler) do
 Begin
  { function info }
  PutComment('--------------------------------- //');
  PutComment('Function name   : '+Func.RefSymbol.Name);
  PutComment('Declared at line: '+IntToStr(Func.RefSymbol.DeclToken^.Line));
  PutComment('--------------------');

  PutComment('Parameters:');
  For Symbol in Func.SymbolList Do
   if (not Symbol.isInternal) and (Symbol.Typ = stVariable) and (Symbol.mVariable.isFuncParam) Then
    PutComment('`'+Symbol.Name+'` allocated at: '+Symbol.mVariable.getAllocationPos);

  PutComment('');

  PutComment('Variables:');
  For Symbol in Func.SymbolList Do // @TODO: StDec?
   if (not Symbol.isInternal) and (Symbol.Typ = stVariable) and (not Symbol.mVariable.isFuncParam) Then
    PutComment('`'+Symbol.Name+'` allocated in: '''+Symbol.mVariable.getAllocationPos+''', scope range: '+IntToStr(Symbol.mVariable.RefSymbol.Range.PBegin.Line)+'..'+IntToStr(Symbol.mVariable.RefSymbol.Range.PEnd.Line)+' lines, CFG cost: '+IntToStr(getVariableCFGCost(Symbol, Func.FlowGraph.Root, nil)));

  PutComment('--------------------------------- //');

  PutOpcode(o_loc_func, ['"'+Func.RefSymbol.Name+'"']);

  With Func do
  Begin
   StDec := StackSize;

   { if register is taken by (a) variable(s), we need to at first save this register's value (and restore it at the end of the function) }
   if (not isNaked) Then
    For StackReg in StackRegs Do
    Begin
     PutOpcode(o_push, ['e'+StackReg^.RegChar+IntToStr(StackReg^.RegID)]);
     Inc(StDec);
    End;

   { allocate local stack variables }
   if (not isNaked) Then
    PutOpcode(o_add, ['stp', StackSize]);

   { fix some variables' positions }
   if (not isNaked) Then
    For Symbol in SymbolList Do
     if (Symbol.Typ = stVariable) and (Symbol.mVariable.LocationData.Location = vlStack) and
        (Symbol.mVariable.isFuncParam) Then
     Begin
      Symbol.mVariable.LocationData.StackPosition -= StDec;
     End;
  End;

  { new label (main function's body; nothing should jump here - it's just facilitation for optimizer so it doesn't possibly remove the epilog code) }
  PutLabel(Func.LabelName+'_body');
 End;
End;

(* AddEpilogCode *)
Procedure AddEpilogCode;
Var I: int32;
Begin
 With TCompiler(Compiler) do
 Begin
  { function end code }
  PutLabel(Func.LabelName+'_end');

  With Func do
  Begin
   if (not isNaked) Then
    PutOpcode(o_sub, ['stp', Func.StackSize]);

   For I := StackRegs.Count-1 Downto 0 Do
    PutOpcode(o_pop, ['e'+StackRegs[I]^.RegChar+IntToStr(StackRegs[I]^.RegID)]);
  End;

  PutOpcode(o_ret);
 End;
End;

(* GenerateBytecode *)
Procedure GenerateBytecode(Root: TCFGNode);
Begin
 VisitedNodes.Clear;
 Generate(Root);
End;
