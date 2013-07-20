Var ret_stp_sub: int16 = 0;

{ GenerateLabelName }
Function GenerateLabelName: String;
Begin
 With TCompiler(Compiler) do
 Begin
  Result := getCurrentFunction.MangledName+'_l_'+IntToStr(SomeCounter);
  Inc(SomeCounter);
 End;
End;

{ RemoveRedundantPush }
Procedure RemoveRedundantPush;
Begin
 With TCompiler(Compiler) do
  if (OpcodeList.Last^.Opcode = o_push) Then
   With OpcodeList.Last^ do
    OpcodeList.Remove(OpcodeList.Last);
End;

{ Generate }
Procedure Generate(Node: TCFGNode);
Var Child   : TCFGNode;
    Expr    : PExpressionNode;
    ExprType: TType;

    LabelName, LabelFalse, LabelOut: String;

    PrevDoNotGenerateCode: Boolean;

    ArgList: PVarRecArray;
    I      : Integer;
    Symbol : TSymbol;
Begin
 if (Node = nil) Then
  Exit;

 if (VisitedNodes.IndexOf(Node) <> -1) Then // if node has been visited more than once, don't compile it again
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
     ExpressionCompiler.CompileExpression(Compiler, Node.Value);
     RemoveRedundantPush;

     //With TCompiler(Compiler) do
     // if (OpcodeList.Last^.Opcode = o_mov) and
     //    (OpcodeList.Last^.Args[0].Typ in [ptBoolReg, ptCharReg, ptIntReg, ptFloatReg, ptStringReg]) and
     //    (OpcodeList.Last^.Args[0].Value = 1) Then
     //     OpcodeList.Remove(OpcodeList.Last); // kinda ugly hack, but let's not talk about it...

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
      PutOpcode(o_jmp, [':'+Func.MangledName+'_end']) Else
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
           ArgList^[I].VPChar := CopyStringToPChar(StringReplace(ArgList^[I].VPChar, 'localvar.'+IntToStr(LongWord(mVariable)), mVariable.getBytecodePos, [rfReplaceAll]));
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
(* ValidateGraph *)
Procedure ValidateGraph;
Var isThereAnyReturn: Boolean = False;

  // CheckTryCatch
  Procedure CheckTryCatch(Node: TCFGNode);
  Var Child: TCFGNode;
  Begin
   if (Node = nil) Then
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node);

   if (Node.Typ = cetTryCatch) and (Node.Child.Count = 2) Then
   Begin
    (* @Note:

      In some specific cases, "try..catch" construction has only 2 children, not 3; like here:

      function<void> foo()
      {
       try
       {
        a();
       } catch(msg)
       {
        b();
       }
      }
      (because no code appears after the try..catch construction)

      This could crash optimizer as well as the code generator (as they expect 'cetTryCatch'-typed nodes to have exactly 3 children), so we're just inserting a `nil` child-node into this node.
    *)

    Node.Child.Add(nil);
   End;

   For Child in Node.Child Do
    CheckTryCatch(Child);
  End;

  // CheckReturn
  Procedure CheckReturn(Node, EndNode: TCFGNode);
  Var Child: TCFGNode;
  Begin
   if (Node = nil) or (Node = EndNode) Then // if encountered nil or end node...
    Exit;

   if (VisitedNodes.IndexOf(Node) <> -1) Then
    Exit;
   VisitedNodes.Add(Node); // add node to the visited list

   if (Node.Typ = cetReturn) Then // if 'return'
   Begin
    isThereAnyReturn := True;

    if (Node.Child.Count = 1) Then // any code appearing after 'return' is 'unreachable'...
     if (Node.Child[0] <> EndNode) and // ...if it isn't ending node
        (VisitedNodes.IndexOf(Node.Child[0]) = -1) and // ...and if it hasn't been already visited
        (Node.Child[0].Value <> nil) Then // ...and ofc. - if it's an expression
     Begin
      TCompiler(Compiler).CompileHint(Node.Child[0].getToken, hUnreachableCode, []);
      VisitedNodes.Add(Node.Child[0]);
     End;

    Exit;
   End;

   if (Node.Child.Count = 0) and (Node.Value <> nil) Then // if it's an edge node with some expression and it isn't 'return', show warning
   Begin
    isThereAnyReturn := True; // otherwise the message below would be shown 2 times instead of one
    TCompiler(Compiler).CompileWarning(TCompiler(Compiler).Parser.next_pnt(-1), wNotEveryPathReturnsAValue, []);
   End;

   if (Node.Typ = cetCondition) Then
   Begin
    CheckReturn(Node.Child[0], Node.Child[2]);
    CheckReturn(Node.Child[1], Node.Child[2]);
    CheckReturn(Node.Child[2], nil);
   End Else

   if (Node.Typ = cetTryCatch) Then
   Begin
    if (Node.Child[0].isThere(cetReturn)) and (Node.Child[1].isThere(cetReturn)) Then // if both nodes ("try" and "catch") return a value...
    Begin
     isThereAnyReturn := True;

     Node := Node.Child[2];

     if (Node <> nil) and (Node.Value <> nil) Then
      TCompiler(Compiler).CompileHint(Node.getToken, hUnreachableCode, []);
    End Else
     CheckReturn(Node.Child[2], EndNode);
   End Else

    For Child in Node.Child Do
     CheckReturn(Child, EndNode);
  End;

Begin
 VisitedNodes.Clear;
 CheckTryCatch(Func.FlowGraph.Root);

 if (not Func.Return.isVoid) and (not Func.isNaked) Then
 Begin
  VisitedNodes.Clear;
  CheckReturn(Func.FlowGraph.Root, nil);

  if (not isThereAnyReturn) Then
   TCompiler(Compiler).CompileWarning(TCompiler(Compiler).Parser.next_pnt(-1), wNotEveryPathReturnsAValue, []);
 End;
End;

{$I dead_code.pas}
{$I opt_expressions.pas}
{$I opt_branches.pas}
{$I variable_allocation.pas}

(* AddPrologCode *)
Procedure AddPrologCode;
Var SavedRegs  : uint16 = 0;
    Symbol     : TSymbol;
Begin
 VarsOnStack := 0;

 With TCompiler(Compiler) do
 Begin
  { function info }
  PutComment('--------------------------------- ;');
  PutComment('Function name   : '+Func.RefSymbol.Name);
  PutComment('Declared at line: '+IntToStr(Func.RefSymbol.DeclToken^.Line));
  PutComment('--------------------');

  PutComment('Parameters:');
  For Symbol in Func.SymbolList Do
   if (not Symbol.isInternal) and (Symbol.Typ = stVariable) and (Symbol.mVariable.isFuncParam) Then
    PutComment('`'+Symbol.Name+'` allocated at: '+Symbol.mVariable.getBytecodePos);

  PutComment('');

  PutComment('Variables:');
  For Symbol in Func.SymbolList Do
   if (not Symbol.isInternal) and (Symbol.Typ = stVariable) and (not Symbol.mVariable.isFuncParam) Then
    PutComment('`'+Symbol.Name+'` allocated at: '+Symbol.mVariable.getBytecodePos+', scope range: '+IntToStr(Symbol.mVariable.RefSymbol.Range.PBegin.Line)+'-'+IntToStr(Symbol.mVariable.RefSymbol.Range.PEnd.Line)+' lines');

  PutComment('--------------------------------- ;');

  PutOpcode(o_loc_func, ['"'+Func.RefSymbol.Name+'"']);

  { allocate local stack variables }
  if (not Func.isNaked) Then
  Begin
   With Func do
    For Symbol in SymbolList Do // each symbol
     if (Symbol.Typ = stVariable) Then // if variable
      With Symbol.mVariable do
       if (MemPos <= 0) and (not DontAllocate) Then
        Inc(VarsOnStack); // next variable to allocate

   PutOpcode(o_add, ['stp', VarsOnStack+1]); // `+1`, because `stack[stp]` is the caller's IP (instruction pointer)
  End;

  { if register is occupied by variable, we need to at first save this register's value (and restore it at the end of the function) }
  SavedRegs := 0;
  With Func do
  Begin
   For Symbol in SymbolList Do // each symbol
    if (Symbol.Typ = stVariable) Then // if variable
     With Symbol.mVariable do
      if (MemPos > 0) Then // if allocated in register
      Begin
       PutOpcode(o_push, ['e'+Typ.RegPrefix+IntToStr(MemPos)]);
       Inc(SavedRegs);
      End;

   For Symbol in SymbolList Do // each symbol
    if (Symbol.Typ = stVariable) Then // if variable
     With Symbol.mVariable do
      if (MemPos <= 0) Then // if allocated on the stack
      Begin
       MemPos -= SavedRegs;

       if (isFuncParam) Then
        MemPos -= VarsOnStack;
      End;
  End;

  { new label (main function's body; nothing should jump here, it's just facilitation for optimizerm, so it doesn't touch the epilog code) }
  PutLabel(Func.MangledName+'_body');
 End;
End;

(* AddEpilogCode *)
Procedure AddEpilogCode;
Var I     : int32;
    Symbol: TSymbol;
Begin
 With TCompiler(Compiler) do
 Begin
  { function end code }
  PutLabel(Func.MangledName+'_end');

  With Func do
  Begin
   For I := SymbolList.Count-1 Downto 0 Do
   Begin
    Symbol := SymbolList[I];
    if (Symbol.Typ = stVariable) Then // if variable
     With Symbol.mVariable do
      if (MemPos > 0) Then // if allocated in register
       PutOpcode(o_pop, ['e'+Typ.RegPrefix+IntToStr(MemPos)]);
   End;
  End;

  if (not Func.isNaked) Then
   PutOpcode(o_sub, ['stp', VarsOnStack+1]);

  PutOpcode(o_ret);
 End;
End;

(* GenerateBytecode *)
Procedure GenerateBytecode(Root: TCFGNode);
Begin
 VisitedNodes.Clear;
 Generate(Root);
End;
