(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
Unit BCGenerator;

 Interface
 Uses SSCompiler, symdef, FlowGraph;

 { TBCGenerator }
 Type TBCGenerator =
      Class
       Private
        Compiler    : TCompiler;
        CurrentFunc : TFunction;
        VisitedNodes: TCFGNodeList;

        CatchDepth: uint8;

       Private
        Procedure AddPrologCode;
        Procedure AddEpilogCode;

       Public
        Constructor Create(const fCompiler: TCompiler);

        Procedure CompileFunction(const Func: TFunction);
        Procedure CompileNode(const Node: TCFGNode);

       Public
        Property getVisitedNodes: TCFGNodeList read VisitedNodes;
       End;

 Implementation
Uses SysUtils, CommandLine, Expression, ExpressionCompiler, Messages, Tokens, Opcodes;

(* TBCGenerator.AddPrologCode *)
Procedure TBCGenerator.AddPrologCode;
Var Symbol  : TSymbol;
    StackReg: PStackSavedReg;
    StackDec: uint16 = 0;
Begin
 With Compiler do
 Begin
  fCurrentNode := CurrentFunc.FlowGraph.Root;

  { function info }
  PutComment('--------------------------------- //');
  PutComment('Function name   : '+CurrentFunc.RefSymbol.Name);
  PutComment('Declared in file: '+CurrentFunc.RefSymbol.DeclToken^.FileName);
  PutComment('Declared at line: '+IntToStr(CurrentFunc.RefSymbol.DeclToken^.Line));
  PutComment('--------------------');

  PutComment('Parameters:');
  For Symbol in CurrentFunc.SymbolList Do
   if (not Symbol.isInternal) and (Symbol.Typ = stVariable) and (Symbol.mVariable.isFuncParam) Then
    PutComment('`'+Symbol.Name+'` allocated at: '+Symbol.mVariable.getAllocationPos);

  PutComment('');

  PutComment('Variables:');
  For Symbol in CurrentFunc.SymbolList Do // @TODO: StackDec?
   if (not Symbol.isInternal) and (Symbol.Typ = stVariable) and (not Symbol.mVariable.isFuncParam) Then
    PutComment('`'+Symbol.Name+'` allocated in: '''+Symbol.mVariable.getAllocationPos+''', scope range: '+IntToStr(Symbol.mVariable.RefSymbol.Range.PBegin.Line)+'..'+IntToStr(Symbol.mVariable.RefSymbol.Range.PEnd.Line)+' lines, CFG cost: '+IntToStr(getVariableCFGCost(Symbol, CurrentFunc.FlowGraph.Root, nil)));

  PutComment('--------------------------------- //');

  With CurrentFunc do
  Begin
   StackDec := StackSize;

   { if register is taken by (a) variable(s), at first we need to save that register value (and restore it at the end of the function) }
   if (not isNaked) Then
   Begin
    For StackReg in StackRegs Do
    Begin
     PutOpcode(o_push, ['e'+StackReg^.RegChar+IntToStr(StackReg^.RegID)]);
     Inc(StackDec);
    End;
   End;

   { allocate local stack variables }
   if (not isNaked) Then
    PutOpcode(o_add, ['stp', StackSize]);

   { fix some variables' positions }
   if (not isNaked) Then
   Begin
    For Symbol in SymbolList Do
    Begin
     if (Symbol.Typ = stVariable) and (Symbol.mVariable.LocationData.Location = vlStack) and
        (Symbol.mVariable.isFuncParam) Then
     Begin
      Symbol.mVariable.LocationData.StackPosition -= StackDec;
     End;
    End;
   End;
  End;

  { new label (main function's body; nothing should jump here - it's just facilitation for the peephole optimizer so it doesn't remove the epilog code) }
  PutLabel(CurrentFunc.LabelName+'_body');
 End;
End;

(* TBCGenerator.AddEpilogCode *)
Procedure TBCGenerator.AddEpilogCode;
Var I: int32;
Begin
 With Compiler do
 Begin
  { function end code }
  PutLabel(CurrentFunc.LabelName+'_end');

  With CurrentFunc do
  Begin
   if (not isNaked) Then
    PutOpcode(o_sub, ['stp', CurrentFunc.StackSize]);

   For I := StackRegs.Count-1 Downto 0 Do
    PutOpcode(o_pop, ['e'+StackRegs[I]^.RegChar+IntToStr(StackRegs[I]^.RegID)]);
  End;

  CurrentFunc.LastOpcode := PutOpcode(o_ret);
 End;
End;

(* TBCGenerator.Create *)
Constructor TBCGenerator.Create(const fCompiler: TCompiler);
Begin
 Compiler := fCompiler;
End;

(* TBCGenerator.CompileFunction *)
Procedure TBCGenerator.CompileFunction(const Func: TFunction);
Begin
 CurrentFunc  := Func;
 VisitedNodes := TCFGNodeList.Create;

 Try
  AddPrologCode;
  CompileNode(Func.FlowGraph.Root);
  AddEpilogCode;
 Finally
  VisitedNodes.Free;
 End;
End;

(* TBCGenerator.CompileNode *)
Procedure TBCGenerator.CompileNode(const Node: TCFGNode);
Var ArrayVar        : TVariable; // used when compiling array initializer
    ArrayElementType: TType;
    ArrayValues     : Array of PExpressionNode;
    ArrayDimSizes   : Array of uint32;
    ArrayTmpDims    : Array of uint32;
    ArrayElementID  : uint32;

  { PutOpcode }
  Procedure PutOpcode(const Opcode: TOpcode_E);
  Begin
   Compiler.PutOpcode(Opcode);
  End;

  { PutOpcode }
  Procedure PutOpcode(const Opcode: TOpcode_E; const Args: Array of Const); // helper proc
  Begin
   Compiler.PutOpcode(Opcode, Args);
  End;

  { PutLabel }
  Function PutLabel(const LabelName: String): PMOpcode; // helper proc;
  Begin
   Result := Compiler.PutLabel(LabelName);
  End;

  { CompileError }
  Procedure CompileError(const Token: TToken_P; const Error: TCompileError; const Args: Array of Const); // helper proc
  Begin
   Compiler.CompileError(Token, Error, Args);
  End;

  { RemoveRedundantMovPush }
  Procedure RemoveRedundantMovPush(const Node: PExpressionNode);
  Var Opcode: PMOpcode;
  Begin
   With Compiler do
   Begin
    if (Node^.ResultOnStack) Then
    Begin
     if (OpcodeList.Last^.Opcode = o_push) Then
     Begin
      Opcode := OpcodeList.Last;

      OpcodeList.Remove(OpcodeList.Last);

      if (OpcodeList.Last^.Opcode = o_mov) and
         (OpcodeList.Last^.Args[0] = Opcode^.Args[0]) Then
      Begin
       OpcodeList.Remove(OpcodeList.Last);
      End;
     End;
    End;
   End;
  End;

  { CompileArrayInitializer }
  Procedure CompileArrayInitializer(const Dim: uint8);
  Var I, DimC : uint8;
      ExprType: TType;
  Begin
   With Compiler do
   Begin
    if (ArrayDimSizes[Dim] = 0) Then
    Begin
     ExprType := CompileExpression(Compiler, ArrayValues[ArrayElementID]); // compile expression

     if (not (type_equal(ExprType, ArrayElementType))) Then // check types
      CompileError(eWrongType, [ExprType.asString, ArrayElementType.asString]);

    // if (ArrayValues[ArrayElementID].ResultOnStack) Then
     PutOpcode(o_pop, ['e'+ExprType.RegPrefix+'1']);

     For I := Dim-1 Downto 0 Do
      PutOpcode(o_push, [ArrayTmpDims[I]]);

     DimC := ArrayVar.Typ.ArrayDimCount;

     if (ArrayVar.Typ.isString) Then
      Dec(DimC);

     PutOpcode(o_arset, [ArrayVar.getAllocationPos(-DimC), DimC, 'e'+ExprType.RegPrefix+'1']);

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

  { cetNone }
  Procedure CompileNone;
  Begin
   if (Node.Edges.Count > 0) Then
    CompileNode(Node.Edges.First);
  End;

  { cetExpression }
  Procedure CompileExpression;
  Begin
   if (Node.Value <> nil) Then
   Begin
    ExpressionCompiler.CompileExpression(Compiler, Node.Value);
    RemoveRedundantMovPush(Node.Value);
   End;

   if (Node.Edges.Count > 0) Then
    CompileNode(Node.Edges.First);
  End;

  { cetCondition }
  Procedure CompileCondition;
  Var LabelFalse, LabelOut: String;
      ExprType            : TType;
  Begin
   // generate label names
   LabelFalse := CurrentFunc.generateLabelName;
   LabelOut   := CurrentFunc.generateLabelName;

   // compile condition
   ExprType := ExpressionCompiler.CompileExpression(Compiler, Node.Value);
   if (not (ExprType.isBool or ExprType.isInt)) Then // condition must be either a bool or an int
    CompileError(Node.getToken^, eWrongType, [ExprType.asString, 'bool']);

   VisitedNodes.Add(Node.Edges[2]);

   PutOpcode(o_pop, ['if']);
   PutOpcode(o_fjmp, [':'+LabelFalse]);

   // compile 'true' node
   CompileNode(Node.Edges[0]);
   PutOpcode(o_jmp, [':'+LabelOut]);

   // compile 'false' node (i.e. `else`, if exists)
   PutLabel(LabelFalse);
   CompileNode(Node.Edges[1]);

   VisitedNodes.Remove(Node.Edges[2]);

   PutLabel(LabelOut);
   CompileNode(Node.Edges[2]);
  End;

  { cetReturn }
  Procedure CompileReturn;
  Var ExprType: TType;
  Begin
   if (Node.Value = nil) Then // return;
   Begin
    if (CatchDepth > 0) Then // there are some unused data on the stack that we need to remove before leaving function (that is - the exception object)
     PutOpcode(o_sub, ['stp', CatchDepth]);

    if (not CurrentFunc.Return.isVoid) Then // error: cannot do void-return inside non-void function
     CompileError(Node.getToken^, eReturnWithNoValue, []);

    if (not CurrentFunc.isNaked) Then
     PutOpcode(o_jmp, [':'+CurrentFunc.LabelName+'_end']) Else
     PutOpcode(o_ret);
   End Else
   Begin // return expression;
    ExprType := ExpressionCompiler.CompileExpression(Compiler, Node.Value);

    if (not ExprType.CanBeAssignedTo(CurrentFunc.Return)) Then // type check
     CompileError(Node.getToken^, eWrongType, [ExprType.asString, CurrentFunc.Return.asString]);

    if (Node.Value^.ResultOnStack) Then // function's result must be in the `e_1` register, not on the stack
     PutOpcode(o_pop, ['e'+CurrentFunc.Return.RegPrefix+'1']);

    if (CatchDepth > 0) Then
     PutOpcode(o_sub, ['stp', CatchDepth]);

    if (not CurrentFunc.isNaked) Then
     PutOpcode(o_jmp, [':'+CurrentFunc.LabelName+'_end']) Else
     PutOpcode(o_ret);
   End;

   // compile further code
   Compiler.DoNotStoreOpcodes := CmdLine.getBoolSwitch(opt__remove_dead); // code after 'return' is generated but deleted, as it would be never executed anyway

   if (Node.Edges.Count > 0) Then
    CompileNode(Node.Edges.First);
  End;

  { cetThrow }
  Procedure CompileThrow;
  Var ExprType: TType;
  Begin
   ExprType := ExpressionCompiler.CompileExpression(Compiler, Node.Value);

   if (not ExprType.isString) Then // error: invalid type
    CompileError(Node.getToken^, eWrongType, [ExprType.asString, 'string']);

   PutOpcode(o_pop, ['es1']); // @TODO: what's the point of these two opcodes? Casting or what?
   PutOpcode(o_push, ['es1']);
   PutOpcode(o_icall, ['"vm.throw"']);

   if (Node.Edges.Count > 0) Then
    CompileNode(Node.Edges.First);
  End;

  { cetTryCatch }
  Procedure CompileTryCatch;
  Var LabelName: String;
  Begin
   LabelName := Node.getName+'_trycatch';

   // save current exception handler and set the new one
   PutOpcode(o_icall, ['"vm.save_exception_state"']);
   PutOpcode(o_push, ['@'+LabelName]);
   PutOpcode(o_icall, ['"vm.set_exception_handler"']);

   // parse `try` block
   CompileNode(Node.Edges[0]);
   PutOpcode(o_jmp, [':'+LabelName+'_end']);

   // parse `catch` block
   PutLabel(LabelName)^.isPublic := True;
   PutOpcode(o_icall, ['"vm.restore_exception_state"']); // restore previous exception state
   PutOpcode(o_icall, ['"vm.get_last_exception"']); // get exception

   Inc(CatchDepth);
   CompileNode(Node.Edges[1]);
   PutOpcode(o_sub, ['stp', 1]); // remove the exception message variable holder
   Dec(CatchDepth);

   PutLabel(LabelName+'_end');
   CompileNode(Node.Edges[2]);
  End;

  { cetForeach }
  Procedure CompileForeach;
  Var LabelName: String;
      ExprType : TType;

      ForeachVar, ForeachIterator, ForeachExprHolder, ForeachSizeHolder: TVariable;
      Pos1, Pos2: String;
  Begin
   LabelName         := Node.getName+'_foreach_loop_';
   ForeachVar        := Node.Foreach.LoopVar as TVariable;
   ForeachIterator   := Node.Foreach.LoopIterVar as TVariable;
   ForeachExprHolder := Node.Foreach.LoopExprHolder as TVariable;
   ForeachSizeHolder := Node.Foreach.LoopSizeHolder as TVariable;

   // compile foreach expression
   ExprType := ExpressionCompiler.CompileExpression(Compiler, Node.Value);

   if (type_equal(ForeachVar.Typ, ExprType)) Then
   Begin
    CompileError(Node.getToken^, eInvalidForeach, []);
   End Else

   if (not ExprType.isArray) Then
   Begin
    CompileError(Node.getToken^, eWrongType, [ExprType.asString, 'array']); // error: an array is required for a foreach-expression
   End Else

   Begin
    if (not type_equal(ForeachVar.Typ, ExprType.getLowerArray)) Then
     CompileError(Node.getToken^, eWrongType, [ForeachVar.Typ.asString, ExprType.getLowerArray.asString]);
   End;

   // get and save foreach expression length
   if (Node.Value^.ResultOnStack) Then
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

   CompileNode(Node.Edges[0]);

   PutOpcode(o_add, [ForeachIterator.getAllocationPos, 1]); // ForeachIterator++
   PutOpcode(o_jmp, [':'+LabelName+'content']); // jump to the beginning of the loop

   PutLabel(LabelName+'end');
   CompileNode(Node.Edges[1]);
  End;

  { cetArrayInitializer }
  Procedure CompileArrayInitializer;
  Var I: uint8;
  Begin
   ArrayVar         := (Node.ArrayInitializer.VarSymbol as TSymbol).mVariable;
   ArrayElementType := ArrayVar.Typ.ArrayBase;
   ArrayValues      := Node.ArrayInitializer.Values;
   ArrayDimSizes    := Node.ArrayInitializer.DimSizes;

   SetLength(ArrayTmpDims, 256);

   // allocate array (in bytecode)
   For I := High(ArrayDimSizes) Downto Low(ArrayDimSizes) Do
   Begin
    if (ArrayDimSizes[I] = 0) Then
     Continue;

    PutOpcode(o_push, [ArrayDimSizes[I]]);
   End;

   I := ArrayVar.Typ.ArrayDimCount;

   if (ArrayVar.Typ.isString) Then
    Dec(I);

   PutOpcode(o_arcrt, ['er1', ArrayVar.Typ.InternalID, I]);
   PutOpcode(o_mov, [ArrayVar.getAllocationPos, 'er1']);

   // compile initializer
   ArrayElementID := 0;
   CompileArrayInitializer(0);

   // compile further nodes
   if (Node.Edges.Count > 0) Then
    CompileNode(Node.Edges[0]);
  End;

  { cetBytecode }
  Procedure CompileBytecode;
  Var ArgList: PVarRecArray;
      Symbol : TSymbol;
      I      : int8;
  Begin
   if (Node.Bytecode.OpcodeName = '') Then
   Begin
    PutLabel(Node.Bytecode.LabelName);
   End Else
   Begin
    ArgList := Node.Bytecode.OpcodeArgList;

    // replace "localvar." with actual variable allocation position
    For I := Low(ArgList^) To High(ArgList^) Do
    Begin
     if (Pos('localvar.', ArgList^[I].VPChar) > 0) Then
     Begin
      For Symbol in CurrentFunc.SymbolList Do
      Begin
       With Symbol do
        if (Typ = stVariable) Then
         ArgList^[I].VPChar := CopyStringToPChar(StringReplace(ArgList^[I].VPChar, 'localvar.'+IntToStr(uint32(mVariable)), mVariable.getAllocationPos, [rfReplaceAll]));
      End;
     End;
    End;

    Compiler.PutOpcode(Node.Bytecode.OpcodeName, ArgList^, Node.getToken);
   End;

   if (Node.Edges.Count > 0) Then
    CompileNode(Node.Edges.First);
  End;

Var PrevDNSO: Boolean; // "previous DoNotStoreOpcodes"
Begin
 if (Node = nil) Then
  Exit;

 if (VisitedNodes.IndexOf(Node) <> -1) Then // if this node has been visited more than once, don't compile it again - place a jump instead; @TODO: it generates many unused jmp's - what about them?
 Begin
  Compiler.PutOpcode(o_jmp, [':'+Node.getName]);
  Exit;
 End;

 // add node to the visited list
 VisitedNodes.Add(Node);

 // change current node
 Compiler.fCurrentNode := Node;

 // put label if it's necessary; @TODO
// if (Node.Typ <> cetBytecode) and (AnythingFromNodePointsAt(CurrentFunc.FlowGraph.Root, nil, Node) Then
  Compiler.PutLabel(Node.getName);

 // save state
 PrevDNSO := Compiler.DoNotStoreOpcodes;

 // generate code
 Try
  Case Node.Typ of
   cetNone            : CompileNone;
   cetExpression      : CompileExpression;
   cetCondition       : CompileCondition;
   cetReturn          : CompileReturn;
   cetThrow           : CompileThrow;
   cetTryCatch        : CompileTryCatch;
   cetForeach         : CompileForeach;
   cetArrayInitializer: CompileArrayInitializer;
   cetBytecode        : CompileBytecode;

   else
    Compiler.CompileError(eInternalError, [Format('TBCCompiler.CompileNode() -> invalid node type: %d', [ord(Node.Typ)])]);
  End;
 Finally
  Compiler.DoNotStoreOpcodes := PrevDNSO;
 End;
End;

End.
