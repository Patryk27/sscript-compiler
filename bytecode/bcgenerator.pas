(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
Unit BCGenerator;

 Interface
 Uses HLCompiler, symdef, FlowGraph, Opcodes, SysUtils;

 { EBCGeneratorException }
 Type EBCGeneratorException = Class(Exception);

 { TBCGenerator }
 Type TBCGenerator =
      Class
       Private
        Compiler: TCompiler;

        CurrentFunc : TFunction;
        VisitedNodes: TCFGNodeList;

        RegisterPushBegin: PMOpcode;

        CatchDepth: uint8;

       Private
        Procedure AddPrologCode;
        Procedure AddEpilogCode;

       Public
        Constructor Create(const fCompiler: TCompiler);
        Destructor Destroy; override;

        Procedure CompileFunction(const Func: TFunction);
        Procedure CompileNode(const Node: TCFGNode);

       Public
        Property getVisitedNodes: TCFGNodeList read VisitedNodes;
       End;

 Implementation
Uses CommandLine, Expression, Messages, Tokens;

(* TBCGenerator.AddPrologCode *)
Procedure TBCGenerator.AddPrologCode;
Var Symbol: TSymbol;
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
  Begin
   if (not Symbol.isInternal) and (Symbol.Typ = stVariable) and (Symbol.mVariable.isFuncParam) Then
    PutComment('`'+Symbol.Name+'` allocated at: '+Symbol.mVariable.getAllocationPos);
  End;

  PutComment('');

  PutComment('Variables:');
  For Symbol in CurrentFunc.SymbolList Do
  Begin
   if (not Symbol.isInternal) and (Symbol.Typ = stVariable) and (not Symbol.mVariable.isFuncParam) Then
    PutComment('`'+Symbol.Name+'` allocated in: '''+Symbol.mVariable.getAllocationPos+''', scope range: '+IntToStr(Symbol.mVariable.RefSymbol.Range.PBegin.Line)+'..'+IntToStr(Symbol.mVariable.RefSymbol.Range.PEnd.Line)+' lines, CFG cost: '+IntToStr(getVariableCFGCost(Symbol, CurrentFunc.FlowGraph.Root, nil)));
  End;

  PutComment('--------------------------------- //');

  With CurrentFunc do
  Begin
   { if register is invalidated inside this function (eg. 'ei3' or 'es1'), at first we need to save that register value (and restore it at the end of the function) }
   if (isNaked) Then
    RegisterPushBegin := nil Else
    RegisterPushBegin := PutOpcode(o_nop); // at this time we don't know which registers should be saved

   { allocate local stack variables }
   if (not isNaked) Then
    PutOpcode(o_add, ['stp', StackSize], RefSymbol.DeclToken);
  End;

  { new label (main function's body; nothing should jump here - it's just facilitation for the peephole optimizer so it doesn't remove the epilog code) }
  PutLabel(CurrentFunc.LabelName+'_body');
 End;
End;

(* TBCGenerator.AddEpilogCode *)
Procedure TBCGenerator.AddEpilogCode;
Var StackDec: int32;
    I       : int32;

  { ReplaceTemporaryVariableRefs }
  Procedure ReplaceTemporaryVariableRefs;
  Var OpcodeID, ArgID: int32;
      VarLocation    : TVariableLocationData;
      Variable       : TVariable;
      Opcode         : PMOpcode;
      Arg            : PMOpcodeArg;

      VarLoc  : PtrUInt;
      StackFix: int32;
      TmpStr  : String;
  Begin
   With Compiler, CurrentFunc do
   Begin
    // each opcode
    For OpcodeID := OpcodeList.IndexOf(CurrentFunc.FirstOpcode) To OpcodeList.Count-1 Do
    Begin
     Opcode := OpcodeList[OpcodeID];

     // each argument
     For ArgID := Low(Opcode^.Args) To High(Opcode^.Args) Do
     Begin
      Arg := @Opcode^.Args[ArgID];

      // if it's a variable reference, replace it
      if (Arg^.Typ = ptVariableRef) Then
      Begin
       TmpStr := Arg^.Value;

       VarLoc   := StrToInt(Copy(TmpStr, 1, Pos(':', TmpStr)-1));
       StackFix := StrToInt(Copy(TmpStr, Pos(':', TmpStr)+1, Length(TmpStr)));

       Variable    := TVariable(Pointer(VarLoc));
       VarLocation := Variable.LocationData;

       Case VarLocation.Location of
        // register
        vlRegister:
        Begin
         Case Variable.Typ.RegPrefix of
          'b': Arg^.Typ := ptBoolReg;
          'c': Arg^.Typ := ptCharReg;
          'i': Arg^.Typ := ptIntReg;
          'f': Arg^.Typ := ptFloatReg;
          's': Arg^.Typ := ptStringReg;
          'r': Arg^.Typ := ptReferenceReg;

          else
           raise EBCGeneratorException.Create('Unknown variable type register prefix.');
         End;

         Arg^.Value := VarLocation.RegisterID;
        End;

        // stack
        vlStack:
        Begin
         Arg^.Typ := ptStackval;

         if (Variable.isFuncParam) Then
          Arg^.Value := VarLocation.StackPosition - StackDec - StackFix Else
          Arg^.Value := VarLocation.StackPosition - StackFix;
        End;

        // memory
        vlMemory:
        Begin
         Arg^.Typ   := ptSymbolMemRef;
         Arg^.Value := VarLocation.MemSymbolName;
        End;

        // unknown
        else
         raise EBCGeneratorException.Create('Unknown variable location kind.');
       End;
      End;
     End;
    End;
   End;
  End;

Begin
 With Compiler, CurrentFunc do
 Begin
  StackDec := StackSize;

  { correct function beginning code }
  if (not isNaked) Then
  Begin
   if (RegisterPushBegin <> nil) Then
   Begin
    DoNotStoreOpcodes := True;

    For I := 0 To StackRegs.Count-1 Do
    Begin
     OpcodeList.Insert(OpcodeList.IndexOf(RegisterPushBegin), PutOpcode(o_push, [StackRegs[I]^.getRegisterName], RefSymbol.DeclToken));
     Inc(StackDec);
    End;

    DoNotStoreOpcodes := False;
   End;
  End;

  { replace temporary variable references (~foo:bar) with their real bytecode locations }
  ReplaceTemporaryVariableRefs;

  { add ending code }
  PutLabel(CurrentFunc.LabelName+'_end');

  if (not isNaked) Then
  Begin
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
 Compiler     := fCompiler;
 VisitedNodes := TCFGNodeList.Create;
End;

(* TBCGenerator.Destroy *)
Destructor TBCGenerator.Destroy;
Begin
 VisitedNodes.Free;

 inherited Destroy;
End;

(* TBCGenerator.CompileFunction *)
Procedure TBCGenerator.CompileFunction(const Func: TFunction);
Begin
 VisitedNodes.Clear;
 CurrentFunc := Func;

 AddPrologCode;
 CompileNode(Func.FlowGraph.Root);
 AddEpilogCode;
End;

(* TBCGenerator.CompileNode *)
Procedure TBCGenerator.CompileNode(const Node: TCFGNode);
Type TIntegerArray = Array of Integer;

  { PutOpcode }
  Procedure PutOpcode(const Opcode: TOpcodeKind);
  Begin
   Compiler.PutOpcode(Opcode);
  End;

  { PutOpcode }
  Procedure PutOpcode(const Opcode: TOpcodeKind; const Args: Array of Const); // helper proc
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

  { CompileArrayInitializer }
  Procedure CompileArrayInitializer(Indexes: TIntegerArray; const AInit: TArrayInitializerValues);
  Var Expression: TExpressionCompileResult;
      ExprNode  : TExpressionNode;
      ExprType  : TType;

      ArrayVar: TVariable;

    { PutArrayIndexes }
    Procedure PutArrayIndexes(const Last: int32=-1);
    Var I: int32;
    Begin
     if (Last > -1) Then
      Compiler.PutOpcode(o_push, [Last]);

     For I := High(Indexes) Downto Low(Indexes) Do
      Compiler.PutOpcode(o_push, [Indexes[I]]);
    End;

    { AssignArrayPointer }
    Procedure AssignArrayPointer;
    Begin
     if (Length(Indexes) = 0) Then
     Begin
      Compiler.PutOpcode(o_mov, [ArrayVar.getAllocationPos, 'er1']);
     End Else
     Begin
      PutArrayIndexes;
      Compiler.PutOpcode(o_arset, [ArrayVar.getAllocationPos, Length(Indexes), 'er1']);
     End;
    End;

  Var I: int32;
  Begin
   ArrayVar := TSymbol(Node.ArrayInitializer.VarSymbol).mVariable;

   With Compiler do
   Begin
    // expression
    if (AInit.Typ = aivtExpression) Then
    Begin
     // create array
     PutOpcode(o_arcrt1, ['er1', ArrayVar.Typ.ArrayPrimitive.InternalID, Length(AInit.ExprValues)]);

     // invalidate register
     CurrentFunction.invalidateRegister('r', 1);

     // assign pointer
     AssignArrayPointer;

     // parse expressions
     For I := 0 To High(AInit.ExprValues) Do
     Begin
      // @TODO: we assume here that 'er1' doesn't get accidentally modified. Is it true in all cases?

      // compile expression
      ExprNode := AInit.ExprValues[I];

      Expression := ExprNode.CompileExpression;
      ExprType   := TType(Expression.Typ);

      // do type check
      if (not ExprType.CanBeAssignedTo(ArrayVar.Typ.ArrayPrimitive)) Then
       CompileError(ExprNode.getToken, eWrongType, [ExprType.asString, ArrayVar.Typ.ArrayPrimitive.asString]);

      // assign value
      PutOpcode(o_arset1, ['er1', I, Expression.getResult]);
     End;
    End Else

    // array
    Begin
     // create array
     PutOpcode(o_arcrt1, ['er1', $FF, Length(AInit.ArrayValues)]);

     // assign it
     AssignArrayPointer;

     // expand indexes array
     SetLength(Indexes, Length(Indexes)+1);

     // assign values
     For I := 0 To High(AInit.ArrayValues) Do
     Begin
      Indexes[High(Indexes)] := I;
      CompileArrayInitializer(Indexes, AInit.ArrayValues[I]^);
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
    Node.Value.CompileExpression();
   End;

   if (Node.Edges.Count > 0) Then
    CompileNode(Node.Edges.First);
  End;

  { cetCondition }
  Procedure CompileCondition;
  Var LabelFalse, LabelOut: String;
      Condition           : TExpressionCompileResult;
      CondType            : TType;
  Begin
   // generate label names
   LabelFalse := CurrentFunc.generateLabelName;
   LabelOut   := CurrentFunc.generateLabelName;

   // compile condition
   Condition := Node.Value.CompileExpression();
   CondType  := TType(Condition.Typ);

   PutOpcode(o_mov, ['if', Condition.getResult]);

   // do type-check
   if (not (CondType.isBool or CondType.isInt)) Then
    CompileError(Node.getToken^, eWrongType, [CondType.asString, 'bool']);

   // mark node as 'visited'
   VisitedNodes.Add(Node.Edges[2]);

   // put a skip to 'else' node if condition is false
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
  Var ValueType: TType;
      Value    : TExpressionCompileResult;
  Begin
   { return; }
   if (Node.Value = nil) Then
   Begin
    // there are some unused data on the stack that we need to remove before leaving function (that is - the exception object)
    if (CatchDepth > 0) Then
     PutOpcode(o_sub, ['stp', CatchDepth]);

    // error: cannot do void-return inside a non-void function
    if (not CurrentFunc.Return.isVoid) Then
     CompileError(Node.getToken^, eReturnWithNoValue, []);

    // generate jmp/ret
    if (CurrentFunc.isNaked) Then
     PutOpcode(o_ret) Else
     PutOpcode(o_jmp, [':'+CurrentFunc.LabelName+'_end']);
   End Else

   { return expression; }
   Begin
    // compile expression
    Value     := Node.Value.CompileExpression();
    ValueType := TType(Value.Typ);

    PutOpcode(o_mov, ['e'+CurrentFunc.Return.RegPrefix+'0', Value.getResult]);

    // do a type-check
    if (not ValueType.CanBeAssignedTo(CurrentFunc.Return)) Then
     CompileError(Node.getToken^, eWrongType, [ValueType.asString, CurrentFunc.Return.asString]);

    // decrease stack pointer, if necessary
    if (CatchDepth > 0) Then
     PutOpcode(o_sub, ['stp', CatchDepth]);

    // generate jmp/ret
    if (CurrentFunc.isNaked) Then
     PutOpcode(o_ret) Else
     PutOpcode(o_jmp, [':'+CurrentFunc.LabelName+'_end']);
   End;

   // compile further code
   Compiler.DoNotStoreOpcodes := CmdLine.getBoolSwitch(opt__remove_dead); // code after 'return' is generated but deleted, as it would be never executed anyway

   if (Node.Edges.Count > 0) Then
    CompileNode(Node.Edges.First);
  End;

  { cetThrow }
  Procedure CompileThrow;
  Var Expression: TExpressionCompileResult;
      ExprType  : TType;
  Begin
   // compile expression
   Expression := Node.Value.CompileExpression;
   ExprType   := TType(Expression.Typ);

   // do type check
   if (not ExprType.isString) Then
    CompileError(Node.getToken^, eWrongType, [ExprType.asString, 'string']);

   // put opcodes
   PutOpcode(o_push, [Expression.getResult]);
   PutOpcode(o_icall, ['"vm.throw"']);

   // proceed further
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

      Expression: TExpressionCompileResult;
      ExprType  : TType;

      ForeachVar, ForeachIterator, ForeachExprHolder, ForeachSizeHolder: TVariable;

      Pos1, Pos2: String;

      Opcode: TOpcodeKind;
  Begin
   LabelName         := Node.getName+'_foreach_loop_';
   ForeachVar        := Node.Foreach.LoopVar as TVariable;
   ForeachIterator   := Node.Foreach.LoopIterVar as TVariable;
   ForeachExprHolder := Node.Foreach.LoopExprHolder as TVariable;
   ForeachSizeHolder := Node.Foreach.LoopSizeHolder as TVariable;

   // compile foreach expression
   Expression := Node.Value.CompileExpression;
   ExprType   := TType(Expression.Typ);

   // do type-check
   if (type_equal(ForeachVar.Typ, ExprType)) Then
   Begin
    // error: type inconsistency
    CompileError(Node.getToken^, eInvalidForeach, []);
   End Else

   if (not ExprType.isArray) Then
   Begin
    // error: an array is required for a foreach-expression
    CompileError(Node.getToken^, eWrongType, [ExprType.asString, 'array']);
   End Else

   Begin
    if (not type_equal(ForeachVar.Typ, ExprType.getLowerArray)) Then
     CompileError(Node.getToken^, eWrongType, [ForeachVar.Typ.asString, ExprType.getLowerArray.asString]);
   End;

   // save foreach expression
   PutOpcode(o_mov, [ForeachExprHolder.getAllocationPos, Expression.getResult]);

   // get foreach expression length
   if (ExprType.RegPrefix = 's') Then
   Begin
    PutOpcode(o_strlen, [ForeachExprHolder.getAllocationPos, ForeachSizeHolder.getAllocationPos]);
    PutOpcode(o_mov, [ForeachIterator.getAllocationPos, 1]); // strings are iterated from 1
   End Else
   Begin
    PutOpcode(o_arlen, [ForeachExprHolder.getAllocationPos, ForeachSizeHolder.getAllocationPos]);
    PutOpcode(o_mov, [ForeachIterator.getAllocationPos, 0]);
   End;

   // put rest of the code
   PutLabel(LabelName+'content');

   if (ExprType.RegPrefix = 's') Then
    Opcode := o_if_le Else
    Opcode := o_if_l;

   PutOpcode(Opcode, [ForeachIterator.getAllocationPos, ForeachSizeHolder.getAllocationPos]);
   PutOpcode(o_fjmp, [':'+LabelName+'end']);

   Pos1 := ForeachExprHolder.getAllocationPos;
   Pos2 := ForeachVar.getAllocationPos;

   if (type_equal(ExprType, TYPE_STRING)) Then
    PutOpcode(o_strget, [Pos1, ForeachIterator.getAllocationPos, Pos2]) Else // strget(ForeachExprHolder, ForeachIterator, ForeachVar)
    PutOpcode(o_arget1, [Pos1, ForeachIterator.getAllocationPos, Pos2]); // arget(ForeachExprHolder, ForeachIterator, ForeachVar)

   CompileNode(Node.Edges[0]);

   PutOpcode(o_add, [ForeachIterator.getAllocationPos, 1]); // ForeachIterator++
   PutOpcode(o_jmp, [':'+LabelName+'content']); // jump to the beginning of the loop

   PutLabel(LabelName+'end');
   CompileNode(Node.Edges[1]);
  End;

  { cetArrayInitializer }
  Procedure CompileArrayInitializer;
  Var Indexes: TIntegerArray;
  Begin
   SetLength(Indexes, 0);
   CompileArrayInitializer(Indexes, Node.ArrayInitializer.Values^);

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
    raise EBCGeneratorException.CreateFmt('Invalid node type: %d', [ord(Node.Typ)]);
  End;
 Finally
  Compiler.DoNotStoreOpcodes := PrevDNSO;
 End;
End;

End.
