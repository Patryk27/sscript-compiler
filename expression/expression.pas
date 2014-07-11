(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
Unit Expression;

 Interface
 Uses Serialization, Tokens, Opcodes, List, MixedValue,
      SysUtils, Variants;

 { EExpressionNodeException }
 Type EExpressionNodeException = Class(Exception);

 // -------------- //
 { TExpressionCompileResultKind }
 Type TExpressionCompileResultKind = (ecRegister, ecStack, ecMemory, ecVariant, ecVariable);

 { TExpressionCompileResultAttributes }
 Type TExpressionCompileResultAttributes = Set of (ecrImmediate, ecrHoldsVariable);

 { TExpressionCompileResult }
 Type TExpressionCompileResult =
      Record
       Public
        Kind: TExpressionCompileResultKind;
        Typ : TObject; // TType class instance

        RegisterChar: Char; RegisterID: uint8; // ecRegister
        StackPosition                 : int8; // ecStack
        MemorySymbol                  : String; // ecMemory
        Value                         : Variant; // ecVariant
        Variable                      : String; // ecVariable

        Attributes: TExpressionCompileResultAttributes;

       Public
        Function isImmediate: Boolean;
        Function isVariableHolder: Boolean;

        Function getResult: String; // returns register name (ei3, eb2...), stack position ([1], [-3]...), memory reference (&foo) or value ("foo", 50, #123...)
       End;

 // -------------- //
 { TRegisterEntryValueKind }
 Type TRegisterEntryValueKind = (rkUnknown, rkVariable);

 { TRegisterEntryValue }
 Type TRegisterEntryValue =
      Record
       Case Kind: TRegisterEntryValueKind of
        rkVariable: (Variable: TObject);
       End;

 { TRegisterEntry }
 Type PRegisterEntry = ^TRegisterEntry;
      TRegisterEntry =
      Record
       // register char (b, c, i, f, s)
       RegisterChar: Char;

       // register ID (0..6)
       RegisterID: uint8;

       // register value (used in fast in-place internal optimizations when eg.variable from stack is loaded into this register and some operation is done on it, eg: "a*a")
       Value: TRegisterEntryValue;

       // is this register free?
       isFree: Boolean;
      End;

 { TRegisterList }
 Type TRegisterList = Array of TRegisterEntry;

 // -------------- //
 Type TExpressionNode = Class;

 // -------------- //
 { TExpressionCompiler }
 Type TExpressionCompiler =
      Class sealed
       Strict private
        Compiler      : TObject;
        UsedStackSlots: int32;

        RegisterList: TRegisterList;

        CompileDepth: uint32;

       Strict private
        Function FetchRegister(const RegisterChar: Char; const RegisterID: uint8): PRegisterEntry;

       Private
        Procedure Reset;

        // TExpressionCompileResult general management
        Function AllocateCompileResult(const fType: TObject; const SkipIFreg: Boolean=False): TExpressionCompileResult;

        Procedure ReleaseRegister(const Rec: TExpressionCompileResult);
        Procedure InvalidateRegister(const Rec: TExpressionCompileResult);

        // register management
        Function FindFreeRegister(const RegisterChar: Char; out RegisterID: uint8; const SkipIFreg: Boolean=False): Boolean;

        Procedure OccupyRegister(const RegisterChar: Char; const RegisterID: uint8);
        Procedure ReleaseRegister(const RegisterChar: Char; const RegisterID: uint8);
        Procedure InvalidateRegister(const RegisterChar: Char; const RegisterID: uint8);

        Function FindRegisterValidatedToVariable(const Variable: TObject; out CompileResult: TExpressionCompileResult): Boolean;
        Procedure ValidateRegisterToVariable(const RegisterChar: Char; const RegisterID: uint8; const Variable: TObject);

        Function isRegisterOccupied(const RegisterChar: Char; const RegisterID: uint8): Boolean;

        // stack management
        Procedure IncUsedStackSlots(const HowMany: int32=1);
        Procedure DecUsedStackSlots(const HowMany: int32=1);

        // "Compile()" function depth
        Procedure IncDepth;
        Procedure DecDepth;

       Private
        Property getCompiler: TObject read Compiler;
        Property getUsedStackSlots: int32 read UsedStackSlots;
        Property getDepth: uint32 read CompileDepth;

       Public
        Constructor Create(const fCompiler: TObject);
       End;

 // -------------- //
 { TExpressionNode } // generic expression node class
 Type TExpressionNode =
      Class
       Private
        Token: TToken_P;

        ExprCompiler: TExpressionCompiler;

       Protected
        // code generation helper routines
        Procedure PutOpcode(const Op: TOpcodeKind; const Args: Array of Const);

        Function SaveResult(const Value: TExpressionCompileResult; const CanBeImmediate: Boolean): TExpressionCompileResult;
        Function MoveResultToRegister(const ResultFrom: TExpressionCompileResult; const RegisterChar: Char; const RegisterID: uint8): TExpressionCompileResult;
        Function MoveResultAtStack(const ResultFrom: TExpressionCompileResult): TExpressionCompileResult;
        Procedure PopResult(var Result: TExpressionCompileResult);

        Function Compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult;

        Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; virtual; abstract;

       Public
        Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P);

        Function CompileExpression: TExpressionCompileResult;

        Function Optimize: TExpressionNode; virtual;
        Function Evaluate: TExpressionNode; virtual;
        Function Clone: TExpressionNode; virtual;

        Function findIdentifier(const Symbol: TObject): Boolean; virtual;

        Function getCost: uint32; virtual;
        Function getPredictedType: TMixedValueKind; virtual;
        Function isLValue: Boolean; virtual;

       Public
        Property getToken: TToken_P read Token;
        Property getExpressionCompiler: TExpressionCompiler read ExprCompiler;
       End;

 { TNullaryExpressionNode }
 Type TNullaryExpressionNode =
      Class (TExpressionNode)
       Private
       Public
       End;

 { TUnaryExpressionNode }
 Type TUnaryExpressionNode =
      Class (TExpressionNode)
       Private
        Child: TExpressionNode;

       Public
        Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode);

        Function findIdentifier(const Symbol: TObject): Boolean; override;

        Function getCost: uint32; override;
        Function getPredictedType: TMixedValueKind; override;

        Procedure setChild(const Node: TExpressionNode);

       Public
        Property getChild: TExpressionNode read Child;
       End;

 { TBinaryExpressionNode }
 Type TBinaryExpressionNode =
      Class (TExpressionNode)
       Private
        Left, Right: TExpressionNode;

       Protected
        Procedure CompileChildNodes(out LArg, RArg: TExpressionCompileResult; const ReleaseRegs: Boolean; const CanBothBeImmediate: Boolean=False);

       Public
        Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode);

        Function Evaluate: TExpressionNode; override;

        Function findIdentifier(const Symbol: TObject): Boolean; override;

        Function getCost: uint32; override;
        Function getPredictedType: TMixedValueKind; override;

        Procedure setLeft(const Node: TExpressionNode);
        Procedure setRight(const Node: TExpressionNode);

        Procedure SwapNodes;

       Public
        Property getLeft: TExpressionNode read Left;
        Property getRight: TExpressionNode read Right;
       End;

 { TCallArgumentList }
 Type TCallArgumentList = specialize TList<TExpressionNode>;

 {$I nullary_nodes.inc}
 {$I unary_nodes.inc}
 {$I binary_nodes.inc}

 Implementation
Uses HLCompiler, Messages, symdef;

// include class helpers
{$I helpers.inc}

// -------------------------------------------------------------------------- //
(* CreateCompileResult *)
Function CreateCompileResult(const Value: String; const Typ: TObject; const Attributes: TExpressionCompileResultAttributes): TExpressionCompileResult;
Begin
 // check if we have a valid class instance
 if (Typ <> nil) and (not (Typ is TType)) Then
  raise EExpressionNodeException.CreateFmt('Expected a TType class instance! (got: %s)', [Typ.ClassName]);

 // fill some fields
 Result.Typ        := Typ;
 Result.Attributes := Attributes;

 // general-purpose register
 if (Copy(Value, 1, 1) = 'e') Then
 Begin
  Result.Kind         := ecRegister;
  Result.RegisterChar := Value[2];
  Result.RegisterID   := StrToInt(Value[3]);
 End Else

 // 'if' register
 if (Value = 'if') Then
 Begin
  Result.Kind         := ecRegister;
  Result.RegisterChar := 'b';
  Result.RegisterID   := 6;
 End Else

 // stack
 if (Copy(Value, 1, 1) = '[') Then
 Begin
  Result.Kind          := ecStack;
  Result.StackPosition := StrToInt(Copy(Value, 2, Pos(']', Value)-2));
 End Else

 // variable
 if (Copy(Value, 1, 1) = '~') Then
 Begin
  Result.Kind     := ecVariable;
  Result.Variable := Copy(Value, 2, Length(Value));
 End Else

 // invalid
 Begin
  raise EExpressionNodeException.Create('Internal error');
 End
End;

(* NullResult *)
Function NullResult: TExpressionCompileResult;
Begin
 Result := CreateCompileResult('[0]', nil, []);
End;

(* CleanResult *)
Procedure CleanResult(out Result: TExpressionCompileResult);
Begin
 Result := NullResult;
End;

// -------------------------------------------------------------------------- //
(* TExpressionCompileResult.isImmediate *)
Function TExpressionCompileResult.isImmediate: Boolean;
Begin
 Result := (ecrImmediate in Attributes);
End;

(* TExpressionCompileResult.isVariableHolder *)
Function TExpressionCompileResult.isVariableHolder: Boolean;
Begin
 Result := (ecrHoldsVariable in Attributes);
End;

(* TExpressionCompileResult.getResult *)
Function TExpressionCompileResult.getResult: String;
Var T: TType;
Begin
 Case Kind of
  // register
  ecRegister:
  Begin
   Result := Format('e%s%d', [String(RegisterChar), RegisterID]);

   // special register: if
   if (RegisterChar = 'b') and (RegisterID = 6) Then
    Result := 'if';
  End;

  // stack
  ecStack:
  Begin
   Result := Format('[%d]', [StackPosition]);
  End;

  // memory
  ecMemory:
  Begin
   Result := Format('&%s', [MemorySymbol]);
  End;

  // variant
  ecVariant:
  Begin
   if ((VarType(Value) and VarTypeMask) = varString) Then
   Begin
    Result := '"'+Value+'"';
   End Else
   Begin
    Result := VarToStr(Value);

    T := TType(Typ);

    if (T.isChar) and (T.ArrayDimCount = 0) Then
     Result := '#'+Result;
   End;
  End;

  // variable
  ecVariable:
  Begin
   Result := '~'+Variable;
  End;

  // invalid
  else
   raise EExpressionNodeException.Create('Invalid enum kind');
 End;
End;

// -------------------------------------------------------------------------- //
(* TExpressionCompiler.ReleaseRegister *)
{
 Relases register of given TExpressionCompileResult.
}
Procedure TExpressionCompiler.ReleaseRegister(const Rec: TExpressionCompileResult);
Begin
 // don't free special immediates
 if ((ecrImmediate in Rec.Attributes) and (Rec.RegisterID in [3..5])) Then
  Exit;

 if (Rec.Kind = ecRegister) Then
  ReleaseRegister(Rec.RegisterChar, Rec.RegisterID);
End;

(* TExpressionCompiler.InvalidateRegister *)
{
 Invalidates value of given TExpressionCompileResult register.
}
Procedure TExpressionCompiler.InvalidateRegister(const Rec: TExpressionCompileResult);
Begin
 if (Rec.Kind = ecRegister) Then
  InvalidateRegister(Rec.RegisterChar, Rec.RegisterID);
End;

(* TExpressionCompiler.FetchRegister *)
{
 Returns pointer to given register.
 Throws an exception if passed values don't match with any register on the list.
}
Function TExpressionCompiler.FetchRegister(const RegisterChar: Char; const RegisterID: uint8): PRegisterEntry;
Var I: uint8;
Begin
 For I := Low(RegisterList) To High(RegisterList) Do
 Begin
  if (RegisterList[I].RegisterChar = RegisterChar) and (RegisterList[I].RegisterID = RegisterID) Then
   Exit(@RegisterList[I]);
 End;

 raise EExpressionNodeException.CreateFmt('Invalid register: e%s%d', [String(RegisterChar), RegisterID]);
End;

(* TExpressionCompiler.Reset *)
{
 Resets expression compiler's state.
}
Procedure TExpressionCompiler.Reset;
Const Regs: Array[0..5] of Char = ('b', 'c', 'i', 'f', 's', 'r');

  { SetRegister }
  Function SetRegister(const RegisterChar: Char; const RegisterID: uint8; const isFree: Boolean): TRegisterEntry;
  Begin
   Result.RegisterChar := RegisterChar;
   Result.RegisterID   := RegisterID;
   Result.Value.Kind   := rkUnknown;
   Result.isFree       := isFree;
  End;

Var // ContainsCall: Boolean; @TODO
    I, J, RL    : uint32;
Begin
 SetLength(RegisterList, 6*Length(Regs) + 1);

 RL := 0;

 For I := Low(Regs) To High(Regs) Do
 Begin
  For J := 0 To 5 Do
  Begin
   // @TODO: when node does not contain any "call()", the e#0 registers can be used
   // @TODO: they also should be selected at first when searching for a free register

   RegisterList[RL] := SetRegister(Regs[I], J, J in [1..2]); // assume that e#3, e#4 and e#5 registers are reserved
   Inc(RL);
  End;
 End;

 RegisterList[RL] := SetRegister('b', 6, True); // the 'if' register
 {
  @Note:
   The "if" register is marked as 'free' for efficiency reasons of the generated bytecode.
   Mainly when a branch condition expression is compiled, no additional "mov(if, eb1/eb2)" has to be done.
 }

 UsedStackSlots := 0;
 CompileDepth   := 0;
End;

(* TExpressionCompile.AllocateCompilerResult *)
Function TExpressionCompiler.AllocateCompileResult(const fType: TObject; const SkipIFreg: Boolean): TExpressionCompileResult;
Var RegChar: Char;
    RegID  : uint8;
    Typ    : TType;
Begin
 // cast to get a TType class instance
 Typ := TType(fType);

 // check for a free register at first
 if (Typ = nil) Then // @TODO: some internal warning message? (this can happen in eg "one_dimensional_int_array[1][2]")
  Exit(NullResult);

 RegChar := Typ.RegPrefix;

 // try to find a free register
 if (FindFreeRegister(RegChar, RegID, SkipIFreg)) Then
 Begin
  Result              := NullResult;
  Result.Kind         := ecRegister;
  Result.Typ          := fType;
  Result.RegisterChar := RegChar;
  Result.RegisterID   := RegID;

  OccupyRegister(RegChar, RegID);
 End Else

 // allocate on stack otherwise
 Begin
  Result := CreateCompileResult('[0]', fType, []);

  IncUsedStackSlots;
  TCompiler(Compiler).PutOpcode(o_push, [0]);
 End;
End;

(* TExpressionCompiler.FindFreeRegister *)
{
 Finds an unoccupied register.
 Returns 'true' if found or 'false' otherwise.
}
Function TExpressionCompiler.FindFreeRegister(const RegisterChar: Char; out RegisterID: uint8; const SkipIFreg: Boolean): Boolean;
Var RegLo, RegHi, I: uint8;
Begin
 Result     := False;
 RegisterID := 0;

 RegLo := 0;
 RegHi := 0;

 For I := Low(RegisterList) To High(RegisterList) Do
 Begin
  if (RegisterList[I].RegisterChar = RegisterChar) and (RegisterList[I].isFree) Then
  Begin
   RegisterID := RegisterList[I].RegisterID;

   if (RegisterID in [1..2]) Then
   Begin
    if (RegLo = 0) Then
     RegLo := RegisterID;
   End Else
   Begin
    if (RegHi = 0) Then
     RegHi := RegisterID;
   End;

   Result := True;
  End;
 End;

 // skip 'if' register when desired
 if (RegisterChar = 'b') and (RegHi = 6) Then
  RegHi := 0;

 // prefer to choose e#3,e#4,e#5 register when available over e#1,e#2
 if (RegHi > 0) Then
 Begin
  RegisterID := RegHi;
 End Else

 if (RegLo > 0) Then
 Begin
  RegisterID := RegLo;
 End Else

 Begin
  Result := False;
 End;
End;

(* TExpressionCompiler.OccupyRegister *)
{
 Marks given register as "occupied".
}
Procedure TExpressionCompiler.OccupyRegister(const RegisterChar: Char; const RegisterID: uint8);
Begin
 With FetchRegister(RegisterChar, RegisterID)^ do
 Begin
  Value.Kind := rkUnknown;
  isFree     := False;
 End;

 With TCompiler(Compiler) do
 Begin
  if (inFunction) Then
   getCurrentFunction.invalidateRegister(RegisterChar, RegisterID);
 End;
End;

(* TExpressionCompiler.ReleaseRegister *)
{
 Marks given register as "free".
}
Procedure TExpressionCompiler.ReleaseRegister(const RegisterChar: Char; const RegisterID: uint8);
Begin
 With FetchRegister(RegisterChar, RegisterID)^ do
 Begin
  Value.Kind := rkUnknown;
  isFree     := True;
 End;
End;

(* TExpressionCompiler.InvalidateRegister *)
{
 Invalidates register value.
}
Procedure TExpressionCompiler.InvalidateRegister(const RegisterChar: Char; const RegisterID: uint8);
Begin
 FetchRegister(RegisterChar, RegisterID)^.Value.Kind := rkUnknown;
End;

(* TExpressionCompiler.FindRegisterValidatedToVariable *)
{
 Searches for a register associated with given variable's value (not the variable itself, its value!) and returns it.
 Returns "true" and a valid TExpressionCompileResult record when succeeded or "false" otherwise.
}
Function TExpressionCompiler.FindRegisterValidatedToVariable(const Variable: TObject; out CompileResult: TExpressionCompileResult): Boolean;
Var I: uint8;
Begin
 Result := False;

 For I := Low(RegisterList) To High(RegisterList) Do
 Begin
  if (RegisterList[I].Value.Kind = rkVariable) and (RegisterList[I].Value.Variable = Variable) Then
  Begin
   CompileResult.Kind         := ecRegister;
   CompileResult.Typ          := TVariable(Variable).Typ;
   CompileResult.Attributes   := [ecrImmediate];
   CompileResult.RegisterChar := RegisterList[I].RegisterChar;
   CompileResult.RegisterID   := RegisterList[I].RegisterID;

   Exit(True);
  End;
 End;
End;

(* TExpressionCompiler.ValidateRegisterToVariable *)
{
 Validates register value.
}
Procedure TExpressionCompiler.ValidateRegisterToVariable(const RegisterChar: Char; const RegisterID: uint8; const Variable: TObject);
Begin
 With FetchRegister(RegisterChar, RegisterID)^ do
 Begin
  Value.Kind     := rkVariable;
  Value.Variable := Variable;
 End;
End;

(* TExpressionCompiler.isRegisterOccupied *)
{
 Returns 'true' if given register is occupied.
}
Function TExpressionCompiler.isRegisterOccupied(const RegisterChar: Char; const RegisterID: uint8): Boolean;
Begin
 Result := (not FetchRegister(RegisterChar, RegisterID)^.isFree);
End;

(* TExpressionCompiler.IncUsedStackSlots *)
Procedure TExpressionCompiler.IncUsedStackSlots(const HowMany: int32);
Begin
 Inc(UsedStackSlots, HowMany);
End;

(* TExpressionCompiler.DecUsedStackSlots *)
Procedure TExpressionCompiler.DecUsedStackSlots(const HowMany: int32);
Begin
 Dec(UsedStackSlots, HowMany);
End;

(* TExpressionCompiler.IncDepth *)
Procedure TExpressionCompiler.IncDepth;
Begin
 Inc(CompileDepth);
End;

(* TExpressionCompiler.DecDepth *)
Procedure TExpressionCompiler.DecDepth;
Begin
 if (CompileDepth = 0) Then
  raise EExpressionNodeException.Create('CompileDepth = 0');

 Dec(CompileDepth);
End;

(* TExpressionCompiler.Create *)
Constructor TExpressionCompiler.Create(const fCompiler: TObject);
Begin
 Compiler := fCompiler;
End;

// -------------------------------------------------------------------------- //
(* TExpressionNode.PutOpcode *)
{
 Adds a new opcode into current bytecode block.
}
Procedure TExpressionNode.PutOpcode(const Op: TOpcodeKind; const Args: Array of const);
Var Opcode: PMOpcode;
Begin
 Opcode := getCompiler.PutOpcode(Op, Args);

 New(Opcode^.Token);
 Opcode^.Token^ := getToken;
End;

(* TExpressionNode.SaveResult *)
{
 Prepares TExpressionCompileResult record and saves given value into register or onto the stack.
}
Function TExpressionNode.SaveResult(const Value: TExpressionCompileResult; const CanBeImmediate: Boolean): TExpressionCompileResult;
Var RegisterID: uint8;
    Typ       : TType;
Begin
 // if result can be immediate, we're done here
 if (CanBeImmediate) Then
 Begin
  Result            := Value;
  Result.Attributes := [ecrImmediate];

  if (Result.Kind = ecRegister) Then
   ExprCompiler.OccupyRegister(Result.RegisterChar, Result.RegisterID);

  Exit;
 End;

 // clean result
 CleanResult(Result);

 // fetch TType instance
 Typ        := TType(Value.Typ);
 Result.Typ := Typ;

 // try to find a free register
 if (ExprCompiler.FindFreeRegister(Typ.RegPrefix, RegisterID)) Then
 Begin
  Result.Kind         := ecRegister;
  Result.RegisterChar := Typ.RegPrefix;
  Result.RegisterID   := RegisterID;

  ExprCompiler.OccupyRegister(Result.RegisterChar, Result.RegisterID);

  if (Result.getResult <> Value.getResult) Then
   PutOpcode(o_mov, [Result.getResult, Value.getResult]);
 End Else

 // otherwise allocate on the stack
 Begin
  Result.Kind          := ecStack;
  Result.StackPosition := -ExprCompiler.getUsedStackSlots;

  PutOpcode(o_push, [Value.getResult]);
  ExprCompiler.IncUsedStackSlots;
 End;
End;

(* TExpressionNode.MoveResultAtStack *)
{
 Pushes compilation result at the stack.
}
Function TExpressionNode.MoveResultAtStack(const ResultFrom: TExpressionCompileResult): TExpressionCompileResult;
Begin
 // prepare structure
 Result.Kind          := ecStack;
 Result.Typ           := ResultFrom.Typ;
 Result.StackPosition := 0;

 // increase used stack slots
 ExprCompiler.IncUsedStackSlots;

 // release register
 ExprCompiler.ReleaseRegister(ResultFrom);

 // push value
 PutOpcode(o_push, [ResultFrom.getResult]);
End;

(* TExpressionNode.PopResult *)
{
 Pops result back to the register if it was pushed onto the stack
}
Procedure TExpressionNode.PopResult(var Result: TExpressionCompileResult);
Var RegChar: Char;
    RegID  : uint8;
Begin
 // if bogus result, exit function
 if (Result.Typ = nil) or (Result.getType.isVoid) Then
  Exit;

 // check if the result is actually an immediate lying on the stack
 if (not
    (
     (Result.Kind = ecStack) and // must be on the stack
     // (Result.isImmediate) and // must be an immediate
     (not Result.isVariableHolder) // cannot hold variable
    )) Then
 Begin
  Exit;
 End;

 // fetch register prefix
 RegChar := Result.getType.RegPrefix;

 // find a free register
 if (ExprCompiler.FindFreeRegister(RegChar, RegID)) Then
 Begin
  // fill record fields
  Result.Kind         := ecRegister;
  Result.RegisterChar := RegChar;
  Result.RegisterID   := RegID;

  // mark this register as occupied
  ExprCompiler.OccupyRegister(RegChar, RegID);

  // put the 'pop' opcode
  PutOpcode(o_pop, [Result.getResult]);
  ExprCompiler.DecUsedStackSlots;
 End Else

 // no free register found
 Begin
  Exit;
 End;
End;

(* TExpressionNode.Compile *)
{
 Compiles node.
}
Function TExpressionNode.Compile(const CanBeImmediate: Boolean): TExpressionCompileResult;
Begin
 ExprCompiler.IncDepth;
 Result := __compile(CanBeImmediate);
 ExprCompiler.DecDepth;
End;

(* TExpressionNode.MoveResultToRegister *)
{
 Moves compilation result to a register.
}
Function TExpressionNode.MoveResultToRegister(const ResultFrom: TExpressionCompileResult; const RegisterChar: Char; const RegisterID: uint8): TExpressionCompileResult;
Begin
 CleanResult(Result);

 Result.Kind         := ecRegister;
 Result.Typ          := ResultFrom.Typ;
 Result.RegisterChar := RegisterChar;
 Result.RegisterID   := RegisterID;

 // register
 if (ResultFrom.Kind = ecRegister) Then
 Begin
  PutOpcode(o_mov, [Result.getResult, ResultFrom.getResult]);
 End Else

 // stack
 if (ResultFrom.Kind = ecStack) Then
 Begin
  PutOpcode(o_pop, [Result.getResult]);
  ExprCompiler.DecUsedStackSlots;
 End Else

 // invalid
 Begin
  raise EExpressionNodeException.Create('Invalid enum kind');
 End;
End;

(* TExpressionNode.Create *)
{
 Creates a TExpressionNode instance.
}
Constructor TExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P);
Begin
 Token        := fToken;
 ExprCompiler := TExpressionCompiler(fExprCompiler);
End;

(* TExpressionNode.CompileExpression *)
Function TExpressionNode.CompileExpression: TExpressionCompileResult;
Begin
 ExprCompiler.Reset;
 Result := Compile(True);
 PopResult(Result);

 if (ExprCompiler.getUsedStackSlots > 0) Then
  PutOpcode(o_sub, ['stp', ExprCompiler.getUsedStackSlots]);
End;

(* TExpressionNode.Optimize *)
{
 Optimizes node so that the its generated code will be faster and smaller.
 By the default it returns copy (clone) of the node itself.

 @Note:
   Should NOT be mistaken for computing value of the node!
   Optimization: 1 + 2*3 -> 2*3 + 1
   Computing: 1 + 2*3 -> 7
}
Function TExpressionNode.Optimize: TExpressionNode;
Begin
 Result := Clone();
End;

(* TExpressionNode.Evaluate *)
{
 Tries to evaluate node value, or at least computes what can be done.
 Eg.:
  2+2*2 -> 6
  2+2+x -> 4+x
  and so on

 By the default it returns copy (clone) of the node itself.

 @Note:
   Should NOT be mistaken for optimization of the node!
   Optimization: 1 + 2*3 -> 2*3 + 1
   Computing: 1 + 2*3 -> 7
}
Function TExpressionNode.Evaluate: TExpressionNode;
Begin
 Result := Clone();
End;

(* TExpressionNode.Clone *)
{
 Returns a clone (copy) of this object.
 Should be overriden by descendants.
}
Function TExpressionNode.Clone: TExpressionNode;
Begin
 raise EExpressionNodeException.Create('Abstract TExpressionNode.Clone() called!');
End;

(* TExpressionNode.findIdentifier *)
{
 Parses whole expression tree and returns "true" if there exists a node for which "findIdentifier(Symbol)" returns true.
 Should be overriden by descendant classes.
 By default it returns 'false'.
}
Function TExpressionNode.findIdentifier(const Symbol: TObject): Boolean;
Begin
 Result := False;
End;

(* TExpressionNode.getCost *)
{
 Returns cost of compiling the whole tree (in most cases it's just number of child nodes plus one, as for itself).
 Should be overriden by descendant classes.
 By default it returns '1'.
}
Function TExpressionNode.getCost: uint32;
Begin
 Result := 1;
End;

(* TExpressionNode.getPredictedType *)
{
 Returns predicted type of node or eptUnknown if it's not known.
 Should be overriden by descendant classes.
}
Function TExpressionNode.getPredictedType: TMixedValueKind;
Begin
 Result := mvUnknown;
End;

(* TExpressionNode.isLValue *)
{
 Should return 'true' if node represents an l-value.
 By default it returns 'false'.
}
Function TExpressionNode.isLValue: Boolean;
Begin
 Result := False;
End;

// -------------------------------------------------------------------------- //
(* TUnaryExpressionNode.Create *)
Constructor TUnaryExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode);
Begin
 inherited Create(fExprCompiler, fToken);

 Child := fChild;
End;

(* TUnaryExpressionNode.findIdentifier *)
{
 See TExpressionNode.findIdentifier
}
Function TUnaryExpressionNode.findIdentifier(const Symbol: TObject): Boolean;
Begin
 if (Child = nil) Then
  Result := False Else
  Result := Child.findIdentifier(Symbol);
End;

(* TUnaryExpressionNode.getCost *)
{
 See TExpressionNode.getCost
}
Function TUnaryExpressionNode.getCost: uint32;
Begin
 Result := 1;

 if (Child <> nil) Then
  Result += Child.getCost;
End;

(* TUnaryExpressionNode.getPredictedType *)
{
 See TExpressionNode.getPredictedType
}
Function TUnaryExpressionNode.getPredictedType: TMixedValueKind;
Begin
 Result := Child.getPredictedType;
End;

(* TUnaryExpressionNode.setChild *)
Procedure TUnaryExpressionNode.setChild(const Node: TExpressionNode);
Begin
 Child := Node;
End;

// -------------------------------------------------------------------------- //
(* TBinaryExpressionNode.CompileChildNodes *)
{
 Compildes two child nodes and returns their result by the ref-arguments.
}
Procedure TBinaryExpressionNode.CompileChildNodes(out LArg, RArg: TExpressionCompileResult; const ReleaseRegs: Boolean; const CanBothBeImmediate: Boolean);
Begin
 // compile
 if (Left.getCost > Right.getCost) Then
 Begin
  LArg := Left.Compile(CanBothBeImmediate);
  RArg := Right.Compile(True);

  PopResult(RArg);
  PopResult(LArg);
 End Else
 Begin
  RArg := Right.Compile(True);
  LArg := Left.Compile(CanBothBeImmediate);

  PopResult(LArg);
  PopResult(RArg);
 End;

 // release
 if (ReleaseRegs) Then
 Begin
  ExprCompiler.ReleaseRegister(LArg);
  ExprCompiler.ReleaseRegister(RArg);
 End;
End;

(* TBinaryExpressionNode.Create *)
Constructor TBinaryExpressionNode.Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode);
Begin
 inherited Create(fExprCompiler, fToken);

 Left  := fLeft;
 Right := fRight;
End;

(* TBinaryExpressionNode.Evaluate *)
Function TBinaryExpressionNode.Evaluate: TExpressionNode;
Var Tmp: TBinaryExpressionNode;
Begin
 Tmp := TBinaryExpressionNode(Clone());

 Tmp.Left  := Tmp.Left.Evaluate();
 Tmp.Right := Tmp.Right.Evaluate();

 Result := Tmp;
End;

(* TBinaryExpressionNode.findIdentifier *)
{
 See TExpressionNode.findIdentifier
}
Function TBinaryExpressionNode.findIdentifier(const Symbol: TObject): Boolean;
Begin
 Result := False;

 if (Left <> nil) Then
  Result := Result or Left.findIdentifier(Symbol);

 if (Right <> nil) Then
  Result := Result or Right.findIdentifier(Symbol);
End;

(* TBinaryExpressionNode.getCost *)
{
 See TExpressionNode.getCost
}
Function TBinaryExpressionNode.getCost: uint32;
Begin
 Result := 1;

 if (Left <> nil) Then
  Result += Left.getCost;

 if (Right <> nil) Then
  Result += Right.getCost;
End;

(* TBinaryExpressionNode.getPredictedType *)
{
 See TExpressionNode.getPredictedType
}
Function TBinaryExpressionNode.getPredictedType: TMixedValueKind;
Var tL, tR: TMixedValueKind;
Begin
 // predict types for children
 tL := Left.getPredictedType;
 tR := Right.getPredictedType;

 // if both types are the same, return it
 if (tL = tR) Then
  Exit(tL);

 // other cases:

 // int/float  op  float/int -> float (eg: 2.5+3)
 if (tL in [mvInt, mvFloat]) or (tR in [mvInt, mvFloat]) Then
  Exit(mvFloat);

 // int/char  op  char/int -> char (eg.: 'x'+4)
 if (tL in [mvChar, mvInt]) or (tR in [mvChar, mvInt]) Then
  Exit(mvChar);

 // string/char  op  char/string -> string (eg.: 'x'+"y")
 if (tL in [mvChar, mvString]) and (tR in [mvChar, mvString]) Then
  Exit(mvString);

 Exit(mvUnknown);
End;

(* TBinaryExpressionNode.setLeft *)
Procedure TBinaryExpressionNode.setLeft(const Node: TExpressionNode);
Begin
 Left := Node;
End;

(* TBinaryExpressionNode.setRight *)
Procedure TBinaryExpressionNode.setRight(const Node: TExpressionNode);
Begin
 Right := Node;
End;

(* TBinaryExpressionNode.SwapNodes *)
{
 Swaps the children nodes.
}
Procedure TBinaryExpressionNode.SwapNodes;
Var Tmp: TExpressionNode;
Begin
 Tmp   := Left;
 Left  := Right;
 Right := Tmp;
End;

// -------------------------------------------------------------------------- //
// nullary
{$I identifier.inc}
{$I constant.inc}

// unary
{$I call.inc}
{$I neg.inc}
{$I logicalnot.inc}
{$I bitwisenot.inc}
{$I prepostop.inc}
{$I new.inc}
{$I cast.inc}
{$I fieldfetch.inc}

// binary
{$I math.inc}
{$I bitwisemath.inc}
{$I logicalmath.inc}
{$I compare.inc}
{$I assign.inc}
{$I arrayelement.inc}
End.
