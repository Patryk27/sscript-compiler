(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit RegisterAllocator;

 Interface
 Uses VariableAllocator, symdef, FGL;

 { TVar }
 Type PVar = ^TVar;
      TVar =
      Record
       mVar   : TVariable;
       CFGCost: uint32;
      End;

 { TVarList }
 Type TVarList = specialize TFPGList<PVar>;

 { a few constants used in TRegisterAllocator.AllocateVariable() }
 Const reg_eb = 0;
       reg_ec = 1;
       reg_ei = 2;
       reg_ef = 3;
       reg_es = 4;
       reg_er = 5;

 { TRegisterAllocator }
 Type TRegisterAllocator =
      Class (TVariableAllocator)
       Private
        VarList : TVarList;
        FreeRegs: Array[reg_eb..reg_er] of Set of uint8;
        StackPos: uint16;

       Private
        Procedure AllocateVariable(const mVar: TVariable);

       Public
        Procedure Execute; override;
       End;

 Implementation
Uses FlowGraph;

(* SortVarRec *)
Function SortVarRec(const A, B: PVar): Integer;
Begin
 if (A^.CFGCost > B^.CFGCost) Then
  Result := -1 Else

 if (A^.CFGCost < B^.CFGCost) Then
  Result := 1 Else

  Result := 0;
End;

(* CheckRanges *)
Function CheckRanges(const A, B: TRange): Boolean;
Begin
 Result := (not ((A.PBegin.Position >= B.PBegin.Position) and (A.PEnd.Position <= B.PEnd.Position))) and
           (not ((B.PBegin.Position >= A.PBegin.Position) and (B.PEnd.Position <= A.PEnd.Position)));
End;

// -------------------------------------------------------------------------- //
(* TRegisterAllocator.AllocateVariable *)
Procedure TRegisterAllocator.AllocateVariable(const mVar: TVariable);
Var VarRec, VarRec2: PVar;

    RegChar: Char;
    TmpVar : TVariable;

    CoalVar : TVariable = nil;
    StackReg: PStackSavedReg;

    Reg: uint8;
    Can: Boolean;

    FRIndex: uint8;
Begin
 RegChar := mVar.Typ.RegPrefix;

 // at first, check if this variable can share register (or stack position) with any other var
 For VarRec in VarList Do
 Begin
  TmpVar := VarRec^.mVar;

  if (TmpVar = mVar) Then
   Break;

  if (CheckRanges(TmpVar.RefSymbol.Range, mVar.RefSymbol.Range)) Then // if variables don't overlap each other in code...
   if (TmpVar.Typ.RegPrefix = RegChar) Then // if types are the same...
    if (CoalVar = nil) or // if no coal-with-var set yet...
       ((TmpVar.LocationData.Location = vlRegister) and (CoalVar.LocationData.Location = vlStack)) { we prefer variables allocated in registers rather than on the stack } Then
    Begin
     (*
      @Note: we have a candidate, but there's one more thing we need to check; let's consider this code:

       if (condition)
       {
        var<int> a1, b1;
       } else
       {
        var<int> a2;
       }

      If we just did 'CoalVar := TmpVar;' (without this check below), everything would be put into 'ei3'.
      Why?
      Let's take a look at the "VarList" which is generated for the code above:
      -> a2, b1, a1

      Thus:
      "a2" would be directly but into 'ei3'
      "b1" would be put into 'ei3', as it liveness doesn't overwrite "a2"
      "a1" would also be put into 'ei3', because the compiler visits the list from the beginning. And here we have the ooops: "a1" and "b1" share the same register!
     *)
     Can := True;

     For VarRec2 in VarList Do
     Begin
      if (VarRec2^.mVar = mVar) Then
      Begin
       Break;
      End Else
      if (not CheckRanges(VarRec2^.mVar.RefSymbol.Range, mVar.RefSymbol.Range)) and
         (VarRec2^.mVar.Typ.RegPrefix = TmpVar.Typ.RegPrefix) and (VarRec2^.mVar.LocationData = TmpVar.LocationData) Then
      Begin
       Can := False;
       Break;
      End;

     if (Can) Then
      CoalVar := TmpVar;
     End;
    End;
 End;

 if (CoalVar <> nil) Then // yes: this variable can be coalesced with another one!
 Begin
  mVar.LocationData := CoalVar.LocationData;
  Exit;
 End;

 mVar.LocationData.Location := vlNone;

 if (not mVar.Typ.isAny) Then // 'any'-typed variables must be allocated on the stack
 Begin
  Case RegChar of
   'b': FRIndex := reg_eb;
   'c': FRIndex := reg_ec;
   'i': FRIndex := reg_ei;
   'f': FRIndex := reg_ef;
   's': FRIndex := reg_es;
   'r': FRIndex := reg_er;
  End;

  For Reg in FreeRegs[FRIndex] Do
  Begin
   mVar.LocationData.Location   := vlRegister;
   mVar.LocationData.RegisterID := Reg;
   Exclude(FreeRegs[FRIndex], Reg);

   New(StackReg);
   StackReg^.RegChar := RegChar;
   StackReg^.RegID   := Reg;
   CurrentFunction.StackRegs.Add(StackReg);

   Break;
  End;
 End;

 if (mVar.LocationData.Location = vlNone) Then // we have to allocate it on the stack
 Begin
  mVar.LocationData.Location      := vlStack;
  mVar.LocationData.StackPosition := -StackPos;

  Inc(StackPos);
  Inc(CurrentFunction.StackSize);
 End;
End;

(* TRegisterAllocator.Execute *)
Procedure TRegisterAllocator.Execute;
Var Symbol: TSymbol;
    VarRec: PVar;
    I     : uint8;
Begin
 { prepare variables }
 For I := Low(FreeRegs) To High(FreeRegs) Do
  FreeRegs[I] := [3, 4, 5]; // variables can be allocated only in `e_3`, `e_4` and `e_5` registers (and of course on the stack)

 { register and stack allocation }
 VarList  := TVarList.Create;
 StackPos := 0;

 Try
  // sort variabes ascending by their CFG cost
  For Symbol in CurrentFunction.SymbolList Do
  Begin
   if (Symbol.Typ = stVariable) Then
   Begin
    if (not Symbol.mVariable.isConst) and (not Symbol.mVariable.isFuncParam) and (not Symbol.mVariable.DontAllocate) Then
    Begin
     New(VarRec);
     VarRec^.mVar    := Symbol.mVariable;
     VarRec^.CFGCost := getVariableCFGCost(Symbol, CurrentFunction.FlowGraph.Root, nil);

     VarList.Add(VarRec);
    End;
   End;
  End;

  VarList.Sort(@SortVarRec);

  // allocate starting from the variable with the highest CFG cost
  For VarRec in VarList Do
   AllocateVariable(VarRec^.mVar);
 Finally
  For VarRec in VarList Do
   Dispose(VarRec);

  VarList.Free;
 End;
End;
End.
