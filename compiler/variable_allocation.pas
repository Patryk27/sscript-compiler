// @TODO: use better method (like graph coloring)

(* AllocateVariables *)
Procedure AllocateVariables(const CanUseRegs: Boolean);
Type TVarList = specialize TFPGList<PVar>;
Var VarList: TVarList;
    Symbol : TLocalSymbol;
    VarRec : PVar;

    // Alloc
    Procedure Alloc(mVar: TVariable);
    Var FreeRegs: Set of Byte = [3, 4]; // variables can be allocated only in `e_3` and `e_4` registers (and ofc.on the stack)
        RegChar : Char;
        VarRec  : PVar;
        StackPos: uint8 = 0;
        TmpVar  : TVariable;
    Begin
     RegChar := mVar.Typ.RegPrefix;

     For VarRec in VarList Do // check each variable
     Begin
      TmpVar := VarRec^.mVar;

      if (TmpVar = mVar) Then
       Break;

      if (TmpVar.Typ.RegPrefix = RegChar) and (TmpVar.MemPos in [3..4]) Then // if allocated in the register, exclude occupied register from the available regs list
       Exclude(FreeRegs, TmpVar.MemPos);

      if (TmpVar.MemPos <= 0) Then // if allocated on the stack
       Inc(StackPos);
     End;

     if (3 in FreeRegs) Then
      mVar.MemPos := 3 Else
     if (4 in FreeRegs) Then
      mVar.MemPos := 4 Else
      mVar.MemPos := -StackPos;
    End;

Var StackPos: uint16 = 0;
Begin
 if (CanUseRegs) Then
 Begin
  { register and stack allocation }
  VarList := TVarList.Create;

  Try
   // the most used variables are going to registers, rest onto the stack.
   For Symbol in Func.SymbolList Do
    if (Symbol.Typ = lsVariable) Then
    Begin
     if (not Symbol.mVariable.isConst) and (not Symbol.mVariable.isFuncParam) and (not Symbol.mVariable.DontAllocate) Then
     Begin
      New(VarRec);
      VarRec^.mVar     := Symbol.mVariable;
      VarRec^.UseCount := getVariableUseCount(Symbol.mVariable, Func.FlowGraph.Root, nil);

      VarList.Add(VarRec);
     End;
    End;

   VarList.Sort(@SortVarRec);

   For VarRec in VarList Do
    Alloc(VarRec^.mVar);
  Finally
   VarList.Free;
  End;
 End Else
 Begin
  { stack-only allocation }
  For Symbol in Func.SymbolList Do
   if (not Symbol.mVariable.isConst) and (not Symbol.mVariable.isFuncParam) and (not Symbol.mVariable.DontAllocate) Then
   Begin
    Symbol.mVariable.MemPos := -StackPos;
    Inc(StackPos);
   End;
 End;
End;
