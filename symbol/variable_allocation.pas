// @TODO: use better method in allocators

(* AllocateVariables *)
Procedure AllocateVariables(const CanUseRegs: Boolean);
Type TVarList = specialize TFPGList<PVar>;
Var VarList: TVarList;
    Symbol : TSymbol;
    VarRec : PVar;

Const reg_eb = 0;
      reg_ec = 1;
      reg_ei = 2;
      reg_ef = 3;
      reg_es = 4;
      reg_er = 5;

Var FreeRegs: Array[reg_eb..reg_er] of Set of uint8 = // variables can be allocated only in `e_3` and `e_4` registers (and of course on the stack)
([3, 4], [3, 4], [3, 4], [3, 4], [3, 4], [3, 4]);

    { CheckRanges }
    Function CheckRanges(const A, B: TRange): Boolean;
    Begin
     Result := (not ((A.PBegin.Position >= B.PBegin.Position) and (A.PEnd.Position <= B.PEnd.Position))) and
               (not ((B.PBegin.Position >= A.PBegin.Position) and (B.PEnd.Position <= A.PEnd.Position)));
    End;

    { Alloc }
    Procedure Alloc(mVar: TVariable);
    Var RegChar: Char;
        VarRec : PVar;
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
           ((TmpVar.MemPos >= 1) and (CoalVar.MemPos <= 0)) { we prefer variables allocated in registers than on the stack } Then
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
          "b1" would be put into 'ei3', as it liveness doesn't collide with "a2"
          "a1" would be put into 'ei3', because the compiler visites the list from the beginning. And here is the 'ooops': "a1" and "b1" share the same register!
         *)
         Can := True;

         For VarRec in VarList Do
          if (VarRec^.mVar = mVar) Then
           Break Else
          if (not CheckRanges(VarRec^.mVar.RefSymbol.Range, mVar.RefSymbol.Range)) and
             (VarRec^.mVar.Typ.RegPrefix = TmpVar.Typ.RegPrefix) and (VarRec^.mVar.MemPos = TmpVar.MemPos) Then
          Begin
           Can := False;
           Break;
          End;

         if (Can) Then
          CoalVar := TmpVar;
        End;
     End;

     if (CoalVar <> nil) Then // yes: this variable can be coalesced with another one!
     Begin
      mVar.MemPos := CoalVar.MemPos;
      Exit;
     End;

     mVar.MemPos := 0;

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
      mVar.MemPos := Reg;
      Exclude(FreeRegs[FRIndex], Reg);

      New(StackReg);
      StackReg^.RegChar := RegChar;
      StackReg^.RegID   := Reg;
      Func.StackRegs.Add(StackReg);

      Break;
     End;

     if (mVar.MemPos = 0) Then // we have to allocate it on the stack
     Begin
      mVar.MemPos := -Func.StackSize;
      Inc(Func.StackSize);
     End;
    End;

Var StackPos: uint16 = 0;
Begin
 Func.StackSize := 0;
 Func.StackRegs.Clear;

 if (CanUseRegs) Then
 Begin
  { register and stack allocation }
  VarList := TVarList.Create;

  Try
   // sort variabes ascending by their CFG cost
   For Symbol in Func.SymbolList Do
    if (Symbol.Typ = stVariable) Then
    Begin
     if (not Symbol.mVariable.isConst) and (not Symbol.mVariable.isFuncParam) and (not Symbol.mVariable.DontAllocate) Then
     Begin
      New(VarRec);
      VarRec^.mVar    := Symbol.mVariable;
      VarRec^.CFGCost := getVariableCFGCost(Symbol, Func.FlowGraph.Root, nil);

      VarList.Add(VarRec);
     End;
    End;

   VarList.Sort(@SortVarRec);

   For VarRec in VarList Do
    Alloc(VarRec^.mVar);
  Finally
   For VarRec in VarList Do
    Dispose(VarRec);
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
    Inc(Func.StackSize);
   End;
 End;
End;
