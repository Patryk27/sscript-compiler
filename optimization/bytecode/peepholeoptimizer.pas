(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$MODE DELPHI} // we're using Delphi syntax because using it, there's no need to write the dereference operator (`^`) all the time.
Unit PeepholeOptimizer;

 Interface
 Uses Optimizer, Variants;

 { TPeepholeOptimizer }
 Type TPeepholeOptimizer =
      Class (TOptimizer)
       Private
       Public
        Procedure Execute;
       End;

 Implementation
Uses Logging, Opcodes, SysUtils, Messages;

(* isRegister *)
{
 Returns `true` when passed primary type is a register.
}
Function isRegister(const T: TPrimaryType): Boolean; inline;
Begin
 Result := T in [ptBoolReg, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg];
End;

(* isMemoryRef *)
{
 Returns `true` if passed primary type is a memory reference.
}
Function isMemoryRef(const T: TPrimaryType): Boolean; inline;
Begin
 Result := T in [ptSymbolMemRef, ptConstantMemRef];
End;

(* isInt *)
{
 Returns `true` when passed primary type is an int.
}
Function isInt(const T: TPrimaryType): Boolean; inline;
Begin
 Result := (T = ptInt);
End;

(* isVariableHolder *)
{
 Returns `true` when passed opcode argument is a variable-holder.

 @Note: 'variable-holders' are registers `e_3` and `e_4`, and also stackvals and
        memory references.
}
Function isVariableHolder(const T: TMOpcodeArg): Boolean; inline;
Begin
 Result := isRegister(T.Typ);

 if (Result) Then
  Exit(StrToInt(VarToStr(T.Value)) in [2..4]) Else
  Exit(isMemoryRef(T.Typ) or (T.Typ = ptStackVal));
End;

// -------------------------------------------------------------------------- //
(* TPeepholeOptimizer.Execute *)
Procedure TPeepholeOptimizer.Execute;
Var Pos, Pos2            : uint64;
    oCurrent, oNext, oTmp: TMOpcode;
    pCurrent, pNext, pTmp: PMOpcode;
    CanBeRemoved         : Boolean;
    PushFix, I           : Integer;
    Optimized            : Boolean;
    TmpArg               : TMOpcodeArg;

  { isArgumentChanging }
  Function isArgumentChanging(const Param: uint8): Boolean; inline;
  Begin
   Result := (oTmp.Args[Param] = oCurrent.Args[0]) or (oTmp.Args[Param] = oCurrent.Args[1]);
  End;

  { __optimize1 }
  Procedure __optimize1(const Param: uint8); inline;
  Begin
   if (oTmp.Args[Param] = oCurrent.Args[0]) Then
   Begin
    TmpArg           := oTmp.Args[Param];
    pTmp.Args[Param] := oCurrent.Args[1]; // replace register with value

    if (isValidOpcode(pTmp^)) Then // is it a valid opcode now?
    Begin
     if (pTmp.Args[Param].Typ = ptStackVal) and (PushFix <> 0) Then
      pTmp.Args[Param].Value -= PushFix;

     Optimized := True;
    End Else // if it's not
     pTmp.Args[Param] := TmpArg;
   End;
  End;

Var OpcodeCount: int64;
Begin
 With Compiler do
 Begin
  Log('-> Peephole bytecode optimizer');

  if (OpcodeList.Count = 0) Then
  Begin
   DevLog(dvInfo, 'OpcodeList.Count = 0; no optimization(s) can be done!');
   Exit;
  End;

  Pos         := 0;
  OpcodeCount := OpcodeList.Count;

  While (Pos < OpcodeList.Count-1) Do
  Begin
   pCurrent := OpcodeList[Pos];
   pNext    := OpcodeList[Pos+1];

   oCurrent := pCurrent^;
   oNext    := pNext^;

   if (oCurrent.isLabel) or (oCurrent.isComment) or (oCurrent.Opcode in [o_bool, o_char, o_int, o_float, o_string]) Then
   Begin
    Inc(Pos);
    Continue;
   End;

   if (oCurrent.Compiler = nil) or (oNext.Compiler = nil) Then // don't optimize bytecode imported from libraries (it could break labels' addresses)
   Begin
    Inc(Pos);
    Continue;
   End;

   {
    nop()
    ->
    [nothing]
   }
   if (oCurrent.Opcode = o_nop) Then
   Begin
    OpcodeList.Remove(pCurrent);
    Continue;
   End;

   {
    mov(x, x)
    ->
    [nothing]
   }
   if (oCurrent.Opcode = o_mov) Then
   Begin
    if (oCurrent.Args[0] = oCurrent.Args[1]) Then
    Begin
     OpcodeList.Remove(pCurrent);
     Continue;
    End;
   End;

   {
    push(value)
    pop(register)
    ->
    mov(register, value)
   }
   if (oCurrent.Opcode = o_push) and (oNext.Opcode = o_pop) Then
   Begin
    if (oCurrent.Args[0] = oNext.Args[0]) Then
    Begin
     {
      push(some register)
      pop(the same register)
      ->
      [nothing]
      (it would be anyway optimized later (because this would be changed to `mov(reg, reg)` and then removed), but I'd like to do it here)
     }
     OpcodeList.Remove(pCurrent);
     OpcodeList.Remove(pNext);
     Continue;
    End;

    pCurrent.Opcode := o_mov;

    SetLength(pCurrent.Args, 2);
    pCurrent.Args[0] := oNext.Args[0];
    pCurrent.Args[1] := oCurrent.Args[0];

    OpcodeList.Remove(pNext);

    Continue;
   End;

   {
    add/sub(register, 0)
    ->
    [nothing]
   }
   if (oCurrent.Opcode in [o_add, o_sub]) Then
   Begin
    if (isInt(oCurrent.Args[1].Typ) and (VarToStr(oCurrent.Args[1].Value) = '0')) Then
    Begin
     OpcodeList.Remove(pCurrent);
     Continue;
    End;
   End;

   {
    mov(reg1, reg2)
    mov(reg2, reg1)
    ->
    mov(reg1, reg2)
   }
   if (oCurrent.Opcode = o_mov) and (oNext.Opcode = o_mov) Then
   Begin
    if (oCurrent.Args[0] = oNext.Args[1]) and
       (oCurrent.Args[1] = oNext.Args[0]) Then
    Begin
     OpcodeList.Remove(pNext);
     Continue;
    End;
   End;

   {
    jmp(a)
    jmp(b)
    ->
    jmp(a)
   }
   if (oCurrent.Opcode = o_jmp) and (oNext.Opcode = o_jmp) Then
   Begin
    OpcodeList.Remove(pNext);
    Continue;
   End;

   {
    Copy propagation

    mov(register, value)
    (...)
    opcode(some value or reg, register)
    ->
    opcode(some value or reg, value)

    If that the register's value does not change in the opcodes between, of course.
   }
   if (oCurrent.Opcode = o_mov) and (not (oCurrent.Args[0].Typ = ptStackVal)) Then
   Begin
    CanBeRemoved := False;
    Optimized    := False;
    Pos2         := Pos+1;
    PushFix      := 0;

    While (Pos2 < OpcodeList.Count-1) Do
    Begin
     pTmp := OpcodeList[Pos2];
     oTmp := pTmp^;

     if (oTmp.isLabel) Then // stop on labels
      Break;

     if (oTmp.isComment) or (Length(oTmp.Args) = 0) Then // skip comments and `nop`s()
     Begin
      Inc(Pos2);
      Continue;
     End;

     if (oTmp.Compiler = nil) Then // stop on non-our bytecode (ie. - don't optimize imported bytecode)
      Break;

     if (oTmp.Opcode in [o_mov, o_pop, o_neg, o_not, o_xor, o_or, o_and, o_shr, o_shl, o_strjoin, o_add, o_sub, o_mul, o_div, o_mod]) and
        (isArgumentChanging(0)) Then
     Begin
      Break; // the register's value is changed -> stop propagation
     End;

     if (oTmp.Opcode = o_arset) Then // arset(out, in, in)
      if (isArgumentChanging(0)) Then
       Break;

     if (oTmp.Opcode = o_arget) Then // arget(in, in, out)
      if (isArgumentChanging(2)) Then
       Break;

     if (oTmp.Opcode = o_arset) Then // arcrt(out, in, in)
      if (isArgumentChanging(0)) Then
       Break;

     if (oTmp.Opcode = o_arlen) Then // arlen(in, in, out)
      if (isArgumentChanging(2)) Then
       Break;

     if (oTmp.Opcode in [o_call, o_acall]) Then // copy propagation can be applied to these two calls as well
      __optimize1(0);

     if (oTmp.Opcode in [o_call, o_acall, o_jmp, o_fjmp, o_tjmp]) Then // stop on jumps and calls
     Begin
      Optimized := False;
      { @TODO: this is lame solution...

       Input:
        mov(ef1, [-1])
        if_g(ef1, 512)
        tjmp(:some_label)
        mul(ef1, ef1)
        some_label:

       Output (without this line above):
        if_g([-1], 512)
        tjmp(:some_label)
        mul(ef1, ef1)
        some_label:

        No assignment to `ef1`, so in the third line it has unknown value!

       Output (with this line):
        mov(ef1, [-1])
        if_g([-1], 512)
        tjmp(:some_label)
        mul(ef1, ef1)
        some_label:

        It's some-way-fixed, but still... this isn't an optimization then.
      }
      Break;
     End;

     { optimize }
     For I := Low(oTmp.Args) To High(oTmp.Args) Do
      __optimize1(I);

     if (oTmp.Opcode in [o_arset, o_arget]) and (oTmp.Args[1].Typ = ptInt) Then
      PushFix -= oTmp.Args[1].Value;

     if (oTmp.Opcode in [o_arcrt]) and (oTmp.Args[2].Typ = ptInt) Then
      PushFix -= oTmp.Args[2].Value;

     if (oTmp.Opcode = o_push) Then
      Inc(PushFix);

     if (oTmp.Opcode = o_pop) Then
      Dec(PushFix);

     Inc(Pos2);
    End;

    { if optimized anything }
    if (Optimized) Then
    Begin
     if (not isVariableHolder(pCurrent^.Args[0])) Then
     Begin
      OpcodeList.Remove(pCurrent); // and remove the first `mov`

      Dec(Pos);
      Continue;
     End;
    End Else
    Begin
     if (not isVariableHolder(pCurrent^.Args[0])) and (CanBeRemoved) Then
     {
      the first `mov` is unusable.
      Like in this code:
       mov(ei1, 10) <- this ei1's value in unused, so this `mov` can be removed as it won't affect anything
       mov(ei1, 20)
     }
     Begin
      OpcodeList.Remove(pCurrent);

      Dec(Pos);
      Continue;
     End;
    End;
   End;

   Inc(Pos);
  End;

  OpcodeCount -= OpcodeList.Count;
  Log('Peephole results: removed '+IntToStr(abs(OpcodeCount))+' opcodes');
 End;
End;

End.
