(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODE DELPHI}
Unit Peephole;

 Interface
 Uses Compile1, Variants;

 Procedure OptimizeBytecode(Compiler: TCompiler);

 Implementation
Uses Opcodes, SysUtils;

Const SafeOpcodes: Set of TOpcode_E =
[
 o_if_e, o_if_ne, o_if_g, o_if_l, o_if_ge, o_if_le
];

(* isRegister *)
{
 Returns `true` when passed primary type is a register
}
Function isRegister(T: TPrimaryType): Boolean;
Begin
 Result := T in [ptBoolReg, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg];
End;

(* isInt *)
{
 Returns `true` when passed primary type is an int
}
Function isInt(T: TPrimaryType): Boolean;
Begin
 Result := T in [ptInt];
End;

(* isVariableHolder *)
{
 Returns `true` when passed opcode argument is a variable-holder.

 @Note: 'variable-holders' are registers `e_3` and `e_4`, as only there a variable
        can be allocated (when `-Or` is passed into the compiler).
}
Function isVariableHolder(T: TMOpcodeArg): Boolean;
Begin
 Result := isRegister(T.Typ);

 if (Result) Then
  Exit(StrToInt(VarToStr(T.Value)) > 2);
End;

(* OptimizeBytecode *)
{
 Optimizes bytecode for smaller size and faster execution.
}
Procedure OptimizeBytecode(Compiler: TCompiler);
Var Pos, Pos2            : LongWord;
    oCurrent, oNext, oTmp: TMOpcode;
    pCurrent, pNext, pTmp: PMOpcode;
    PushFix              : Integer;
    Optimized            : Boolean;
    TmpArg               : TMOpcodeArg;
Begin
 With Compiler do
 Begin
  Pos := 0;

  While (Pos < OpcodeList.Count-2) Do
  Begin
   pCurrent := OpcodeList[Pos];
   pNext    := OpcodeList[Pos+1];

   oCurrent := pCurrent^;
   oNext    := pNext^;

   if (oCurrent.isLabel) or (oCurrent.isComment) or (oCurrent.Opcode in [o_byte, o_word, o_integer, o_extended]) Then
   Begin
    Inc(Pos);
    Continue;
   End;

   {
    mov(register, register)
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
      push(register)
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
    add(register, 0)
    sub(register, 0)
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

   // @TODO: mul(register, 0) -> mov(register, 0)

   {
    mov(register, value)
    (...)
    opcode(some value or reg, register)
    [ safe opcode(register, some value or reg) ]
    ->
    opcode(some value or reg, value)
    [ safe opcode(value, some value or reg) ]

    Assuming that the register isn't changing in the opcodes between.
   }
   if (oCurrent.Opcode = o_mov) and not (oCurrent.Args[0].Typ = ptStackVal) Then
   Begin
    Optimized := False;
    Pos2      := Pos+1;
    PushFix   := 0;

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

     if (oTmp.Opcode in [o_neg, o_not, o_xor, o_or, o_and, o_shr, o_shl, o_strjoin, o_mov, o_pop, o_add, o_sub, o_mul, o_div, o_mod]) and
        ((oTmp.Args[0] = oCurrent.Args[0]) or (oTmp.Args[0] = oCurrent.Args[1])) Then
         Break; // the register's value is changing somewhere by the way

     if (oTmp.Opcode = o_arget) Then
      if (oTmp.Args[2] = oCurrent.Args[0]) or (oTmp.Args[2] = oCurrent.Args[1]) Then
       Break; // the register's value is changed by `arget` (`arget(in, in, out)`)

     if (oTmp.Opcode in [o_jmp, o_fjmp, o_tjmp]) or (oTmp.Opcode = o_call) Then // stop on jumps and calls
      Break;

     if (oTmp.Opcode = o_push) Then
      Inc(PushFix);

     if (oTmp.Opcode = o_pop) Then
      Dec(PushFix);

     { 1-param opcodes }
     if (Length(oTmp.Args) = 1) Then
     Begin
      if (oTmp.Opcode in [o_push, o_jmp, o_tjmp, o_fjmp, o_call]) Then // push, jmp, tjmp, fjmp, call
      Begin
       if (oTmp.Args[0] = oCurrent.Args[0]) Then // yes, optimize! :)
       Begin
        TmpArg       := oTmp.Args[0];
        pTmp.Args[0] := oCurrent.Args[1];

        if (isValidOpcode(pTmp^)) Then
        Begin
         if (pTmp.Args[0].Typ = ptStackVal) Then
          pTmp.Args[0].Value -= PushFix;

         Optimized := True;
        End Else
         pTmp.Args[0] := TmpArg;
       End;
      End;
     End;

     { 2-param opcodes }
     if (Length(oTmp.Args) = 2) Then
     Begin
      if (oTmp.Opcode in SafeOpcodes) Then
      Begin
       if (oTmp.Args[0] = oCurrent.Args[0]) Then // yes, optimize! :)
       Begin
        TmpArg       := oTmp.Args[0];
        pTmp.Args[0] := oCurrent.Args[1]; // replace register with pure value

        if (isValidOpcode(pTmp^)) Then
        Begin
         if (pTmp.Args[0].Typ = ptStackVal) Then
          pTmp.Args[0].Value -= PushFix;

         Optimized := True;
        End Else
         pTmp.Args[0] := TmpArg;
       End;
      End;

      if (oTmp.Args[1] = oCurrent.Args[0]) Then // yes, optimize! :)
      Begin
       TmpArg       := oTmp.Args[1];
       pTmp.Args[1] := oCurrent.Args[1]; // replace register with pure value

       if (isValidOpcode(pTmp^)) Then
       Begin
        if (pTmp.Args[1].Typ = ptStackVal) Then
         pTmp.Args[1].Value -= PushFix;

        Optimized := True;
       End Else
        pTmp.Args[1] := TmpArg;
      End;
     End;

     { 3-param opcodes }
     if (Length(oTmp.Args) = 3) Then
     Begin
      // @TODO (arget/arset)
     End;

     Inc(Pos2);
    End;

    if (Optimized) Then
    Begin
     if (not isVariableHolder(pCurrent^.Args[0])) Then
      OpcodeList.Remove(pCurrent); // and remove the first `mov`

     Dec(Pos);
     Continue;
    End;
   End;

   Inc(Pos);
  End;
 End;
End;
End.
