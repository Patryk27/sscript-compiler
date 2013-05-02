(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Opcodes;

 Interface
 Uses Tokens;

 Type TPrimaryType = (ptNone=-3, ptAny=-2, ptAnyReg=-1, // not emmited into bytecode (only for compiler internal usage)
                      ptBoolReg=0, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg,
                      ptBool, ptChar, ptInt, ptFloat, ptString, ptStackVal,
                      ptLabelAbsoluteReference);

 Const PrimaryTypeNames: Array[TPrimaryType] of String =
                     ('none', 'any', 'any reg',
                      'bool reg', 'char reg', 'int reg', 'float reg', 'string reg', 'reference reg',
                      'bool', 'char', 'int', 'float', 'string', 'stackval',
                      'label absolute reference');

 Type TRegister = Record
                   Name: String;
                   ID  : Integer;
                   Typ : TPrimaryType; 
                  End;

 Const RegisterCount = 26;
 Const RegisterList: Array[0..RegisterCount-1] of TRegister =
 (
  (* ===== BOOLEAN ===== *)
  (Name: 'eb1'; ID: 1; Typ: ptBool),
  (Name: 'eb2'; ID: 2; Typ: ptBool),
  (Name: 'eb3'; ID: 3; Typ: ptBool),
  (Name: 'eb4'; ID: 4; Typ: ptBool),
  (Name: 'if' ; ID: 5; Typ: ptBool),

  (* ===== CHAR ===== *)
  (Name: 'ec1'; ID: 1; Typ: ptChar),
  (Name: 'ec2'; ID: 2; Typ: ptChar),
  (Name: 'ec3'; ID: 3; Typ: ptChar),
  (Name: 'ec4'; ID: 4; Typ: ptChar),

  (* ===== INTEGER ===== *)
  (Name: 'ei1'; ID: 1; Typ: ptInt),
  (Name: 'ei2'; ID: 2; Typ: ptInt),
  (Name: 'ei3'; ID: 3; Typ: ptInt),
  (Name: 'ei4'; ID: 4; Typ: ptInt),
  (Name: 'stp'; ID: 5; Typ: ptInt),
   
  (* ===== FLOAT ===== *)
  (Name: 'ef1'; ID: 1; Typ: ptFloat),
  (Name: 'ef2'; ID: 2; Typ: ptFloat),
  (Name: 'ef3'; ID: 3; Typ: ptFloat),
  (Name: 'ef4'; ID: 4; Typ: ptFloat),
   
  (* ===== STRING ===== *)
  (Name: 'es1'; ID: 1; Typ: ptString),
  (Name: 'es2'; ID: 2; Typ: ptString),
  (Name: 'es3'; ID: 3; Typ: ptString),
  (Name: 'es4'; ID: 4; Typ: ptString),

  (* ===== REFERENCE ===== *)
  (Name: 'er1'; ID: 1; Typ: ptInt),
  (Name: 'er2'; ID: 2; Typ: ptInt),
  (Name: 'er3'; ID: 3; Typ: ptInt),
  (Name: 'er4'; ID: 4; Typ: ptInt)
 );

 Type TOpcode = Record
                 Name  : String;
                 ParamC: Integer;
                 ParamT: Array[0..2] of TPrimaryType;
                End;

 Type TOpcode_E = (o_nop, o_stop,
                   o_push, o_pop,
                   o_add, o_sub, o_mul, o_div, o_neg, o_mov,
                   o_jmp, o_tjmp, o_fjmp, o_call, o_icall, o_acall, o_ret,
                   o_if_e, o_if_ne, o_if_g, o_if_l, o_if_ge, o_if_le,
                   o_strjoin,
                   o_not, o_or, o_xor, o_and, o_shl, o_shr,
                   o_mod,
                   o_arset, o_arget, o_arcrt, o_arlen,
                   o_objfree,
                   o_loc_file, o_loc_func, o_loc_line,
                   o_byte, o_word, o_integer, o_extended);

 // TMOpcodeArg
 Type TMOpcodeArg = Record
                     Typ  : TPrimaryType; // parameter type (register, integer value etc.)
                     Value: Variant;
                    End;

 // TMOpcode
 Type PMOpcode = ^TMOpcode;
      TMOpcode = Record
                  Name  : String;
                  Opcode: TOpcode_E;
                  Args  : Array of TMOpcodeArg;

                  isLabel  : Boolean;
                  isComment: Boolean;

                  isPublic: Boolean;

                  Token   : PToken_P;
                  Compiler: Pointer;
                 End;

 Const OpcodeList: Array[0..ord(High(TOpcode_E))] of TOpcode =
 (
  (* ====== NOP ====== *)
  (Name: 'nop'; ParamC: 0; ParamT: (ptNone, ptNone, ptNone)),

  (* ===== STOP () ===== *)
  (Name: 'stop'; ParamC: 0; ParamT: (ptNone, ptNone, ptNone)),

  (* ====== PUSH (value) ====== *)
  (Name: 'push'; ParamC: 1; ParamT: (ptAny, ptNone, ptNone)),
                                                                                      
  (* ====== POP (register) ====== *)
  (Name: 'pop'; ParamC: 1; ParamT: (ptAnyReg, ptNone, ptNone)),

  (* ===== ADD (int/float reg, int/float value) ===== *)
  (Name: 'add'; ParamC: 2; ParamT: (ptAnyReg, ptFloat, ptNone)),

  (* ==== SUB (int/float reg, int/float value) ===== *)
  (Name: 'sub'; ParamC: 2; ParamT: (ptAnyReg, ptFloat, ptNone)),

  (* ===== MUL (int/float reg, int/float value) ===== *)
  (Name: 'mul'; ParamC: 2; ParamT: (ptAnyReg, ptFloat, ptNone)),

  (* ===== DIV (int/float reg, int/float value) ===== *)
  (Name: 'div'; ParamC: 2; ParamT: (ptAnyReg, ptFloat, ptNone)),

  (* ===== NEG (value) ===== *)
  (Name: 'neg'; ParamC: 1; ParamT: (ptAnyReg, ptNone, ptNone)),

  (* ===== MOV (dest, src) ===== *)
  (Name: 'mov'; ParamC: 2; ParamT: (ptAnyReg, ptAny, ptNone)),

  (* ===== JUMP (name/id) ===== *)
  (Name: 'jmp'; ParamC: 1; ParamT: (ptInt, ptNone, ptNone)),
  (Name: 'tjmp'; ParamC: 1; ParamT: (ptInt, ptNone, ptNone)),
  (Name: 'fjmp'; ParamC: 1; ParamT: (ptInt, ptNone, ptNone)),

  (* ===== CALL (name/id) ===== *)
  (Name: 'call'; ParamC: 1; ParamT: (ptInt, ptNone, ptNone)),

  (* ===== ICALL (name) ===== *)
  (Name: 'icall'; ParamC: 1; ParamT: (ptString, ptNone, ptNone)),

  (* ===== ACALL (position) ===== *)
  (Name: 'acall'; ParamC: 1; ParamT: (ptInt, ptNone, ptNone)),

  (* ===== RET () ===== *)
  (Name: 'ret'; ParamC: 0; ParamT: (ptNone, ptNone, ptNone)),

  (* ===== IF (a, b) ===== *)
  (Name: 'if_e'; ParamC: 2; ParamT: (ptAny, ptAny, ptNone)),
  (Name: 'if_ne'; ParamC: 2; ParamT: (ptAny, ptAny, ptNone)),
  (Name: 'if_g'; ParamC: 2; ParamT: (ptAny, ptAny, ptNone)),
  (Name: 'if_l'; ParamC: 2; ParamT: (ptAny, ptAny, ptNone)),
  (Name: 'if_ge'; ParamC: 2; ParamT: (ptAny, ptAny, ptNone)),
  (Name: 'if_le'; ParamC: 2; ParamT: (ptAny, ptAny, ptNone)),

  (* ===== STRJOIN (string reg, string) ===== *)
  (Name: 'strjoin'; ParamC: 2; ParamT: (ptStringReg, ptString, ptNone)),

  (* ===== NOT (reg) ===== *)
  (Name: 'not'; ParamC: 1; ParamT: (ptAnyReg, ptNone, ptNone)),

  (* ===== OR (reg, val) ===== *)
  (Name: 'or'; ParamC: 2; ParamT: (ptAnyReg, ptAny, ptNone)),

  (* ===== XOR (reg, val) ===== *)
  (Name: 'xor'; ParamC: 2; ParamT: (ptAnyReg, ptAny, ptNone)),

  (* ===== AND (reg, val) ===== *)
  (Name: 'and'; ParamC: 2; ParamT: (ptAnyReg, ptAny, ptNone)),

  (* ===== SHL (reg, val) ===== *)
  (Name: 'shl'; ParamC: 2; ParamT: (ptAnyReg, ptInt, ptNone)),

  (* ===== SHR (reg, val) ===== *)
  (Name: 'shr'; ParamC: 2; ParamT: (ptAnyReg, ptInt, ptNone)),

  (* ===== MOD () ===== *)
  (Name: 'mod'; ParamC: 2; ParamT: (ptAnyReg, ptAny, ptNone)),

  (* ===== ARSET (refreg, indexes count, value) ===== *)
  (Name: 'arset'; ParamC: 3; ParamT: (ptReferenceReg, ptInt, ptAny)),

  (* ===== ARGET (refreg, indexes count, out reg) ===== *)
  (Name: 'arget'; ParamC: 3; ParamT: (ptReferenceReg, ptInt, ptAnyReg)),

  (* ===== ARCRT (refreg, primary type id, array dim count) ===== *)
  (Name: 'arcrt'; ParamC: 3; ParamT: (ptReferenceReg, ptInt, ptInt)),

  (* ===== ARLEN (refreg, dimension, out reg) ===== *)
  (Name: 'arlen'; ParamC: 3; ParamT: (ptReferenceReg, ptInt, ptIntReg)),

  (* ===== OBJFREE (refreg) ===== *)
  (Name: 'objfree'; ParamC: 1; ParamT: (ptReferenceReg, ptNone, ptNone)),

  (* ===== LOC_FILE (string) ===== *)
  (Name: 'loc_file'; ParamC: 1; ParamT: (ptString, ptNone, ptNone)),

  (* ===== LOC_FUNC (string) ===== *)
  (Name: 'loc_func'; ParamC: 1; ParamT: (ptString, ptNone, ptNone)),

  (* ===== LOC_LINE (int) ===== *)
  (Name: 'loc_line'; ParamC: 1; ParamT: (ptInt, ptNone, ptNone)),

  (* ===== ===== ===== ===== *)
  (Name: 'db'; ParamC: 1; ParamT: (ptInt, ptNone, ptNone)),
  (Name: 'dw'; ParamC: 1; ParamT: (ptInt, ptNone, ptNone)),
  (Name: 'di'; ParamC: 1; ParamT: (ptInt, ptNone, ptNone)),
  (Name: 'de'; ParamC: 1; ParamT: (ptFloat, ptNone, ptNone))
 );

 Operator = (A, B: TMOpcodeArg): Boolean;
 Function isValidOpcode(O: TMOpcode): Boolean;
 Function GetOpcodeID(Name: String): Integer;
 Function isRegisterName(Name: String): Boolean;
 Function getRegister(Name: String): TRegister;

 Implementation

Operator = (A, B: TMOpcodeArg): Boolean;
Begin
 Result := (A.Typ = B.Typ) and (A.Value = B.Value);
End;

{ OpcodeTypeCheck }
Function OpcodeTypeCheck(A, B: TPrimaryType): Boolean;
Begin
 Result := (A=B);

 if (B = ptNone) Then
  Exit(True);

 Case A of
  ptAny, ptStackVal    : Result := True;
  ptAnyReg             : Result := B in [ptStackVal, ptBoolReg, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg];
  ptBool, ptBoolReg    : Result := B in [ptStackVal, ptBool, ptBoolReg, ptInt, ptIntReg];
  ptChar               : Result := B in [ptStackVal, ptChar, ptCharReg, ptInt, ptIntReg];
  ptCharReg            : Result := B in [ptStackVal, ptChar, ptCharReg, ptInt, ptIntReg, ptString, ptStringReg];
  ptInt, ptIntReg      : Result := B in [ptStackVal, ptInt, ptIntReg, ptChar, ptCharReg, ptFloat, ptFloatReg];
  ptFloat, ptFloatReg  : Result := B in [ptStackVal, ptInt, ptIntReg, ptFloat, ptFloatReg];
  ptString, ptStringReg: Result := B in [ptStackVal, ptString, ptStringReg, ptChar, ptCharReg];
  ptReferenceReg       : Result := B in [ptStackVal, ptInt, {ptString,} ptStringReg, ptReferenceReg];
 End;
End;

{ CheckMOV }
Function CheckMOV(A, B: TPrimaryType): Boolean;
Begin
 Result := OpcodeTypeCheck(A, B);
End;

{ isValidOpcode }
Function isValidOpcode(O: TMOpcode): Boolean;
Var ID, I : Integer;
Begin
 if (O.isLabel) or (O.isComment) Then
  Exit(True);

 Result := False;
 ID     := ord(O.Opcode);

 if (ID < 0) or (ID > High(OpcodeList)) Then // invalid opcode
  Exit;

 if (Length(O.Args) <> OpcodeList[ID].ParamC) Then // wrong param count
  Exit;

 if (O.Opcode = o_mov) Then
 Begin
  Exit(CheckMOV(O.Args[0].Typ, O.Args[1].Typ) and (O.Args[0].Typ in [ptStackVal, ptBoolReg, ptCharReg, ptIntReg, ptFloatReg, ptStringReg, ptReferenceReg]));
 End Else
 Begin
  For I := 0 To High(O.Args) Do
   if (not OpcodeTypeCheck(OpcodeList[ID].ParamT[I], O.Args[I].Typ)) Then // wrong param type
    Exit;
 End;

 Exit(True);
End;

{ GetOpcodeID }
Function GetOpcodeID(Name: String): Integer;
Var I: Integer;
Begin
 Result := -1;
 For I := Low(OpcodeList) To High(OpcodeList) Do
  if (OpcodeList[I].Name = Name) Then
  Begin
   Result := I;
   Exit;
  End;
End;

{ isRegisterName }
Function isRegisterName(Name: String): Boolean;
Var I: Integer;
Begin
 Result := False;
 For I := 0 To High(RegisterList) Do
  if (RegisterList[I].Name = Name) Then
  Begin
   Result := True;
   Exit;
  End;
End;

{ getRegister }
Function getRegister(Name: String): TRegister;
Var I: Integer;
Begin
 For I := 0 To High(RegisterList) Do
  if (RegisterList[I].Name = Name) Then
  Begin
   Result := RegisterList[I];
   Exit;
  End;
End;
End.
