(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit BCCompiler;

 Interface
 Uses Logging, SSCompiler, symdef, Classes, SysUtils, Variants, Opcodes, Tokens, Messages, Zipper, Stream;

 Const bytecode_version_major: uint8 = 0;
       bytecode_version_minor: uint8 = 42;

 { TBCLabel }
 Type TBCLabel =
      Record
       Name    : String;
       Position: uint32;

       isPublic, isFunction: Boolean;
       FunctionSymbol      : TSymbol; // if 'isFunction' equals 'true'
      End;

 { TBCAllocatedSymbol }
 Type TBCAllocatedSymbol =
      Record
       Name    : String;
       Position: uint32;
       Size    : uint8;
      End;

 { TCompiler }
 Type TCompiler =
      Class
       Public { fields }
        BytecodeStream : TStream;
        HeaderStream   : TStream;
        ReferenceStream: TStream;

        Compiler: SSCompiler.TCompiler;
        Bytecode: TWriter;

        LabelList      : Array of TBCLabel;
        AllocSymbolList: Array of TBCAllocatedSymbol;

       Private { methods }
        Function getLabelID(const Name: String): int32;
        Function CreateReference(const Name: String): uint32;
        Function AllocateGlobalVar(const Name: String): uint32;

        Procedure Preparse(const AllocateGlobalVars: Boolean);
        Procedure Parse(const ResolveReferences: Boolean);

       Public { methods }
        Procedure Compile(fCompiler: SSCompiler.TCompiler; SaveAs_SSM: Boolean);
       End;

 Implementation
Uses SSMParser, Serialization;

(* TCompiler.getLabelID *)
Function TCompiler.getLabelID(const Name: String): int32;
Var I: Integer;
Begin
 Result := -1;
 For I := Low(LabelList) To High(LabelList) Do
  if (LabelList[I].Name = Name) Then
   Exit(I);
End;

(* TCompiler.CreateReference *)
Function TCompiler.CreateReference(const Name: String): uint32;
Begin
 Result := ReferenceStream.Position;
 ReferenceStream.write_string(Name);
End;

(* TCompiler.AllocateGlobalVar *)
Function TCompiler.AllocateGlobalVar(const Name: String): uint32;
Var I, Pos, Size: int32;
    Data        : TUnserializer;
Begin
 Pos := 0;

 For I := Low(AllocSymbolList) To High(AllocSymbolList) Do
 Begin
  Inc(Pos, AllocSymbolList[I].Size);

  if (AllocSymbolList[I].Name = Name) Then
   Exit(AllocSymbolList[I].Position);
 End;

 // We need to fetch variable's length from its serialized form.
 Data := TUnserializer.Create(Name);

 Try
  Size := Data.getRoot[1].getInt;
 Finally
  Data.Free;
 End;

 SetLength(AllocSymbolList, Length(AllocSymbolList)+1);
 AllocSymbolList[High(AllocSymbolList)].Name     := Name;
 AllocSymbolList[High(AllocSymbolList)].Position := Pos;
 AllocSymbolList[High(AllocSymbolList)].Size     := Size;
 Result                                          := Pos;
End;

(* TCompiler.Preparse *)
Procedure TCompiler.Preparse(const AllocateGlobalVars: Boolean);
Var I, Q    : uint32;
    Int     : Integer;
    Str, Tmp: String;

    OpcodeLen: LongWord = 0;
Begin
 With Compiler do
 Begin
  if (OpcodeList.Count = 0) Then
   SSCompiler.TCompiler(Compiler).CompileError(eInternalError, ['OpcodeList.Count = 0']);

  OpcodeLen := 0;

  // allocate global variables
  if (AllocateGlobalVars) Then
  Begin
   For I := 0 To OpcodeList.Count-1 Do // each opcode
    With OpcodeList[I]^ do
     if (Length(Args) > 0) Then
      For Q := Low(Args) To High(Args) Do // each argument
       if (Args[Q].Typ = ptSymbolMemRef) Then
        AllocateGlobalVar(Args[Q].Value);

   if (Length(AllocSymbolList) > 0) Then // is there anything to allocate?
   Begin
    DoNotStoreOpcodes := True;

    For I := Low(AllocSymbolList) To High(AllocSymbolList) Do
     For Q := 1 To AllocSymbolList[I].Size Do
      OpcodeList.Insert(0, PutOpcode(o_byte, [0]));

    DoNotStoreOpcodes := False;
   End;
  End;

  // parse opcodes and labels
  For I := 0 To OpcodeList.Count-1 Do
   With OpcodeList[I]^ do
    if (not isComment) and (not isLabel) Then // if opcode
    Begin
     if (Opcode in [o_byte, o_word, o_integer, o_extended]) Then
     Begin
      Case Opcode of
       o_byte    : Inc(OpcodeLen, 1);
       o_word    : Inc(OpcodeLen, 2);
       o_integer : Inc(OpcodeLen, 4);
       o_extended: Inc(OpcodeLen, 10);
      End;

      Continue;
     End;

     Inc(OpcodeLen, sizeof(Byte)); // opcode type
     if (Length(Args) > 0) Then // are there any arguments?
      For Q := Low(Args) To High(Args) Do // for each parameter
       With Args[Q] do
       Begin
        Inc(OpcodeLen, sizeof(Byte)); // parameter type

        Str := VarToStr(Value);

        if (Typ <> ptString) Then
        Begin
         if (Length(Str) > 2) and (Str[1] in [':', '@']) Then
         Begin
          Tmp := Copy(Str, 2, Length(Str));
          if (Copy(Tmp, 1, 10) = '$function.') Then
          Begin
           Delete(Tmp, 1, 10);
           Int := StrToInt(Tmp);
           Tmp := TFunction(Int).LabelName;

           if (Length(Tmp) = 0) Then
            SSCompiler.TCompiler(Compiler).CompileError(eInternalError, ['Couldn''t fetch function''s label name; funcname = '+TSymbol(Int).mFunction.RefSymbol.Name]);

           Str   := Str[1] + Tmp;
           Value := Str;
          End;
         End;

         { label relative address }
         if (Copy(Str, 1, 1) = ':') Then
          Typ := ptInt;

         { label absolute address }
         if (Copy(Str, 1, 1) = '@') Then
         Begin
          Typ   := ptLabelAbsoluteReference;
          Value := CreateReference(Copy(Str, 2, Length(Str))); // remove the beginning `@` char
         End;

         { char }
         if (Copy(Str, 1, 1) = '#') Then
         Begin
          Typ := ptChar;

          Delete(Str, 1, 1);

          if (TryStrToInt(Str, Int)) Then
           Value := Int Else
           Value := 0;
         End;

         { register }
         if (isRegisterName(Str)) Then
         Begin
          Typ   := TPrimaryType(Byte(getRegister(Str).Typ)-Byte(ptBool)); // get register's type
          Value := getRegister(Str).ID;
         End;

         { boolean truth }
         if (Str = 'true') or (Str = 'True') Then
         Begin
          Typ   := ptBool;
          Value := 1;
         End;

         { boolean false }
         if (Str = 'false') or (Str = 'False') Then
         Begin
          Typ   := ptBool;
          Value := 0;
         End;
        End;

        Case Typ of
         ptBoolReg..ptReferenceReg: Inc(OpcodeLen, 1);
         ptBool, ptChar           : Inc(OpcodeLen, 1);
         ptInt                    : Inc(OpcodeLen, 8);
         ptFloat                  : Inc(OpcodeLen, 10);
         ptString                 : Inc(OpcodeLen, Length(VarToStr(Value))+1); // string + terminator char (0x00)
         ptConstantMemRef         : Inc(OpcodeLen, 8);
         ptLabelAbsoluteReference : Inc(OpcodeLen, 8);
         ptSymbolMemRef           : Inc(OpcodeLen, 8);

         else
          Inc(OpcodeLen, 4);
        End;
       End;
    End Else // if label
    if (isLabel) Then
    Begin
     SetLength(LabelList, Length(LabelList)+1);
     LabelList[High(LabelList)].Name       := Name;
     LabelList[High(LabelList)].Position   := OpcodeLen;
     LabelList[High(LabelList)].isPublic   := isPublic;
     LabelList[High(LabelList)].isFunction := isFunction;

     if (isFunction) Then
      LabelList[High(LabelList)].FunctionSymbol := FunctionSymbol;
    End;
 End;
End;

(* TCompiler.Parse *)
Procedure TCompiler.Parse(const ResolveReferences: Boolean);
Var OpcodeBegin: LongWord;
    Opcode     : PMOpcode;
    Tmp        : int32;
    Arg        : int8;

    Str: String;
    Int: int32;
Begin
 With BytecodeStream do
 Begin
  For Opcode in Compiler.OpcodeList Do
   With Opcode^ do
   Begin
    OpcodeBegin := BytecodeStream.Position;

    if (isLabel) or (isComment) Then // skip labels and comments
     Continue;

    if (Opcode in [o_byte, o_word, o_integer, o_extended]) Then // special opcodes
    Begin
     Case Opcode of
      o_byte    : write_uint8(Args[0].Value);
      o_word    : write_uint16(Args[0].Value);
      o_integer : write_int32(Args[0].Value);
      o_extended: write_float(Args[0].Value);
     End;

     Continue;
    End;

    write_uint8(ord(Opcode)); // write opcode type

    For Arg := Low(Args) To High(Args) Do // write each argument
    Begin
     With Args[Arg] do
     Begin
      if (Typ = ptLabelAbsoluteReference) Then
      Begin
       { label absolute references }
       if (ResolveReferences) Then
       Begin
        if ((VarType(Value) and VarTypeMask) in [varInteger, varLongword, varInt64]) Then
        Begin
         ReferenceStream.Position := Value;
         Value                    := ReferenceStream.read_string;
        End;

        Tmp := getLabelID(Value); // find the reference

        if (Tmp = -1) Then // not found!
        Begin
         if (Token = nil) Then
          self.Compiler.CompileError(eLinker_UnknownReference, [VarToStr(Value)]) Else
          self.Compiler.CompileError(Token, eLinker_UnknownReference, [VarToStr(Value)]);
         Exit;
        End;

        // found!
        Case Typ of
         ptLabelAbsoluteReference: Value := LabelList[Tmp].Position;

         else
          raise Exception.Create('This should NOT happen! Check your toaster''s firmware and contact your doctor.');
        End;

        Typ := ptInt;
       End Else
       Begin
        if ((VarType(Value) and VarTypeMask) = varString) Then
         Value := CreateReference(Value); // add new reference
       End;
      End;

      if (Typ = ptSymbolMemRef) Then
      Begin
       { symbol memory ('&$symbol') references }
       if (ResolveReferences) Then
       Begin
        Typ   := ptConstantMemRef;
        Value := AllocateGlobalVar(Value);
       End Else
       Begin
        Value := CreateReference(Value); // add new reference
       End;
      End;

      if (Typ = ptInt) and (Copy(Value, 1, 1) = ':') Then
      Begin
       { resolve label relative address }
       Str := VarToStr(Value);

       Delete(Str, 1, 1); // remove `:`

       Int := getLabelID(Str);

       With SSCompiler.TCompiler(Compiler) do
        if (Int = -1) Then // label not found
        Begin
         if (Token = nil) Then
          SSCompiler.TCompiler(Compiler).CompileError(eBytecode_LabelNotFound, [Str]) Else
          SSCompiler.TCompiler(Compiler).CompileError(Token, eBytecode_LabelNotFound, [Str]);

         Value := 0;
        End Else // label found
         Value := Int64(LabelList[Int].Position)-OpcodeBegin; // jumps have to be relative against the current opcode
      End;

      Try
       write_uint8(ord(Typ)); // write parameter type

       Case Typ of // write parameter value
        ptBoolReg..ptReferenceReg: write_uint8(Value);
        ptBool, ptChar           : write_uint8(Value);
        ptInt                    : write_int64(Value);
        ptFloat                  : write_float(Value);
        ptString                 : write_string(Value);
        ptConstantMemRef         : write_int64(Value);
        ptSymbolMemRef           : write_int64(Value);
        ptLabelAbsoluteReference : write_int64(Value);

        else
         write_int32(Value);
       End;
      Except
       self.Compiler.CompileError(eInternalError, ['Cannot compile opcode; not a numeric parameter value: `'+VarToStr(Value)+'`']);
      End;
     End;
    End;
   End;
 End;
End;

(* TCompiler.Compile *)
Procedure TCompiler.Compile(fCompiler: SSCompiler.TCompiler; SaveAs_SSM: Boolean);
Var Zip   : TZipper;
    Output: String;

    { AddFile }
    Procedure AddFile(const Stream: TStream; FileName: String);
    Begin
     Stream.SaveToFile(Output+FileName);
     Zip.Entries.AddFileEntry(Output+FileName, FileName);
    End;

Begin
 Compiler := fCompiler;

 Output := Compiler.OutputFile;

 HeaderStream    := TStream.Create;
 ReferenceStream := TStream.Create;
 BytecodeStream  := TStream.Create;
 Zip             := TZipper.Create;

 Preparse(not SaveAs_SSM);

 Try
  Parse(not SaveAs_SSM); // resolve references only when compiling to a program

  // save header
  With HeaderStream do
  Begin
   write_uint32($0DEFACED);

   if (SaveAs_SSM) Then
    write_uint8(0) { not runnable } else
    write_uint8(1) { runnable };

   write_uint8(bytecode_version_major);
   write_uint8(bytecode_version_minor);
  End;

  Log('Header size: '+IntToStr(HeaderStream.Size)+' bytes');
  Log('References data size: '+IntToStr(ReferenceStream.Size)+' bytes');
  Log('Bytecode size: '+IntToStr(BytecodeStream.Size)+' bytes');

  if (SaveAs_SSM) Then // save as a library?
  Begin
   With TSSMWriter.Create(Compiler.OutputFile, fCompiler, self) do
   Begin
    Save;
    Free;
   End;

   Exit;
  End;

  { make zip archive }
  AddFile(HeaderStream, '.header');
  AddFile(BytecodeStream, '.bytecode');

  { save it }
  Zip.FileName := Output;
  Zip.ZipAllFiles;
 Finally
  BytecodeStream.Free;
  Zip.Free;

  DeleteFile(Output+'.header');
  DeleteFile(Output+'.bytecode');
 End;
End;

End.
