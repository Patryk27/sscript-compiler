(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Compile2;

 Interface
 Uses CompilerUnit, Compile1, symdef, Classes, SysUtils, Variants, Opcodes, Tokens, Messages, Zipper, Stream;

 Const bytecode_version_major = 0;
       bytecode_version_minor = 41;

 // TLabel
 Type TLabel = Record
                Name    : String;
                Position: LongWord;

                isPublic: Boolean;
               End;

 // TCompiler
 Type TCompiler = Class
                   Public
                    BytecodeStream : TStream;
                    HeaderStream   : TStream;
                    ReferenceStream: TStream;

                    Compiler: Compile1.TCompiler;
                    Bytecode: TWriter;

                    LabelList: Array of TLabel;

                   Private
                    Function getLabelID(const Name: String): Integer;
                    Function CreateReference(const Name: String): LongWord;

                    Procedure Preparse;
                    Procedure Parse(const ResolveReferences: Boolean);

                   Public
                    Procedure Compile(fCompiler: Compile1.TCompiler; SaveAs_SSM: Boolean);
                   End;

 Implementation
Uses SSM_parser;

(* TCompiler.getLabelID *)
Function TCompiler.getLabelID(const Name: String): Integer;
Var I: Integer;
Begin
 Result := -1;
 For I := Low(LabelList) To High(LabelList) Do
  if (LabelList[I].Name = Name) Then
   Exit(I);
End;

(* TCompiler.CreateReference *)
Function TCompiler.CreateReference(const Name: String): LongWord;
Begin
 Result := ReferenceStream.Position;
 ReferenceStream.write_string(Name);
End;

(* TCompiler.Preparse *)
Procedure TCompiler.Preparse;
Var I, Q    : LongWord;
    Int     : Integer;
    Str, Tmp: String;

    OpcodeLen: LongWord = 0;
Begin
 With Compiler do
 Begin
  if (OpcodeList.Count = 0) Then
   Compile1.TCompiler(Compiler).CompileError(eInternalError, ['OpcodeList.Count = 0']);

  // search for strings in opcodes
  For I := 0 To OpcodeList.Count-1 Do
   With OpcodeList[I]^ do
    if (not isComment) and (not isLabel) and (Length(Args) > 0) Then
     For Q := Low(Args) To High(Args) Do
      With Args[Q] do
      Begin
       Str := VarToStr(Value);

       { string }
       if (Copy(Str, 1, 1) = '"') Then
       Begin
        Typ := ptString;

        Delete(Str, 1, 1);
        Delete(Str, Length(Str), 1);

        Value := Str;
       End;
      End;

  // parse opcodes and labels
  OpcodeLen := 0;

  For I := 0 To OpcodeList.Count-1 Do
   With OpcodeList[I]^ do
    if (not isComment) and (not isLabel) Then // if opcode
    Begin
     if (Opcode in [o_byte, o_word, o_integer, o_extended]) Then
     Begin
      Case Opcode of
       o_byte: Inc(OpcodeLen, sizeof(Byte));
       o_word: Inc(OpcodeLen, sizeof(Word));
       o_integer: Inc(OpcodeLen, sizeof(Integer));
       o_extended: Inc(OpcodeLen, sizeof(Extended));
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
           Tmp := TSymbol(Int).mFunction.MangledName;

           if (Length(Tmp) = 0) Then
            Compile1.TCompiler(Compiler).CompileError(eInternalError, ['Couldn''t fetch function''s label name; funcname = '+TSymbol(Int).mFunction.RefSymbol.Name]);

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
          Value := CreateReference(Copy(Str, 2, Length(Str))); // remove beginning `@` char
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
          Typ   := TPrimaryType(Byte(getRegister(Str).Typ)-Byte(ptBool)); // get register type
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
         ptBoolReg..ptReferenceReg: Inc(OpcodeLen, sizeof(Byte));
         ptBool, ptChar           : Inc(OpcodeLen, sizeof(Byte));
         ptInt                    : Inc(OpcodeLen, sizeof(Int64));
         ptFloat                  : Inc(OpcodeLen, sizeof(Extended));
         ptString                 : Inc(OpcodeLen, Length(VarToStr(Value))+sizeof(Byte)); // string + terminator char (0x00)
         ptLabelAbsoluteReference : Inc(OpcodeLen, sizeof(Int64));

         else
          Inc(OpcodeLen, sizeof(Integer));
        End;
       End;
    End Else // if label
    if (isLabel) Then
    Begin
     SetLength(LabelList, Length(LabelList)+1);
     LabelList[High(LabelList)].Name     := Name;
     LabelList[High(LabelList)].Position := OpcodeLen;
     LabelList[High(LabelList)].isPublic := isPublic;
    End;
 End;
End;

(* TCompiler.Parse *)
Procedure TCompiler.Parse(const ResolveReferences: Boolean);
Var OpcodeBegin: LongWord;
    Opcode     : PMOpcode;
    Arg        : TMOpcodeArg;
    Tmp        : Integer;

    Str: String;
    Int: Integer;

    Func: TFunction;
Begin
 With BytecodeStream do
 Begin
  For Opcode in Compiler.OpcodeList Do
   With Opcode^ do
   Begin
    OpcodeBegin := BytecodeStream.Position;

    if (isLabel) or (isComment) Then // skip comments
     Continue;

    if (Opcode in [o_byte, o_word, o_integer, o_extended]) Then // special opcodes
    Begin
     Case Opcode of
      o_byte: write_byte(Args[0].Value);
      o_word: write_word(Args[0].Value);
      o_integer: write_integer(Args[0].Value);
      o_extended: write_float(Args[0].Value);
     End;

     Continue;
    End;

    write_byte(ord(Opcode)); // opcode type
    For Arg in Args Do
     With Arg do
     Begin
      if (Typ = ptLabelAbsoluteReference) Then // is a references?
      Begin
       if (ResolveReferences) Then // should be resolved?
       Begin
        if ((VarType(Value) and VarTypeMask) in [varInteger, varLongword, varInt64]) Then
        Begin
         ReferenceStream.Position := Value;
         Value                    := ReferenceStream.read_string;
        End;

        Tmp := getLabelID(Value); // find reference
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
        if ((VarType(Value) and VarTypeMask) = varString) Then
         Value := CreateReference(Value);
      End;

      if (Typ = ptInt) and (Copy(Value, 1, 1) = ':') Then // resolve label relative address
      Begin
       Str := VarToStr(Value);

       Delete(Str, 1, 1); // remove `:`

       Int := getLabelID(Str);

       With Compile1.TCompiler(Compiler) do
        if (Int = -1) Then // label not found
        Begin
         Func := findFunctionByLabel(Str);

         if (Func <> nil) Then
         Begin
          CompileError(Func.RefSymbol.DeclToken, eFunctionNotFound, [Func.RefSymbol.Name, Func.LibraryFile]);
         End Else
         Begin
          if (Token = nil) Then
           Compile1.TCompiler(Compiler).CompileError(eBytecode_LabelNotFound, [Str]) Else
           Compile1.TCompiler(Compiler).CompileError(Token, eBytecode_LabelNotFound, [Str]);
         End;

         Value := 0;
        End Else // label found
         Value := Int64(LabelList[Int].Position)-OpcodeBegin; // jump have to be relative against the current opcode
      End;

      Try
       write_byte(ord(Typ)); // param type
       Case Typ of // param value
        ptBoolReg..ptReferenceReg: write_byte(Value);
        ptBool, ptChar           : write_byte(Value);
        ptInt                    : write_int64(Value);
        ptFloat                  : write_float(Value);
        ptString                 : write_string(Value);
        ptLabelAbsoluteReference : write_int64(Value);

        else
         write_integer(Value);
       End;
      Except
       self.Compiler.CompileError(eInternalError, ['Cannot compile opcode; not a numeric parameter value: `'+VarToStr(Value)+'`']);
      End;
     End;
   End;
 End;
End;

(* TCompiler.Compile *)
Procedure TCompiler.Compile(fCompiler: Compile1.TCompiler; SaveAs_SSM: Boolean);
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

 Preparse;

 Try
  Parse(not SaveAs_SSM); // resolve references only when compiling to a program

  // save header
  With HeaderStream do
  Begin
   write_longword($0DEFACED);

   if (SaveAs_SSM) Then
    write_byte(0) { not runnable } else
    write_byte(1) { runnable };

   write_byte(bytecode_version_major);
   write_byte(bytecode_version_minor);
  End;

  Log('Header size: '+IntToStr(HeaderStream.Size)+' bytes');
  Log('References data size: '+IntToStr(ReferenceStream.Size)+' bytes');
  Log('Bytecode size: '+IntToStr(BytecodeStream.Size)+' bytes');

  if (SaveAs_SSM) Then // save as a library?
  Begin
   TSSM.Create.Save(Compiler.OutputFile, fCompiler, self).Free;
   Exit;
  End;

 // if (ReferenceStream.Size <> 0) Then
 //  Compiler.CompileError(eInternalError, ['Output program file contains external references!']);

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
