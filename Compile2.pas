(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Compile2;

 Interface
 Uses CompilerUnit, Compile1, Classes, SysUtils, Variants, MTypes, Opcodes, Messages, Zipper, Stream;

 // TLabel
 Type TLabel = Record
                Name    : String;
                Position: LongWord;

                isFunction: Boolean;
               End;

 // TCompiler
 Type TCompiler = Class
                   Public
                    BytecodeStream: TStream;
                    HeaderStream  : TStream;

                    Compiler: Compile1.TCompiler;
                    Bytecode: TWriter;

                    LabelList: Array of TLabel;

                   Private
                    Function getLabelID(Name: String): Integer;

                    Procedure Preparse;
                    Procedure Parse(const ResolveReferences: Boolean);

                   Public
                    Procedure Compile(fCompiler: Compile1.TCompiler; SaveAs_SSM: Boolean);
                   End;

 Implementation
Uses SSM_parser;

{ TCompiler.getLabelID }
Function TCompiler.getLabelID(Name: String): Integer;
Var I: Integer;
Begin
 Result := -1;
 For I := Low(LabelList) To High(LabelList) Do
  if (LabelList[I].Name = Name) Then
   Exit(I);
End;

{ TCompiler.Preparse }
Procedure TCompiler.Preparse;
Var I, Q: LongWord;
    Int : Integer;
    Str : String;

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
       End;
      End;

  // parse opcodes and labels
  SetLength(LabelList, 0);
  OpcodeLen := 0;

  For I := 0 To OpcodeList.Count-1 Do
   With OpcodeList[I]^ do
    if (not isComment) and (not isLabel) Then // if opcode
    Begin
     Pos := OpcodeLen;

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
         { label relative address }
         if (Copy(Str, 1, 1) = ':') Then
          Typ := ptInt;

         { label absolute address }
         if (Copy(Str, 1, 1) = '@') Then
         Begin
          Typ   := ptLabelAbsoluteReference;
          Value := Copy(Str, 2, Length(Str)); // remove beginning `@` char
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
         ptBool, ptChar: Inc(OpcodeLen, sizeof(Byte));
         ptFloat: Inc(OpcodeLen, sizeof(Extended));
         ptString: Inc(OpcodeLen, Length(VarToStr(Value))+sizeof(Byte)); // string + terminator char (0x00)
         else Inc(OpcodeLen, sizeof(Integer));
        End;

        {if (EmitSSMLabelOpcode) Then
         if (Typ = ptLabelAbsoluteReference) Then
          Inc(OpcodeLen, Length(VarToStr(Value))+sizeof(Byte)) Else
          Inc(OpcodeLen, sizeof(Integer));}
       End;
    End Else // if label
    if (isLabel) Then
    Begin
     SetLength(LabelList, Length(LabelList)+1);
     LabelList[High(LabelList)].Name       := Name;
     LabelList[High(LabelList)].Position   := OpcodeLen;
     LabelList[High(LabelList)].isFunction := isFunctionBeginLabel;
    End;
 End;
End;

{ TCompiler.Parse }
Procedure TCompiler.Parse(const ResolveReferences: Boolean);
Var OpcodeBegin: LongWord;
    Opcode     : PMOpcode;
    Arg        : TMOpcodeArg;
    Tmp        : Integer;

    Str: String;
    Int: Integer;

    FuncID, Namespace: Integer;

Label LabelNotFound;
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
      if (Typ = ptLabelAbsoluteReference) and (ResolveReferences) Then // resolve reference?
      Begin
       Tmp := getLabelID(VarToStr(Value)); // find reference
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
         raise Exception.Create('This shouldn''t happen! Contact your doctor and check your toaster''s firmware...');
       End;

       Typ := ptInt;
      End;

      if (Typ = ptInt) and (Copy(Value, 1, 1) = ':') Then // resolve label relative address (it's not a label reference!)
      Begin
       Str := VarToStr(Value);

       Delete(Str, 1, 1); // remove `:`
       Int := getLabelID(Str);

       if (Int = -1) Then // label not found
       Begin
        if (Copy(Str, 1, 11) = '__function_') Then // function label not found
        Begin
         With Compile1.TCompiler(Compiler) do
         Begin
          findFunctionByLabel(Str, FuncID, Namespace);
          if (FuncID = -1) Then
           goto LabelNotFound;

          With Compile1.TCompiler(Compiler) do
           With NamespaceList[Namespace].SymbolList[FuncID].mFunction do
            CompileError(DeclToken, eFunctionNotFound, [Name, LibraryFile]);
         End;
        End Else // just some label not found
        Begin
        LabelNotFound:
         if (Token = nil) Then
          Compile1.TCompiler(Compiler).CompileError(eBytecode_LabelNotFound, [Str]) Else
          Compile1.TCompiler(Compiler).CompileError(Token^, eBytecode_LabelNotFound, [Str]);
        End;

        Value := 0;
       End Else // label found
        Value := Int64(LabelList[Int].Position)-OpcodeBegin; // jump have to be relative against the current opcode
      End;

      Try
       write_byte(ord(Typ)); // param type
       Case Typ of // param value
        ptBoolReg..ptReferenceReg: write_byte(Value);
        ptBool, ptChar: write_byte(Value);
        ptFloat: write_float(Value);
        ptString, ptLabelAbsoluteReference: write_string(Value);
        else write_integer(Value);
       End;
      Except
       self.Compiler.CompileError(eInternalError, ['Cannot compile opcode; not a numeric value: `'+VarToStr(Value)+'`']);
      End;
     End;
   End;
 End;
End;

{ TCompiler.Compile }
Procedure TCompiler.Compile(fCompiler: Compile1.TCompiler; SaveAs_SSM: Boolean);
Var Zip   : TZipper;
    Output: String;

    // AddFile
    Procedure AddFile(const Stream: TStream; FileName: String);
    Begin
     Stream.SaveToFile(Output+FileName);
     Zip.Entries.AddFileEntry(Output+FileName, FileName);
    End;

Begin
 Compiler := fCompiler;

 Preparse;

 Output := Compiler.OutputFile;

 BytecodeStream := TStream.Create;
 HeaderStream   := TStream.Create;
 Zip            := TZipper.Create;

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

  if (SaveAs_SSM) Then // save as a library?
  Begin
   TSSM.Create.Save(Compiler.OutputFile, fCompiler, self).Free;
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
