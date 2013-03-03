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
               End;

 // TCompiler
 Type TCompiler = Class
                   Public
                    BytecodeStream: TStream;
                    HeaderStream  : TStream;

                    Compiler: Compile1.TCompiler;
                    Bytecode: TWriter;

                    LabelList: Array of TLabel;

                    Function getLabelID(Name: String): Integer;

                   Private
                    Procedure Preparse;
                    Procedure Preparse2;

                    Procedure Parse;

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

       { inline string }
       if (Copy(Str, 1, 1) = '"') Then
       Begin
        Typ := ptString;

        Delete(Str, 1, 1);
        Delete(Str, Length(Str), 1);
       End;

       { string reference }
       if (Copy(Str, 1, 8) = 'string__') Then
       Begin
        Typ := ptString;
        Int := self.Compiler.findStringByName(Str);
        if (Int = -1) Then
        Begin
         Compile1.TCompiler(Compiler).CompileError(Token^, eBytecode_StringNotFound, [Str]);
         Value := '';
        End Else
         Value := self.Compiler.StringList[Int].Value;
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
         { label address }
         if ((Copy(Str, 1, 1) = ':') or (Copy(Str, 1, 1) = '@')) Then
          Typ := ptInt;

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
         ptBool: Inc(OpcodeLen, sizeof(Byte));
         ptChar: Inc(OpcodeLen, sizeof(Byte));
         ptFloat: Inc(OpcodeLen, sizeof(Extended));
         ptString: Inc(OpcodeLen, Length(Str)+sizeof(Byte));
         else Inc(OpcodeLen, sizeof(Integer));
        End;
       End;
    End Else // if label
    if (isLabel) Then
    Begin
     SetLength(LabelList, Length(LabelList)+1);
     LabelList[High(LabelList)].Name     := Name;
     LabelList[High(LabelList)].Position := OpcodeLen;
    End;
 End;
End;

{ TCompiler.Preparse2 }
Procedure TCompiler.Preparse2;
Var I, Q: LongWord;
    Int : Integer;
    Str : String;

    OpcodeLen: Longword;

    FuncID, Namespace: Integer;

Label LabelNotFound;
Begin
 { from here, we have already parsed each label }

 With Compiler do
 Begin
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
         Begin
          Delete(Str, 1, 1); // remove `:`
          Int := getLabelID(Str);

          if (Int = -1) Then
          Begin
           if (Copy(Str, 1, 11) = '__function_') Then // function label not found
           Begin
            With Compile1.TCompiler(Compiler) do
            Begin
             findFunctionByLabel(Str, FuncID, Namespace);
             if (FuncID = -1) Then
              goto LabelNotFound;

             With Compile1.TCompiler(Compiler) do
              With NamespaceList[Namespace].GlobalList[FuncID].mFunction do
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
          End Else
          Begin
           TVarData(Value).vtype := vtInteger; // @TODO: I have no idea why, but without this line, program crashes :|
           Value := LabelList[Int].Position-Pos; // jump have to be relative against the current opcode
          End;
         End;

         { label absolute address }
         if (Copy(Str, 1, 1) = '@') Then
         Begin
          Delete(Str, 1, 1); // remove `@`
          Int := getLabelID(Str);

          if (Int = -1) Then
          Begin
           if (Token = nil) Then
            Compile1.TCompiler(Compiler).CompileError(eBytecode_LabelNotFound, [Str]) Else
            Compile1.TCompiler(Compiler).CompileError(Token^, eBytecode_LabelNotFound, [Str]);

           Value := 0;
          End Else
           Value := LabelList[Int].Position;
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
        End;

        Case Typ of
         ptBoolReg..ptReferenceReg: Inc(OpcodeLen, sizeof(Byte));
         ptBool: Inc(OpcodeLen, sizeof(Byte));
         ptChar: Inc(OpcodeLen, sizeof(Byte));
         ptFloat: Inc(OpcodeLen, sizeof(Extended));
         ptString: Inc(OpcodeLen, Length(Str)+sizeof(Byte)); // string + terminator char (0x00)
         else Inc(OpcodeLen, sizeof(Integer));
        End;
       End;
    End;
 End;
End;

{ TCompiler.Parse }
Procedure TCompiler.Parse;
Var Opcode: PMOpcode;
    Arg   : TMOpcodeArg;
Begin
 With BytecodeStream do
 Begin
  For Opcode in Compiler.OpcodeList Do
   With Opcode^ do
   Begin
    if (isComment) or (isLabel) Then // we care neither about comments nor labels
     Continue;

    if (Opcode in [o_byte, o_word, o_integer, o_extended]) Then
    Begin
     Case Opcode of
      o_byte: write_byte(Args[0].Value);
      o_word: write_word(Args[0].Value);
      o_integer: write_integer(Args[0].Value);
      o_extended: write_extended(Args[0].Value);
     End;

     Continue;
    End;

    write_byte(ord(Opcode)); // opcode type
    For Arg in Args Do
     With Arg do
     Begin
      Try
       write_byte(ord(Typ)); // param type
       Case Typ of // param value
        ptBoolReg..ptReferenceReg: write_byte(Value);
        ptBool: write_byte(Value);
        ptChar: write_byte(Value);
        ptFloat: write_extended(Value);
        ptString: write_string(Value);
        else write_integer(Value);
       End;
      Except
       self.Compiler.CompileError(eInternalError, ['Not a numeric value: `'+VarToStr(Value)+'`']);
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
 Preparse2;

 Output := Compiler.OutputFile;

 BytecodeStream := TStream.Create;
 HeaderStream   := TStream.Create;
 Zip            := TZipper.Create;

 Try
  Parse;

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
