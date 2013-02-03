(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Compile2;

 Interface
 Uses CompilerUnit, Compile1, Classes, SysUtils, Variants, MTypes, Opcodes, Messages;

 Type TLabel = Record
                Name    : String;
                Position: LongWord;
               End;

 Type dbgTLabel = Record
                   Name, Position: LongWord;
                  End;

 Type TDebugSection = Record
                      { LabelsCount: LongWord; }
                       LabelList: Array of dbgTLabel;
                      End;

 Type TCompiler = Class
                   Private
                    Output   : PByte;
                    OutputPos: LongWord;
                   Public
                    Compiler: Compile1.TCompiler;
                    Debug   : TDebugSection;

                    LabelList: Array of TLabel;

                    StringSection: String;

                    FilePosition: LongWord;

                    OpcodeLen: LongWord;
                    DebugLen : LongWord; // 'DEBUG DATA' section length

                    Procedure write_byte(V: Byte);
                    Procedure write_word(V: Word);
                    Procedure write_longword(V: LongWord);
                    Procedure write_integer(V: Integer);
                    Procedure write_extended(V: Extended);
                    Procedure write_string(V: String);
                    Procedure write_section(ID: Byte; Length, DataPnt: LongWord);

                    Function getLabelID(Name: String): Integer;

                    Procedure Preparse;
                    Procedure Preparse2;
                    Procedure PreparseDebugData;

                    Procedure Parse;
                    Procedure ParseDebugData;

                   Public
                    Procedure Compile(fCompiler: Compile1.TCompiler; fStackSize: LongWord; SaveAs_SSM: Boolean);
                   End;

 Implementation
Uses SSM_parser;

{ TCompiler.write_byte }
Procedure TCompiler.write_byte(V: Byte);
Begin
 PByte(Output+OutputPos)^ := V;
 Inc(FilePosition, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

{ TCompiler.write_word }
Procedure TCompiler.write_word(V: Word);
Begin
 PWord(Output+OutputPos)^ := V;
 Inc(FilePosition, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

{ TCompiler.write_longword }
Procedure TCompiler.write_longword(V: LongWord);
Begin
 PLongWord(Output+OutputPos)^ := V;
 Inc(FilePosition, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

{ TCompiler.write_integer }
Procedure TCompiler.write_integer(V: Integer);
Begin
 PInteger(Output+OutputPos)^ := V;
 Inc(FilePosition, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

{ TCompiler.write_extended }
Procedure TCompiler.write_extended(V: Extended);
Begin
 PExtended(Output+OutputPos)^ := V;
 Inc(FilePosition, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

{ TCompiler.write_string }
Procedure TCompiler.write_string(V: String);
Var Ch: Char;
Begin
 For Ch in V Do
  write_byte(ord(Ch));
 write_byte(0);
End;

{ TCompiler.write_section }
Procedure TCompiler.write_section(ID: Byte; Length, DataPnt: LongWord);
Begin
 write_byte(ID);
 write_longword(Length);
 write_longword(DataPnt);
End;

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
Begin
 StringSection := '';

 With Compiler do
 Begin
  // search for strings in opcodes
  For I := 0 To OpcodeList.Count-1 Do
   With OpcodeList[I]^ do
    if (not isComment) and (not isLabel) and (Length(Args) > 0) Then // if opcode with even 1 argument
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

        { label }
        if (Copy(Str, 1, 1) = ':') Then
         Typ := ptInt;

        { register }
        if (isRegisterName(Str)) Then
        Begin
         Typ   := TPrimaryType(Byte(getRegister(Str).Typ)-Byte(ptBool)); // get register type
         Value := IntToStr(getRegister(Str).ID);
        End;

        { boolean truth }
        if (Str = 'true') or (Str = 'True') Then
        Begin
         Typ   := ptBool;
         Value := '1';
        End;

        { boolean false }
        if (Str = 'false') or (Str = 'False') Then
        Begin
         Typ   := ptBool;
         Value := '0';
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

  // add exports' names
  if (Length(ExportList) > 0) Then
   For I := Low(ExportList) To High(ExportList) Do
   Begin
    ExportList[I].NamePos := OpcodeLen;
    StringSection += ExportList[I].Name+#0;
    OpcodeLen += Length(ExportList[I].Name)+1;
   End;
 End;
End;

{ TCompiler.Preparse2 }
Procedure TCompiler.Preparse2;
Var I, Q: LongWord;
    Int : Integer;
    Str : String;
Label LabelNotFound;
Begin
 { from here, we have already parsed all the labels }

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

        { label }
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
            Int := FindFunctionByLabel(Str);
            if (Int = -1) Then
             goto LabelNotFound;

            With FunctionList[Int] do
             CompileError(DeclToken, eFunctionNotFound, [Name, LibraryFile]);
           End;
          End Else // just some "random" label not found
          Begin
          LabelNotFound:
           if (Token = nil) Then
            Compile1.TCompiler(Compiler).CompileError(eBytecode_LabelNotFound, [Str]) Else
            Compile1.TCompiler(Compiler).CompileError(Token^, eBytecode_LabelNotFound, [Str]);
          End;

          Value := 0; // otherwise compiler could crash
         End Else
          Value := LabelList[Int].Position - Pos; // jump have to be relative against the current opcode
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

        Case Typ of
         ptBoolReg..ptReferenceReg: Inc(OpcodeLen, sizeof(Byte));
         ptBool: Inc(OpcodeLen, sizeof(Byte));
         ptChar: Inc(OpcodeLen, sizeof(Byte));
         ptFloat: Inc(OpcodeLen, sizeof(Extended));
         ptString: Inc(OpcodeLen, Length(Str)+sizeof(Byte)); // string + terminator (0x00) char
         else Inc(OpcodeLen, sizeof(Integer));
        End;
       End;
    End;
 End;
End;

{ TCompiler.PreparseDebugData }
Procedure TCompiler.PreparseDebugData;
Var I: Integer;
Begin
 SetLength(Debug.LabelList, Length(LabelList));

 For I := Low(LabelList) To High(LabelList) Do
 Begin
  Debug.LabelList[I].Name     := Length(StringSection);
  Debug.LabelList[I].Position := LabelList[I].Position;

  Inc(DebugLen, 2*sizeof(LongWord));

  StringSection += LabelList[I].Name+#0;
 End;
End;

{ TCompiler.Parse }
Procedure TCompiler.Parse;
Var Opcode: PMOpcode;
    Arg   : TMOpcodeArg;
    Str   : String;
    Ch    : Char;
Begin
 FilePosition := 0;

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
     Str := VarToStr(Value);

     write_byte(ord(Typ)); // param type
     Case Typ of // param value
      ptBoolReg..ptReferenceReg: write_byte(StrToInt(Str));
      ptBool: write_byte(StrToInt(Str));
      ptChar: write_byte(StrToInt(Str));
      ptFloat: write_extended(StrToFloat(Str));
      ptString: write_string(Str);
      else write_integer(StrToInt(Str));
     End;
    End;
  End;

 { save strings }
 For Ch in StringSection Do
  write_byte(ord(Ch));
End;

{ TCompiler.ParseDebugData }
Procedure TCompiler.ParseDebugData;
Var I: Integer;
Begin
 write_longword(Length(Debug.LabelList));

 For I := Low(Debug.LabelList) To High(Debug.LabelList) Do
  With Debug.LabelList[I] do
  Begin
   write_longword(Name);
   write_longword(Position);
  End;
End;

{ TCompiler.Compile }
Procedure TCompiler.Compile(fCompiler: Compile1.TCompiler; fStackSize: LongWord; SaveAs_SSM: Boolean);
Var Ex: TMExport;
    FFile: File of Byte;
    Tmp, DataBegin, DataPos, SectionsCount: LongWord;
Begin
 Compiler := fCompiler;

 SectionsCount := 3; // info, exports, code
 DebugLen      := 0;

 Output    := AllocMem(5*1024*1024); // 5 MB
 OutputPos := 0;

 Preparse;
 Preparse2;

 if (_DBG in Compiler.Options) Then
 Begin
  PreparseDebugData;
  Inc(SectionsCount); // + debug data
 End;

 if (SaveAs_SSM) Then
 Begin
  TSSM.Create.Create_SSM(Compiler.OutputFile, fCompiler, self).Free;
  Exit;
 End;

 Try
  { header }
  write_byte($53);
  write_byte($53);
  write_byte($03);
  write_byte(SectionsCount); { sections count }

  DataBegin := 4+SectionsCount*9; // header size + 4*section size
  DataPos   := DataBegin;

  write_section(1, 8, DataPos); // 'INFO' section; type = 1; length = 12; data position = DataPos
  DataPos += 8;

  Tmp := (2*sizeof(LongWord)) * Length(Compiler.ExportList) + sizeof(Word);
  write_section(3, Tmp, DataPos); // 'EXPORTS' section; type = 3; length = Tmp
  DataPos += Tmp;

  Tmp := OpcodeLen+Length(StringSection);
  write_section(5, Tmp, DataPos); // 'CODE' section; type = 5; length = OpcodeLen + Length(StringSection)
  DataPos += Tmp;

  if (_DBG in Compiler.Options) Then
   write_section(6, DebugLen, DataPos); // 'DEBUG DATA' section; type = 6; length = DebugSectionPos

  { now - save the sections' data }

  // 1.'INFO' section
  write_longword(fStackSize); // stack size
  write_longword(0); // entry point

  // 2.'EXPORTS' section
  write_word(Length(Compiler.ExportList));
  For Ex in Compiler.ExportList Do
  Begin
   Ex.Pos := getLabelID(Ex.Name);
   if (Ex.Pos = -1) Then
    Compiler.CompileError(eBytecode_ExportNotFound, [Ex.Name]);

   if (Ex.Pos < 0) or (Ex.Pos > High(LabelList)) Then
    Compiler.CompileError(eInternalError, ['Invalid `Ex.Pos`']);

   Ex.Pos := LabelList[Ex.Pos].Position;
   write_longword(Ex.NamePos); // function name
   write_longword(Ex.Pos);     // function position
  End;

  // 3.'CODE' section+strings
  Parse;

  // 4.'DEBUG DATA' section
  if (_DBG in Compiler.Options) Then
   ParseDebugData;
 Except
  Writeln('-- bytecode compile failed --');
  raise;
 End;

 AssignFile(FFile, fCompiler.OutputFile);
 Rewrite(FFile);
 BlockWrite(FFile, Output[0], OutputPos);
 CloseFile(FFile);
End;

End.
