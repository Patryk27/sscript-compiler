(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit SSM_parser;

 Interface
 Uses Compile1, Compile2, Opcodes, MTypes, Messages;

 Const Header: Array[0..2] of Byte = ($53, $4D, $04);

 Type TSSM = Class
              Private
               FFile     : File of Byte;
               C1        : Compile1.TCompiler;
               C2        : Compile2.TCompiler;
               WritePos  : LongWord;
               ModuleName: String;

               Output   : PByte;
               OutputPos: LongWord;

               ResolveLabelsNames: Boolean;

               Procedure write_byte(V: Byte);
               Procedure write_word(V: Word);
               Procedure write_longword(V: LongWord);
               Procedure write_integer(V: Integer);
               Procedure write_extended(V: Extended);
               Procedure write_string(V: String);

               Function read_byte: Byte;
               Function read_longword: LongWord;
               Function read_integer: Integer;
               Function read_extended: Extended;

               Procedure WriteExports;
               Procedure WriteOpcodes;

               Procedure ReadExports;
               Function ReadOpcodes: Boolean;
              Public
               mExportList: Array of TMExport;

               Function Create_SSM(OutputFile: String; pC1, pC2: Pointer): TSSM;
               Function Load(InputFile, fModuleName: String; pC1: Pointer; fResolveLabelsNames: Boolean=True): Boolean;
              End;

 Implementation
Uses SysUtils, Variants;

(* TSSM.write_byte *)
{
 Writes a byte into the output file
}
Procedure TSSM.write_byte(V: Byte);
Begin
 PByte(Output+OutputPos)^ := V;
 Inc(WritePos, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

(* TSSM.write_longword *)
{
 Writes a longword value into the output file
}
Procedure TSSM.write_longword(V: LongWord);
Begin
 PLongWord(Output+OutputPos)^ := V;
 Inc(WritePos, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

(* TSSM.write_word *)
{
 Writes a word value into the output file
}
Procedure TSSM.write_word(V: Word);
Begin
 PWord(Output+OutputPos)^ := V;
 Inc(WritePos, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

(* TSSM.write_integer *)
{
 Writes an integer value into the output file
}
Procedure TSSM.write_integer(V: Integer);
Begin
 PInteger(Output+OutputPos)^ := V;
 Inc(WritePos, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

(* TSSM.write_extended *)
{
 Writes an extended value into the output file
}
Procedure TSSM.write_extended(V: Extended);
Begin
 PExtended(Output+OutputPos)^ := V;
 Inc(WritePos, sizeof(V));
 Inc(OutputPos, sizeof(V));
End;

(* TSSM.write_string *)
{
 Writes a string value into the output file
}
Procedure TSSM.write_string(V: String);
Var Ch: Char;
Begin
 For Ch in V Do
  write_byte(ord(Ch));
 write_byte(0);
End;

(* TSSM.read_byte *)
{
 Reads a byte from the input file
}
Function TSSM.read_byte: Byte;
Begin
 BlockRead(FFile, Result, sizeof(Result));
End;

(* TSSM.read_longword *)
{
 Reads a longword value from the input file
}
Function TSSM.read_longword: LongWord;
Begin
 BlockRead(FFile, Result, sizeof(Result));
End;

(* TSSM.read_integer *)
{
 Reads an integer value from the input file
}
Function TSSM.read_integer: Integer;
Begin
 BlockRead(FFile, Result, sizeof(Result));
End;

(* TSSM.read_extended *)
{
 Reads an extended value from the input file
}
Function TSSM.read_extended: Extended;
Begin
 BlockRead(FFile, Result, sizeof(Result));
End;

(* TSSM.WriteExports *)
{
 Writes export list into the output file
}
Procedure TSSM.WriteExports;
Var Ex: TMExport;
Begin
 write_longword(Length(C1.ExportList));
 For Ex in C1.ExportList Do
 Begin
  Ex.Pos := C2.getLabelID(Ex.Name);
  if (Ex.Pos = -1) Then
   C1.CompileError(eBytecode_ExportNotFound, [Ex.Name]);

  if (Ex.Pos < 0) or (Ex.Pos > High(C2.LabelList)) Then // shouldn't happen
   C1.CompileError(eInternalError, ['Invalid `Ex.Pos`']);

  Ex.Pos := C2.LabelList[Ex.Pos].Position;
  write_longword(Ex.NamePos); // function name
  write_longword(Ex.Pos);     // function position
 End;
End;

(* TSSM.WriteOpcodes *)
{
 Writes opcodes into the output file
}
Procedure TSSM.WriteOpcodes;
Var Opcode: PMOpcode;
    Arg   : TMOpcodeArg;
    Str   : String;
Begin
 WritePos := 0;

 For Opcode in C1.OpcodeList Do
  With Opcode^ do
  Begin
   if (isComment) or (isLabel) Then // we don't care about comments nor labels
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
End;

(* TSSM.ReadExports *)
{
 Reads an export list from the input file
}
Procedure TSSM.ReadExports;
Var I, Count: LongWord;
Begin
 With C1 do
 Begin
  Count := read_longword;

  if (Count = 0) Then // no imports
   Exit;

  SetLength(mExportList, Count);

  For I := 0 To Count-1 Do
  Begin
   mExportList[I].NamePos := read_longword;
   mExportList[I].Pos     := read_longword;
  End;
 End;
End;

(* TSSM.ReadOpcodes *)
{
 Reads opcodes from the input file
}
Function TSSM.ReadOpcodes: Boolean;
Var Size, Pos: LongWord;
    I        : Integer;
    Code     : PByte;

{ getString }
Function getString(Pos: LongWord): String;
Begin
 Result := '';

 While (Code[Pos] <> 0) Do
 Begin
  Result += chr(Code[Pos]);
  Inc(Pos);
 End;
End;

Begin
 Result := False;

 Size := FileSize(FFile)-FilePos(FFile); // how many bytes left?
 Code := AllocMem(Size);

 BlockRead(FFile, Code[0], Size);

 Pos := 0;

 { read exports' names }
 For I := Low(mExportList) To High(mExportList) Do
  mExportList[I].Name := getString(mExportList[I].NamePos);

 { read opcodes }
 While (Pos < Size) do
 Begin
  For I := Low(mExportList) To High(mExportList) Do
   if (mExportList[I].Pos = Pos) Then
   Begin
    if (ResolveLabelsNames) Then
     C1.PutLabel(StringReplace(mExportList[I].Name, '___', '_'+ModuleName+'_', [])) Else
     C1.PutLabel(mExportList[I].Name);
    Break;
   End;

  C1.PutOpcode(o_byte, [Code[Pos]]);
  Inc(Pos);
 End;

 { success! :) }
 Exit(True);
End;

(* TSSM.Create_SSM *)
{
 Saves a SSM file.
 pC1 -> pointer to Compile1.TCompiler
 pC2 -> pointer to Compile2.TCompiler
}
Function TSSM.Create_SSM(OutputFile: String; pC1, pC2: Pointer): TSSM;
Var Ch: Char;
Begin
 C1 := Compile1.TCompiler(pC1);
 C2 := Compile2.TCompiler(pC2);

 Result := self;

 Output    := AllocMem(15*1024*1024); // 15 MB should be enough, I guess...
 OutputPos := 0;

 Try
  write_byte(Header[0]);
  write_byte(Header[1]);
  write_byte(Header[2]);

  WriteExports;
  WriteOpcodes;
  For Ch in C2.StringSection Do
   write_byte(ord(Ch));
 Except
  Writeln('-- bytecode compile failed --');
  raise;
 End;

 { save buffer }
 AssignFile(FFile, OutputFile);
 Rewrite(FFile);
 BlockWrite(FFile, Output[0], OutputPos);
 CloseFile(FFile);

 { free memory }
 FreeMem(Output);
End;

{ TSSM.Load }
Function TSSM.Load(InputFile, fModuleName: String; pC1: Pointer; fResolveLabelsNames: Boolean=True): Boolean;
Begin
 Result := False;

 C1         := Compile1.TCompiler(pC1);
 ModuleName := fModuleName;

 ResolveLabelsNames := fResolveLabelsNames;

 if (not FileExists(InputFile)) Then // is file found?
  Exit;

 Try
  AssignFile(FFile, InputFile);
  Reset(FFile);

  { read header }
  if (read_byte <> Header[0]) or (read_byte <> Header[1]) or (read_byte <> Header[2]) Then
   Exit;

  { read exports }
  ReadExports;

  { read opcodes }
  if (not ReadOpcodes) Then
   Exit; // failed
 Finally
  CloseFile(FFile);
 End;

 Exit(True);
End;

End.
