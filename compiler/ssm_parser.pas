(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit SSM_parser;

 Interface
 Uses Compile1, Compile2, Opcodes, MTypes, Messages, Classes, Stream, Zipper;

 Const SSM_version = 1;

 { TSSMData }
 Type TSSMData = Packed Record
                  SSM_version: Word;

                  FunctionCount: Word;
                  FunctionList : Array of TLabel;
                 End;

 { TSSM }
 Type TSSM = Class
              Private
               Zip   : TZipper;
               Unzip : TUnzipper;
               LoadOK: Boolean;

               C1        : Compile1.TCompiler;
               C2        : Compile2.TCompiler;
               FileName  : String;
               ModuleName: String;

               DataStream: TStream;

               ResolveLabelsNames: Boolean;

               Procedure ReadHeader(const AStream: TStream);
               Procedure ReadSSMData(const AStream: TStream);
               Procedure ReadOpcodes(const AStream: TStream);

               Procedure OnCreateStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);
               Procedure OnDoneStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);

              Public
               Data: TSSMData;

               Function Save(const OutputFile: String; pC1, pC2: Pointer): TSSM;
               Function Load(const InputFile, fModuleName: String; pC1: Pointer; fResolveLabelsNames: Boolean=True): Boolean;
              End;

 Implementation
Uses CompilerUnit, SysUtils, Variants;

(* TSSM.ReadHeader *)
Procedure TSSM.ReadHeader(const AStream: TStream);
Var magic_number                : Longword;
    version_major, version_minor: Byte;

    // EndingZero
    Function EndingZero(const Text: String): String;
    Begin
     if (Length(Text) = 1) Then
      Exit(Text+'0') Else
      Exit(Text);
    End;

Begin
 magic_number := AStream.read_longword;
 AStream.ReadByte; // is_runnable
 version_major := AStream.read_byte;
 version_minor := AStream.read_byte;

 if (magic_number <> $0DEFACED) Then
 Begin
  Log('Invalid magic number: 0x'+IntToHex(magic_number, 2*sizeof(LongWord)));
  LoadOK := False;
  Exit;
 End;

 if (version_major <> bytecode_version_major) or (version_minor <> bytecode_version_minor) Then
 Begin
  Log('Invalid bytecode version: '+IntToStr(version_major)+'.'+EndingZero(IntToStr(version_minor)));
  LoadOK := False;
  Exit;
 End;
End;

(* TSSM.ReadSSMData *)
Procedure TSSM.ReadSSMData(const AStream: TStream);
Var I: Integer;
Begin
 Data.SSM_version := AStream.read_word;

 if (Data.SSM_version <> SSM_version) Then
 Begin
  Log('Invalid SSM file version: '+IntToStr(Data.SSM_version)+'; expected: '+IntToStr(SSM_version));
  LoadOK := False;
  Exit;
 End;

 With Data do
 Begin
  FunctionCount := AStream.read_word;

  SetLength(FunctionList, FunctionCount);
  For I := 0 To FunctionCount-1 Do
  Begin
   FunctionList[I].Name     := AStream.read_string;
   FunctionList[I].Position := AStream.read_longword;
  End;
 End;
End;

(* TSSM.ReadOpcodes *)
{
 Reads opcodes from the input file
}
Procedure TSSM.ReadOpcodes(const AStream: TStream);
Var I, ParamC: Integer;
    MOpcode  : PMOpcode;

    Name: String;
Begin
 { put file label (it will be used to resolve absolute addresses) }
 Name := FileName;

 For I := 1 To Length(Name) do
  if not (Name[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) Then
   Name[I] := '_';

 { read opcodes }
 While (AStream.Can) do
 Begin
  For I := 0 To High(Data.FunctionList) Do
   if (Data.FunctionList[I].Position = AStream.Position) Then // function label declaration
   Begin
    C1.PutLabel(Data.FunctionList[I].Name);
    Break;
   End;

  // read and prepare opcode
  New(MOpcode);
  MOpcode^.Opcode    := TOpcode_E(AStream.read_byte);
  MOpcode^.Compiler  := C1;
  MOpcode^.isComment := False;
  MOpcode^.isLabel   := False;
  MOpcode^.Pos       := 0;
  MOpcode^.Token     := nil;

  ParamC := OpcodeList[ord(MOpcode^.Opcode)].ParamC;
  SetLength(MOpcode^.Args, ParamC);

  // read opcode's parameters
  With MOpcode^ do
   For I := 0 To ParamC-1 Do
    With Args[I] do
    Begin
     Typ := TPrimaryType(AStream.read_byte);

     Case Typ of
      ptBoolReg..ptReferenceReg: Value := AStream.read_byte;
      ptBool, ptChar: Value := AStream.read_byte;
      ptFloat: Value := AStream.read_float;
      ptString, ptLabelAbsoluteReference: Value := AStream.read_string;
      else Value := AStream.read_integer;
     End;
    End;

  // add opcode onto the list
  C1.OpcodeList.Add(MOpcode);
 End;
End;

(* TSSM.OnCreateStream *)
Procedure TSSM.OnCreateStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);
Begin
 AStream := TMemoryStream.Create;
End;

(* TSSM.OnDoneStream *)
Procedure TSSM.OnDoneStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);
Var NStream: TStream;
Begin
 Log('Reading SSM archive file: '+AItem.ArchiveFileName);

 AStream.Position := 0;

 NStream := TStream(AStream);

 Case AItem.ArchiveFileName of
  '.header': ReadHeader(NStream);
  '.bytecode': ReadOpcodes(NStream);
  '.ssm_data': ReadSSMData(NStream);
  else
   C1.CompileError(eInternalError, ['Unknown archive file name: '+AItem.ArchiveFileName]);
 End;

 AStream.Free;
End;

(* TSSM.Create_SSM *)
{
 Saves a SSM file.
 pC1 -> pointer to Compile1.TCompiler
 pC2 -> pointer to Compile2.TCompiler
}
Function TSSM.Save(const OutputFile: String; pC1, pC2: Pointer): TSSM;

    // AddFile
    Procedure AddFile(const Stream: TStream; FileName: String);
    Begin
     Stream.SaveToFile(OutputFile+FileName);
     Zip.Entries.AddFileEntry(OutputFile+FileName, FileName);
    End;

Var FunctionList: Array of TLabel;
    I           : Integer;
Begin
 Log('Saving SSM file: '+OutputFile);

 Result := self;

 // prepare variables
 Zip := TZipper.Create;
 C1  := Compile1.TCompiler(pC1);
 C2  := Compile2.TCompiler(pC2);

 // create classes
 DataStream := TStream.Create;

 // write TSSMData
 With DataStream do
 Begin
  SetLength(FunctionList, 0);
  For I := Low(C2.LabelList) To High(C2.LabelList) Do
   if (C2.LabelList[I].isFunction) Then
   Begin
    SetLength(FunctionList, Length(FunctionList)+1);
    FunctionList[High(FunctionList)] := C2.LabelList[I];
   End;

  write_word(SSM_version);
  write_word(Length(FunctionList));
  For I := Low(FunctionList) To High(FunctionList) Do
  Begin
   write_string(FunctionList[I].Name);
   write_longword(FunctionList[I].Position);
  End;
 End;

 // save
 Try
  AddFile(C2.HeaderStream, '.header');
  AddFile(DataStream, '.ssm_data');
  AddFile(C2.BytecodeStream, '.bytecode');

  Zip.FileName := OutputFile;
  Zip.ZipAllFiles;
 Finally
  // free classes
  DataStream.Free;
  Zip.Free;

  // remove unused files
  DeleteFile(OutputFile+'.header');
  DeleteFile(OutputFile+'.ssm_data');
  DeleteFile(OutputFile+'.bytecode');
 End;
End;

(* TSSM.Load *)
{
 Loads a SSM file.
}
Function TSSM.Load(const InputFile, fModuleName: String; pC1: Pointer; fResolveLabelsNames: Boolean=True): Boolean;
Var FileList: TStringList;

  // Open
  Function Open(const FileName: String): Boolean;
  Begin
   FileList.Clear;
   FileList.Add(FileName);
   Unzip.UnzipFiles(FileList);

   Exit(LoadOK);
  End;

Begin
 Log('Reading SSM file: '+InputFile);

 Result   := False;
 FileName := InputFile;

 if (not FileExists(InputFile)) Then // does file exist?
  Exit;

 // create classes
 FileList   := TStringList.Create;
 Unzip      := TUnzipper.Create;
 C1         := Compile1.TCompiler(pC1);
 ModuleName := fModuleName;

 // set variables
 LoadOK             := True;
 ResolveLabelsNames := fResolveLabelsNames;

 // open and parse the SSM file
 Try
  Unzip.FileName := InputFile;
  Unzip.Examine;
  Unzip.OnCreateStream := @OnCreateStream;
  Unzip.OnDoneStream   := @OnDoneStream;

  // load header
  if (not Open('.header')) Then
   Exit;

  // load SSM file data
  if (not Open('.ssm_data')) Then
   Exit;

  // load bytecode
  FileList.Clear;
  FileList.Add('.bytecode');
  Unzip.UnzipFiles(FileList);
 Finally
  Unzip.Free;
  FileList.Free;
 End;

 Exit(True);
End;

End.
