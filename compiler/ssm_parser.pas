(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit SSM_parser;

 Interface
 Uses Compile1, Compile2, Opcodes, MTypes, Messages, Classes, Stream, Zipper;

 Type TSSM = Class
              Private
               Zip   : TZipper;
               Unzip : TUnzipper;
               LoadOK: Boolean;

               C1        : Compile1.TCompiler;
               C2        : Compile2.TCompiler;
               ModuleName: String;

               ExportsStream: TStream;

               ResolveLabelsNames: Boolean;

               Procedure WriteExports;

               Procedure ReadHeader(const AStream: TStream);
               Procedure ReadExports(const AStream: TStream);
               Procedure ReadOpcodes(const AStream: TStream);

               Procedure OnCreateStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);
               Procedure OnDoneStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);

              Public
               mExportList: Array of TMExport;

               Function Save(const OutputFile: String; pC1, pC2: Pointer): TSSM;
               Function Load(const InputFile, fModuleName: String; pC1: Pointer; fResolveLabelsNames: Boolean=True): Boolean;
              End;

 Implementation
Uses CompilerUnit, SysUtils, Variants;

(* TSSM.WriteExports *)
{
 Writes export list into the output file
}
Procedure TSSM.WriteExports;
Var Ex: TMExport;
Begin
 With ExportsStream do
 Begin
  write_longword(Length(C1.ExportList)); // export count

  For Ex in C1.ExportList Do
  Begin
   { get label's ID }
   Ex.Pos := C2.getLabelID(Ex.Name);
   if (Ex.Pos = -1) Then
    C1.CompileError(eBytecode_ExportNotFound, [Ex.Name]);

   { get label's position }
   Ex.Pos := C2.LabelList[Ex.Pos].Position;

   { write data }
   write_string(Ex.Name);  // function name
   write_longword(Ex.Pos); // function position
  End;
 End;
End;

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
 End;

 if (version_major <> bytecode_version_major) or (version_minor <> bytecode_version_minor) Then
 Begin
  Log('Invalid bytecode version: '+IntToStr(version_major)+'.'+EndingZero(IntToStr(version_minor)));
  LoadOK := False;
 End;
End;

(* TSSM.ReadExports *)
Procedure TSSM.ReadExports(const AStream: TStream);
Var Count: LongWord;
    Ex   : TMExport;
Begin
 Count := AStream.read_longword;

 SetLength(mExportList, Count);

 if (Count > 0) Then
  For Count := 0 To Count-1 Do
  Begin
   Ex.Name := AStream.read_string;
   Ex.Pos  := AStream.read_longword;

   mExportList[Count] := Ex;
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
 AStream.Position := 0;

 NStream := TStream(AStream);

 Case AItem.ArchiveFileName of
  '.header': ReadHeader(NStream);
  '.exports': ReadExports(NStream);
  '.bytecode': ReadOpcodes(NStream);
  else
   C1.CompileError(eInternalError, ['Unknown archive file name: '+AItem.ArchiveFileName]);
 End;

 AStream.Free;
End;

(* TSSM.ReadOpcodes *)
{
 Reads opcodes from the input file
}
Procedure TSSM.ReadOpcodes(const AStream: TStream);
Var I, Pos: Integer;
Begin
 Pos := 0;

 { read opcodes }
 While (AStream.Can) do
 Begin
  For I := Low(mExportList) To High(mExportList) Do
   if (mExportList[I].Pos = Pos) Then
   Begin
    if (ResolveLabelsNames) Then
     C1.PutLabel(StringReplace(mExportList[I].Name, '___', '_'+ModuleName+'_', [])) Else
     C1.PutLabel(mExportList[I].Name);
    Break;
   End;

  C1.PutOpcode(o_byte, [AStream.read_byte]);
  Inc(Pos);
 End;
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

Begin
 Result := self;

 Zip := TZipper.Create;
 C1  := Compile1.TCompiler(pC1);
 C2  := Compile2.TCompiler(pC2);

 ExportsStream := TStream.Create;

 Try
  WriteExports;

  AddFile(C2.HeaderStream, '.header');
  AddFile(C2.BytecodeStream, '.bytecode');
  AddFile(ExportsStream, '.exports');

  Zip.FileName := OutputFile;
  Zip.ZipAllFiles;
 Finally
  ExportsStream.Free;
  Zip.Free;

  DeleteFile(OutputFile+'.header');
  DeleteFile(OutputFile+'.bytecode');
  DeleteFile(OutputFile+'.exports');
 End;
End;

(* TSSM.Load *)
{
 Loads a SSM file.
}
Function TSSM.Load(const InputFile, fModuleName: String; pC1: Pointer; fResolveLabelsNames: Boolean=True): Boolean;
Var FileList: TStringList;
Begin
 Result := False;

 if (not FileExists(InputFile)) Then // is file found?
  Exit;

 FileList   := TStringList.Create;
 Unzip      := TUnzipper.Create;
 C1         := Compile1.TCompiler(pC1);
 ModuleName := fModuleName;

 LoadOK := True;

 ResolveLabelsNames := fResolveLabelsNames;

 Try
  Unzip.FileName := InputFile;
  Unzip.Examine;
  Unzip.OnCreateStream := @OnCreateStream;
  Unzip.OnDoneStream   := @OnDoneStream;

  FileList.Clear;
  FileList.Add('.header');
  Unzip.UnzipFiles(FileList);

  if (not LoadOK) Then
   Exit(False);

  FileList.Clear;
  FileList.Add('.exports');
  Unzip.UnzipFiles(FileList);

  if (not LoadOK) Then
   Exit(False);

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
