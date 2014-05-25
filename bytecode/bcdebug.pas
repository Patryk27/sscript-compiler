(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 Generator for SScript files (both: libraries and applications).
*)
{$H+}
Unit BCDebug;

 Interface
 Uses symdef, LLCompiler, HLCompiler, Opcodes, Stream, List, SysUtils;

 Const DebugDataVersion: uint16 = 1;

 { EBCDebugWriterException }
 Type EBCDebugWriterException = Class(Exception);

 { EBCDebugReaderException }
 Type EBCDebugReaderException = Class(Exception);

 { TDBGFileData }
 Type TDBGFileData =
      Record
       FileName    : String;
       BytecodeSize: uint32;
      End;

 { TDBGFunctionData }
 Type TDBGFunctionData =
      Record
       FunctionName, LabelName: String;
       RangeBegin, RangeEnd   : uint32;
      End;

 { TDBGLineData }
 Type TDBGLineData =
      Record
       FileID, FunctionID: uint16; // taken from the debug symbol list
       Opcode            : uint32;
       Line              : uint16;
      End;

 { lists }
 Type TDBGFileList     = specialize TList<TDBGFileData>;
      TDBGFunctionList = specialize TList<TDBGFunctionData>;
      TDBGLineDataList = specialize TList<TDBGLineData>;

 { TDebugData }
 Type TDebugData =
      Record
       Version: uint16;

       FileCount    : uint32;
       FunctionCount: uint32;
       LineDataCount: uint32;

       FileList    : TDBGFileList;
       FunctionList: TDBGFunctionList;
       LineDataList: TDBGLineDataList;
      End;

 (* TBCDebugWriter *)
 Type TBCDebugWriter =
      Class
       Private
        HLCompiler: HLCompiler.TCompiler;
        LLCompiler: LLCompiler.TCompiler;

        Output: TStream;

        dbgFileList    : TDBGFileList;
        dbgFunctionList: TDBGFunctionList;
        dbgLineDataList: TDBGLineDataList;

       Private
        Procedure AppendDebugData(const Data: TDebugData);

        Function AllocDebugFile(const FileName: String; const BytecodeSize: uint32): uint32;
        Function AllocDebugFile(const Compiler: TCompiler): uint32;
        Function AllocDebugFunction(const mFunction: TFunction): uint32;
        Procedure GenerateLineInfo(const RangeBegin, RangeEnd: uint32; const FileID, FunctionID: uint32);
        Procedure GenerateLineInfo(const mFunction: TFunction; const FileID, FunctionID: uint32);

        Procedure FixOpcodePositions(const RelativeTo: PMOpcode; var Debug: TDebugData);

        Procedure GenerateData;
        Procedure SortLineData;
        Procedure WriteData;

       Public
        Constructor Create(const fHLCompiler: HLCompiler.TCompiler; const fLLCompiler: LLCompiler.TCompiler);
        Destructor Destroy; override;

        Function Generate(const DoSorting: Boolean=False): TStream;

       Public
        Property getOutput: TStream read Output;
       End;

 (* TBCDebugReader *)
 Type TBCDebugReader =
      Class
       Private
        Stream: TStream;

       Public
        Constructor Create(const fStream: TStream);
        Destructor Destroy; override;

        Function Read: TDebugData;
       End;

 Implementation
Uses SSMParser, Logging;

(* TBCDebugWriter.AppendDebugData *)
Procedure TBCDebugWriter.AppendDebugData(const Data: TDebugData);
Var I: int32;
Begin
 With Data do
 Begin
  // append file list
  For I := 0 To FileList.Count-1 Do
   dbgFileList.Add(FileList[I]);

  // append function list
  For I := 0 To FunctionList.Count-1 Do
   dbgFunctionList.Add(FunctionList[I]);
 End;
End;

(* TBCDebugWriter.AllocDebugFile *)
Function TBCDebugWriter.AllocDebugFile(const FileName: String; const BytecodeSize: uint32): uint32;
Var dbgFile: TDBGFileData;
Begin
 // check if this file hasn't been already placed on the list
 if (dbgFileList.Count > 0) Then
 Begin
  For Result := 0 To dbgFileList.Count-1 Do
   if (dbgFileList[Result].FileName = FileName) Then
    Exit;
 End;

 // quick check
 if (BytecodeSize = 0) Then
  raise EBCDebugWriterException.Create('This was not supposed to happen.');

 // if not, put it
 dbgFile.FileName     := FileName;
 dbgFile.BytecodeSize := BytecodeSize;

 Result := dbgFileList.Add(dbgFile);
End;

(* TBCDebugWriter.AllocDebugFile *)
Function TBCDebugWriter.AllocDebugFile(const Compiler: TCompiler): uint32;
Var FileName        : String;
    FuncMin, FuncMax: uint32;

    Namespace: TNamespace;
    Symbol   : TSymbol;
    Func     : TFunction;

    Tmp: uint32;
Begin
 FileName := Compiler.InputFile;

 FuncMin := High(uint32);
 FuncMax := 0;

 {
  @Note:
  A little of magic comes below, but we're basically searching for the first and
  last function and subtracting their opcode positions so we get the unit size
  as the result.
 }

 // each namespace
 For Namespace in Compiler.NamespaceList Do
 Begin
  // each symbol
  For Symbol in Namespace.getSymbolList Do
  Begin
   if (Symbol.Typ = stFunction) and (Symbol.mCompiler = Pointer(Compiler)) Then
   Begin
    Func := Symbol.mFunction;

    Tmp := PMOpcode(Func.FirstOpcode)^.OpcodePos;
    if (Tmp < FuncMin) Then
     FuncMin := Tmp;

    Tmp := PMOpcode(Func.LastOpcode)^.OpcodePos + PMOpcode(Func.LastOpcode)^.OpcodeSize;
    if (Tmp > FuncMax) Then
     FuncMax := Tmp;
   End;
  End;
 End;

 Result := AllocDebugFile(FileName, FuncMax-FuncMin);
End;

(* TBCDebugWriter.AllocDebugFunction *)
Function TBCDebugWriter.AllocDebugFunction(const mFunction: TFunction): uint32;
Var FunctionName, LabelName: String;
    dbgFunction            : TDBGFunctionData;
Begin
 FunctionName := mFunction.RefSymbol.getFullName('::');
 LabelName    := mFunction.LabelName;

 // check if this function hasn't been already placed on the list
 if (dbgFunctionList.Count > 0) Then
 Begin
  For Result := 0 To dbgFunctionList.Count-1 Do
   if (dbgFunctionList[Result].FunctionName = FunctionName) Then
    Exit;
 End;

 // if not, do so
 dbgFunction.FunctionName := FunctionName;
 dbgFunction.LabelName    := LabelName;
 dbgFunction.RangeBegin   := PMOpcode(mFunction.FirstOpcode)^.OpcodePos;
 dbgFunction.RangeEnd     := PMOpcode(mFunction.LastOpcode)^.OpcodePos;

 Result := dbgFunctionList.Add(dbgFunction);
End;

(* TBCDebugWriter.GenerateLineInfo *)
Procedure TBCDebugWriter.GenerateLineInfo(const RangeBegin, RangeEnd: uint32; const FileID, FunctionID: uint32);
Var PreviousLine: uint32 = 0;
    OpcodeList  : TOpcodeList;
    inBounds    : Boolean = False;
    LineData    : TDBGLineData;
    Opcode      : PMOpcode;
Begin
 LineData.FileID     := FileID;
 LineData.FunctionID := FunctionID;

 // fetch opdode list
 OpcodeList := HLCompiler.OpcodeList;

 // and iterate
 For Opcode in OpcodeList Do
 Begin
  inBounds := (Opcode^.OpcodePos >= RangeBegin) and (Opcode^.OpcodePos <= RangeEnd);

  // if inside function
  if (inBounds) Then
  Begin
   // skip labels and comments
   if (Opcode^.isLabel) or (Opcode^.isComment) Then
    Continue;

   // check token
   if (Opcode^.Token = nil) Then
    Continue;

   LineData.Opcode := Opcode^.OpcodePos;
   LineData.Line   := Opcode^.Token^.Line;

   if (LineData.Line > PreviousLine) Then // reduce size - we don't need an uncountable number of the same entries in the table
   Begin
    PreviousLine := LineData.Line;
    dbgLineDataList.Add(LineData);
   End;
  End;
 End;
End;

(* TBCDebugWriter.GenerateLineInfo *)
Procedure TBCDebugWriter.GenerateLineInfo(const mFunction: TFunction; const FileID, FunctionID: uint32);
Var First, Last: PMOpcode;
Begin
 // fetch function bounds
 First := mFunction.FirstOpcode;
 Last  := mFunction.LastOpcode;

 // check them
 if (First = nil) or (Last = nil) Then
  raise EBCDebugWriterException.CreateFmt('Cannot generate line data for function %s - unknown function bounds.', [mFunction.RefSymbol.getFullName('::')]);

 // call actual handler
 GenerateLineInfo(First^.OpcodePos, Last^.OpcodePos, FileID, FunctionID);
End;

(* TBCDebugWriter.FixOpcodePositions *)
Procedure TBCDebugWriter.FixOpcodePositions(const RelativeTo: PMOpcode; var Debug: TDebugData);
Var Shift: uint32;
    LD   : TDBGLineData;
    I    : uint32;
Begin
 if (RelativeTo = nil) Then
  Exit;

 Shift := RelativeTo^.OpcodePos + RelativeTo^.OpcodeSize;

 With Debug do
 Begin
  if (LineDataCount > 0) Then
  Begin
   For I := 0 To LineDataCount-1 Do
   Begin
    LD := LineDataList[I];

    LD.Opcode += Shift;

    With FunctionList[LD.FunctionID] do
    Begin
     RangeBegin += Shift;
     RangeEnd   += Shift;
    End;

    LineDataList[I] := LD;
   End;
  End;
 End;
End;

(* TBCDebugWriter.GenerateData *)
{
 Generates debug data.
}
Procedure TBCDebugWriter.GenerateData;
Var Opcode: PMOpcode;
    Symbol: TSymbol;
    Reader: TSSMReader;
    Debug : TDebugData;
    I     : int32;
Begin
 For I := 0 To HLCompiler.SSMReaderList.Count-1 Do
 Begin
  Reader := TSSMReader(HLCompiler.SSMReaderList[I]);
  Debug  := Reader.getDebugData;

  FixOpcodePositions(Reader.getLastCompilerOpcode, Debug);

  AppendDebugData(Debug);
 End;

 // each opcode
 For Opcode in HLCompiler.OpcodeList Do
 Begin
  if (Opcode^.isLabel) and (Opcode^.isFunction) Then
  Begin
   Symbol := Opcode^.FunctionSymbol;

   if (Symbol = nil) Then
   Begin
    // @TODO (?)
    Continue;
   End;

   if (Symbol.mCompiler = nil) Then
   Begin
    GenerateLineInfo(Symbol.mFunction, AllocDebugFile(Symbol.DeclToken^.FileName, 0), AllocDebugFunction(Symbol.mFunction));
   End Else
   Begin
    GenerateLineInfo(Symbol.mFunction, AllocDebugFile(TCompiler(Symbol.mCompiler)), AllocDebugFunction(Symbol.mFunction));
   End;
  End;
 End;
End;

(* TBCDebugWriter.SortLineData *)
{
 Sorts line data ascending by opcode positions.
}
Procedure TBCDebugWriter.SortLineData;

  { Swap }
  Procedure Swap(const A, B: uint32);
  Var Tmp: TDBGLineData;
  Begin
   Tmp                := dbgLineDataList[A];
   dbgLineDataList[A] := dbgLineDataList[B];
   dbgLineDataList[B] := Tmp;
  End;

  { Sort }
  Procedure Sort(const L, R: uint32);
  Var M, I: uint32;
  Begin
   if (L < R) Then
   Begin
    M := L;

    For I := L+1 To R Do
    Begin
     if (dbgLineDataList[I].Opcode < dbgLineDataList[L].Opcode) Then
     Begin
      Inc(M);
      Swap(M, I);
     End;
    End;

    Swap(L, M);

    if (M > 0) Then
     Sort(L, M-1);

    Sort(M+1, R);
   End;
  End;

Begin
 if (dbgLineDataList.Count < 2) Then
  Exit;

 Sort(0, dbgLineDataList.Count-1);
End;

(* TBCDebugWriter.WriteData *)
{
 Writes generated data to the stream.
}
Procedure TBCDebugWriter.WriteData;
Var dbgFileData: TDBGFileData;
    dbgFunction: TDBGFunctionData;
    dbgLineData: TDBGLineData;
Begin
 // write debug data
 With Output do
 Begin
  // write version
  write_uint16(DebugDataVersion);

  // write sizes
  write_uint32(dbgFileList.Count);
  write_uint32(dbgFunctionList.Count);
  write_uint32(dbgLineDataList.Count);

  // write debug file list
  For dbgFileData in dbgFileList Do
  Begin
   write_string(dbgFileData.FileName);
   write_uint32(dbgFileData.BytecodeSize);
  End;

  // write debug function name list
  For dbgFunction in dbgFunctionList Do
  Begin
   write_string(dbgFunction.FunctionName);
   write_string(dbgFunction.LabelName);
   write_uint32(dbgFunction.RangeBegin);
   write_uint32(dbgFunction.RangeEnd);
  End;

  // write debug line data
  For dbgLineData in dbgLineDataList Do
  Begin
   write_uint16(dbgLineData.FileID);
   write_uint16(dbgLineData.FunctionID);
   write_uint32(dbgLineData.Opcode);
   write_uint16(dbgLineData.Line);

//   Writeln(dbgLineData.Opcode, ' -> ', dbgFunctionList[dbgLineData.FunctionID].FunctionName, ':', dbgLineData.Line, ' (', ExtractFileName(dbgFileList[dbgLineData.FileID].FileName), ' - ', dbgFileList[dbgLineData.FileID].BytecodeSize, ')');
  End;
 End;
End;

(* TBCDebugWriter.Create *)
Constructor TBCDebugWriter.Create(const fHLCompiler: HLCompiler.TCompiler; const fLLCompiler: LLCompiler.TCompiler);
Begin
 HLCompiler := fHLCompiler;
 LLCompiler := fLLCompiler;
End;

(* TBCDebugWriter.Destroy *)
Destructor TBCDebugWriter.Destroy;
Begin
 dbgFileList.Free;
 dbgFunctionList.Free;
 dbgLineDataList.Free;

 inherited Destroy;
End;

(* TBCDebugWriter.Generate *)
{
 Generates debug data and saves it to the output stream.
 Sorting should be done only when saving to a program (as VM expects data to
 be sorted). In SSM files it doesn't matter.
}
Function TBCDebugWriter.Generate(const DoSorting: Boolean): TStream;
Begin
 Output := TStream.Create;

 dbgFileList     := TDBGFileList.Create;
 dbgFunctionList := TDBGFunctionList.Create;
 dbgLineDataList := TDBGLineDataList.Create;

 GenerateData;

 if (DoSorting) Then
  SortLineData;

 WriteData;

 Result := Output;
End;

// -------------------------------------------------------------------------- //
(* TBCDebugReader.Create *)
Constructor TBCDebugReader.Create(const fStream: TStream);
Begin
 Stream := fStream;
End;

(* TBCDebugReader.Destroy *)
Destructor TBCDebugReader.Destroy;
Begin
 inherited Destroy;
End;

(* TBCDebugReader.Read *)
{
 Reads debug data from stream specified in the constructor.
}
Function TBCDebugReader.Read: TDebugData;
Var FD: TDBGFileData;
    FN: TDBGFunctionData;
    LD: TDBGLineData;
    I : uint32;
Begin
 With Result do
 Begin
  // read version
  Version := Stream.read_uint16;

  if (Version <> DebugDataVersion) Then
   raise EBCDebugReaderException.CreateFmt('Unsupported debug data version! (got %d, expected %d)', [Version, DebugDataVersion]);

  // read sizes
  FileCount     := Stream.read_uint32;
  FunctionCount := Stream.read_uint32;
  LineDataCount := Stream.read_uint32;

  Log('%d debug data files.', [FileCount]);
  Log('%d debug data functions.', [FunctionCount]);
  Log('%d debug data lines.', [LineDataCount]);

  // allocate lists
  FileList     := TDBGFileList.Create(FileCount);
  FunctionList := TDBGFunctionList.Create(FunctionCount);
  LineDataList := TDBGLineDataList.Create(LineDataCount);

  // read file list
  if (FileCount > 0) Then
  Begin
   For I := 0 To FileCount-1 Do
   Begin
    FD.FileName     := Stream.read_string;
    FD.BytecodeSize := Stream.read_uint32;

    FileList[I] := FD;
   End;
  End;

  // read function list
  if (FunctionCount > 0) Then
  Begin
   For I := 0 To FunctionCount-1 Do
   Begin
    FN.FunctionName := Stream.read_string;
    FN.LabelName    := Stream.read_string;
    FN.RangeBegin   := Stream.read_uint32;
    FN.RangeEnd     := Stream.read_uint32;

    FunctionList[I] := FN;
   End;
  End;

  // read line data
  if (LineDataCount > 0) Then
  Begin
   For I := 0 To LineDataCount-1 Do
   Begin
    LD.FileID     := Stream.read_uint16;
    LD.FunctionID := Stream.read_uint16;
    LD.Opcode     := Stream.read_uint32;
    LD.Line       := Stream.read_uint16;

//    Writeln(LD.Opcode, ' -> ', FunctionList[LD.FunctionID].FunctionName, ':', LD.Line, ' (', ExtractFileName(FileList[LD.FileID].FileName), ' - ', FileList[LD.FileID].BytecodeSize, ')');

    LineDataList[I] := LD;
   End;
  End;
 End;
End;
End.
