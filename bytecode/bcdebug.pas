(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 Generator for SScript files (both: libraries and applications).
*)
{$H+}
Unit BCDebug;

 Interface
 Uses symdef, BCCompiler, SSCompiler, Opcodes, Stream, List, SysUtils;

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
        SSCompiler: SSCompiler.TCompiler;
        BCCompiler: BCCompiler.TCompiler;

        Output: TStream;

        dbgFileList    : TDBGFileList;
        dbgFunctionList: TDBGFunctionList;
        dbgLineDataList: TDBGLineDataList;

       Private
        Procedure AppendDebugData(const Data: TDebugData);

        Function AllocDebugFile(const FileName: String; const BytecodeSize: uint32): uint32;
        Function AllocDebugFile(const Compiler: TCompiler): uint32;
        Function AllocDebugFunction(const mFunction: TFunction): uint32;
        Procedure GenerateLineInfo(const mFunction: TFunction; const FileID, FunctionID: uint32);

        Procedure FixOpcodePositions(const RelativeTo: PMOpcode; var Debug: TDebugData);

        Procedure GenerateData;
        Procedure SortLineData;
        Procedure WriteData;

       Public
        Constructor Create(const fSSCompiler: SSCompiler.TCompiler; const fBCCompiler: BCCompiler.TCompiler);
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

 Result := dbgFunctionList.Add(dbgFunction);
End;

(* TBCDebugWriter.GenerateLineInfo *)
Procedure TBCDebugWriter.GenerateLineInfo(const mFunction: TFunction; const FileID, FunctionID: uint32);
Var Opcode, First, Last: PMOpcode;
    PreviousLine       : uint32 = 0;
    OpcodeList         : TOpcodeList;
    inBounds           : Boolean = False;
    LineData           : TDBGLineData;
Begin
 LineData.FileID     := FileID;
 LineData.FunctionID := FunctionID;

 // fetch opdode list
 OpcodeList := SSCompiler.OpcodeList;

 // fetch function bounds
 First := mFunction.FirstOpcode;
 Last  := mFunction.LastOpcode;

 // check them
 if (First = nil) or (Last = nil) Then
  raise EBCDebugWriterException.CreateFmt('Cannot generate line data for function %s - unknown function bounds.', [mFunction.RefSymbol.getFullName('::')]);

 // and iterate
 For Opcode in OpcodeList Do
 Begin
  if (Opcode = First) Then
   inBounds := True Else

  if (Opcode = Last) Then
   inBounds := False;

  // if inside function
  if (inBounds) Then
  Begin
   // skip labels and comments
   if (Opcode^.isLabel) or (Opcode^.isComment) Then
    Continue;

   // check token
   if (Opcode^.Token = nil) Then
   Begin
    DevLog(dvError, 'Cannot generate line data for opcode #%d (in function %s) - it doesn''t have associated token.', [Opcode^.OpcodeID, mFunction.RefSymbol.getFullName('::')]);
    // raise EBCDebugWriterException.CreateFmt('Cannot generate line data for function %s - opcode #%d doesn''t have associated token.', [mFunction.RefSymbol.getFullName('::'), Opcode^.OpcodeID]);
    Continue;
   End;

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
Var Compiler : TCompiler;
    Namespace: TNamespace;
    Symbol   : TSymbol;

    List: TCompilerArray;

    Reader: TSSMReader;
    Debug : TDebugData;
    I     : int32;
Begin
 For I := 0 To SSCompiler.SSMReaderList.Count-1 Do
 Begin
  Reader := TSSMReader(SSCompiler.SSMReaderList[I]);
  Debug  := Reader.getDebugData;

  FixOpcodePositions(Reader.getLastCompilerOpcode, Debug);

  AppendDebugData(Debug);
 End;

 List := SSCompiler.IncludeList^;

 SetLength(List, Length(List)+1);
 List[High(List)] := SSCompiler;

 // each compiler
 For Compiler in List Do
 Begin
  // each namespace
  For Namespace in Compiler.NamespaceList Do
  Begin
   // each symbol
   For Symbol in Namespace.getSymbolList Do
   Begin
    // skip non-functions
    if (Symbol.Typ <> stFunction) Then
     Continue;

    // if library-imported symbol
    if (Symbol.mCompiler = nil) Then
    Begin
     With Symbol.mFunction do
     Begin
      if (RefSymbol.DeclToken = nil) Then
       raise EBCDebugWriterException.CreateFmt('Declaration token of function %s is not present!', [RefSymbol.getFullName('::')]);

      GenerateLineInfo(Symbol.mFunction, AllocDebugFile(RefSymbol.DeclToken^.FileName, 0), AllocDebugFunction(Symbol.mFunction));
     End;

     {
      @Note:
      We're calling here "AllocDebugFile" with the second parameter set to zero because we assume that this file already exists on the list.
      (otherwise there's nothing we can do as we don't know bytecode size of that file.)
     }
    End Else

    // if actual user symbol
    Begin
     GenerateLineInfo(Symbol.mFunction, AllocDebugFile(TCompiler(Symbol.mCompiler)), AllocDebugFunction(Symbol.mFunction));
    End;
   End;
  End;
 End;
End;

(* TBCDebugWriter.SortLineData *)
{
 Sorts line data ascending by opcode positions.
}
Procedure TBCDebugWriter.SortLineData;
Var N, I: uint32;
    Tmp : TDBGLineData;
Begin
 // @TODO: use quicksort or sth
 N := dbgLineDataList.Count-1;

 Repeat
  For I := 0 To N-1 Do
  Begin
   if (dbgLineDataList[I].Opcode > dbgLineDataList[I+1].Opcode) Then
   Begin
    Tmp                  := dbgLineDataList[I];
    dbgLineDataList[I]   := dbgLineDataList[I+1];
    dbgLineDataList[I+1] := Tmp;
   End;
  End;

  Dec(N);
 Until (N = 0);
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
Constructor TBCDebugWriter.Create(const fSSCompiler: SSCompiler.TCompiler; const fBCCompiler: BCCompiler.TCompiler);
Begin
 SSCompiler := fSSCompiler;
 BCCompiler := fBCCompiler;
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
