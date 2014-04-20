(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 SSM file writer and reader.
*)
{$H+}
Unit SSMParser;

 Interface
 Uses BCCompiler, SSCompiler, BCDebug, Opcodes, Messages, symdef, Classes, List, SysUtils, Stream, Zipper;

 Const SSM_version: uint16 = 3;

 { ESSMParserExeption }
 Type ESSMParserException = Class(Exception);

 { TBCFunction }
 Type TBCFunction =
      Record
       Signature: String; // 'Signature' is basically serialized form of the function
       Position : uint32;

       Symbol: TSymbol; // not saved to the SSM file - used only as helper during the generation of the debug data
      End;

 { TBCType }
 Type TBCType =
      Record
       Name, Signature: String;
      End;

 { TBCVariable }
 Type TBCVariable =
      Record
       Name, Signature: String;
      End;

 { arrays }
 Type TBCFunctionList = specialize TList<TBCFunction>;
      TBCTypeList     = specialize TList<TBCType>;
      TBCVariableList = specialize TList<TBCVariable>;

 { TSSMData }
 Type TSSMData =
      Record
       // SSM file version
       SSM_version: uint16;

       // file data
       LabelCount   : uint32;
       FunctionCount: uint32;
       TypeCount    : uint32;
       VarCount     : uint32;

       LabelList   : TBCLabelList;
       FunctionList: TBCFunctionList;
       TypeList    : TBCTypeList;
       VarList     : TBCVariableList;
      End;

 { TSSMWriter }
 Type TSSMWriter =
      Class
       Private { fields }
        FileName: String;
        Zip     : TZipper;

        SSCompiler: SSCompiler.TCompiler;
        BCCompiler: BCCompiler.TCompiler;

       Public { methods }
        Constructor Create(const fFileName: String; const fSSCompiler: SSCompiler.TCompiler; const fBCCompiler: BCCompiler.TCompiler);
        Procedure Save;
       End;

 { TSSMReader }
 Type TSSMReader =
      Class
       Private { fields }
        FileName: String;
        Unzip   : TUnzipper;
        LoadOK  : Boolean;

        SSMData      : TSSMData;
        DebugData    : TDebugData;
        References   : String;
        NamespaceList: TNamespaceList;
        OpcodeList   : TOpcodeList;

        SSCompiler        : SSCompiler.TCompiler;
        LastCompilerOpcode: PMOpcode;

       Private { methods }
        Procedure ReadHeader(const AStream: TStream);
        Procedure ReadSSMData(const AStream: TStream);
        Procedure ReadDebugData(const AStream: TStream);
        Procedure ReadReferences(const AStream: TStream);
        Procedure ReadOpcodes(const AStream: TStream);

        Procedure OnCreateStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);
        Procedure OnDoneStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);

        Procedure ParseTypes;
        Procedure ParseFunctions;
        Procedure ParseVariables;

        Function findLabel(const Name: String): PMOpcode;

        Function findType(const NamespaceName, TypeName: String): TType;
        Function findNamespace(const Name: String): TNamespace;
        Function findOrCreateNamespace(const Name: String): TNamespace;

       Public { methods }
        Constructor Create(const fSSCompiler: SSCompiler.TCompiler; const fFileName: String);

        Function Load: Boolean;

        Property getFileName: String read FileName;
        Property getSSMData: TSSMData read SSMData;
        Property getDebugData: TDebugData read DebugData;
        Property getNamespaceList: TNamespaceList read NamespaceList;
        Property getOpcodeList: TOpcodeList read OpcodeList;
        Property getLastCompilerOpcode: PMOpcode read LastCompilerOpcode;
       End;

 Implementation
Uses Tokens, Variants, Logging, Serialization;

(* SplitByDot *)
Procedure SplitByDot(const Str: String; out Pre, Post: String);
Begin
 Pre  := Copy(Str, 1, Pos('.', Str)-1);
 Post := Copy(Str, Pos('.', Str)+1, Length(Str));
End;

// -------------------------------------------------------------------------- //
(* TSSMWriter.Create *)
Constructor TSSMWriter.Create(const fFileName: String; const fSSCompiler: SSCompiler.TCompiler; const fBCCompiler: BCCompiler.TCompiler);
Begin
 FileName   := fFileName;
 SSCompiler := fSSCompiler;
 BCCompiler := fBCCompiler;
End;

(* TSSMWriter.Save *)
Procedure TSSMWriter.Save;
Var DataStream, DebugDataStream: TStream;

    LabelList   : TBCLabelList;
    FunctionList: TBCFunctionList;
    TypeList    : TBCTypeList;
    VarList     : TBCVariableList;

  { AddFile }
  Procedure AddFile(const Stream: TStream; FName: String); inline;
  Begin
   Stream.Position := 0;
   Zip.Entries.AddFileEntry(Stream, FName);
  End;

Var BCLabel   : TBCLabel;
    BCFunction: TBCFunction;
    BCType    : TBCType;
    BCVariable: TBCVariable;

    Namespace: TNamespace;
    Symbol   : TSymbol;

    Debug: TBCDebugWriter = nil;
Begin
 Log('Saving the SSM file as: %s', [FileName]);

 // create classes
 Zip        := TZipper.Create;
 DataStream := TStream.Create;

 Try
  LabelList := TBCLabelList.Create;

  FunctionList := TBCFunctionList.Create;
  TypeList     := TBCTypeList.Create;
  VarList      := TBCVariableList.Create;

  Try
   // prepare label and function list
   For BCLabel in BCCompiler.LabelList Do
   Begin
    // skip private labels
    if (not BCLabel.isPublic) Then
     Continue;

    // add it into the label list
    LabelList.Add(BCLabel);

    // ... and check if it's a function label
    if (BCLabel.isFunction) and (BCLabel.FunctionSymbol.mFunction.RefSymbol.Visibility = mvPublic) Then
    Begin
     BCFunction.Signature := BCLabel.Name;
     BCFunction.Position  := BCLabel.Position;
     BCFunction.Symbol    := BCLabel.FunctionSymbol;

     FunctionList.Add(BCFunction);
    End;
   End;

   // prepare type and variable list
   For Namespace in SSCompiler.NamespaceList Do
   Begin
    For Symbol in Namespace.SymbolList Do
    Begin
     // if type
     if (Symbol.Typ = stType) Then
     Begin
      if (Symbol.isInternal) or (Symbol.Visibility = mvPublic) Then // internal types are saved so that functions like "function<void> foo()" can be properly re-sparsed
      Begin
       BCType.Name      := Symbol.getFullName;
       BCType.Signature := Symbol.mType.getSerializedForm;

       TypeList.Add(BCType);
      End;
     End Else

     // if variable
     if (Symbol.Typ = stVariable) Then
     Begin
      if (not Symbol.isInternal) and (Symbol.Visibility = mvPublic) Then
      Begin
       BCVariable.Name      := Symbol.getFullName;
       BCVariable.Signature := Symbol.mVariable.getSerializedForm;

       VarList.Add(BCVariable);
      End;
     End;
    End;
   End;

   // write SSM data
   With DataStream do
   Begin
    write_uint16(SSM_version);

    write_uint32(LabelList.Count);
    write_uint32(FunctionList.Count);
    write_uint32(TypeList.Count);
    write_uint32(VarList.Count);

    // write labels
    For BCLabel in LabelList Do
    Begin
     write_string(BCLabel.Name);
     write_uint32(BCLabel.Position);
    End;

    // write functions
    For BCFunction in FunctionList Do
    Begin
     write_string(BCFunction.Signature);
     write_uint32(BCFunction.Position);
    End;

    // write types
    For BCType in TypeList Do
    Begin
     write_string(BCType.Name);
     write_string(BCType.Signature);
    End;

    // write variables
    For BCVariable in VarList Do
    Begin
     write_string(BCVariable.Name);
     write_string(BCVariable.Signature);
    End;
   End;
  Finally
   // free resources
   LabelList.Free;
   FunctionList.Free;
   TypeList.Free;
   VarList.Free;
  End;

  // generate debug data
  Debug           := TBCDebugWriter.Create(SSCompiler, BCCompiler);
  DebugDataStream := Debug.Generate;

  // save ZIP
  AddFile(BCCompiler.HeaderStream, '.header');
  AddFile(DataStream, '.ssm_data');
  AddFile(DebugDataStream, '.debug_data');
  AddFile(BCCompiler.ReferenceStream, '.references');
  AddFile(BCCompiler.BytecodeStream, '.bytecode');

  Zip.FileName := FileName;
  Zip.ZipAllFiles;
 Finally
  // free classes
  DataStream.Free;
  Debug.Free;
  Zip.Free;
 End;
End;

// -------------------------------------------------------------------------- //
(* TSSMReader.ReadHeader *)
{
 Reads and parses header from the stream
}
Procedure TSSMReader.ReadHeader(const AStream: TStream);
Var magic_number                : uint32;
    version_major, version_minor: uint8;

  { EndingZero }
  Function EndingZero(const Text: String): String;
  Begin
   if (Length(Text) = 1) Then
    Exit(Text+'0') Else
    Exit(Text);
  End;

Begin
 magic_number := AStream.read_uint32;
 AStream.ReadByte; // isRunnable attribute is skipped (it doesn't matter here)
 version_major := AStream.read_uint8;
 version_minor := AStream.read_uint8;

 if (magic_number <> $0DEFACED) Then
 Begin
  Log('Invalid magic number: 0x'+IntToHex(magic_number, 2*sizeof(uint32)));
  LoadOK := False;
  Exit;
 End;

 if (version_major <> bytecode_version_major) or (version_minor <> bytecode_version_minor) Then
 Begin
  Log('Invalid bytecode version: %d.%s', [version_major, EndingZero(IntToStr(version_minor))]);
  LoadOK := False;
  Exit;
 End;
End;

(* TSSMReader.ReadSSMData *)
Procedure TSSMReader.ReadSSMData(const AStream: TStream);
Var BCLabel   : TBCLabel;
    BCFunction: TBCFunction;
    BCType    : TBCType;
    BCVariable: TBCVariable;

    I: uint32;
Begin
 // read version
 SSMData.SSM_version := AStream.read_uint16;

 // check version
 if (SSMData.SSM_version <> SSM_version) Then
 Begin
  Log('Invalid SSM file version: %d (expected: %d)', [SSMData.SSM_version, SSM_version]);
  LoadOK := False;
  Exit;
 End;

 With SSMData do
 Begin
  // read sizes
  LabelCount    := AStream.read_uint32;
  FunctionCount := AStream.read_uint32;
  TypeCount     := AStream.read_uint32;
  VarCount      := AStream.read_uint32;

  Log('%d labels to be read.', [LabelCount]);
  Log('%d functions to be read.', [FunctionCount]);
  Log('%d types to be read.', [TypeCount]);
  Log('%d variables to be read.', [VarCount]);

  // allocate lists
  LabelList    := TBCLabelList.Create(LabelCount);
  FunctionList := TBCFunctionList.Create(FunctionCount);
  TypeList     := TBCTypeList.Create(TypeCount);
  VarList      := TBCVariableList.Create(VarCount);

  // read label list
  if (LabelCount > 0) Then
  Begin
   For I := 0 To LabelCount-1 Do
   Begin
    BCLabel.Name     := AStream.read_string;
    BCLabel.Position := AStream.read_uint32;

    DevLog(dvInfo, 'Label #%d -> name=''%s''; position=0x%x', [I, BCLabel.Name, BCLabel.Position]);
    LabelList[I] := BCLabel;
   End;
  End;

  // read function list
  if (FunctionCount > 0) Then
  Begin
   For I := 0 To FunctionCount-1 Do
   Begin
    BCFunction.Signature := AStream.read_string;
    BCFunction.Position  := AStream.read_uint32;

    DevLog(dvInfo, 'Function #%d -> signature=''%s''; position=0x%x', [I, BCFunction.Signature, BCFunction.Position]);
    FunctionList[I] := BCFunction;
   End;
  End;

  // read type list
  if (TypeCount > 0) Then
  Begin
   For I := 0 To TypeCount-1 Do
   Begin
    BCType.Name      := AStream.read_string;
    BCType.Signature := AStream.read_string;

    DevLog(dvInfo, 'Type #%d -> name=''%s''; signature==''%s''', [I, BCType.Name, BCType.Signature]);
    TypeList[I] := BCType;
   End;
  End;

  // read variable list
  if (VarCount > 0) Then
  Begin
   For I := 0 To VarCount-1 Do
   Begin
    BCVariable.Name      := AStream.read_string;
    BCVariable.Signature := AStream.read_string;

    DevLog(dvInfo, 'Variable #%d -> name=''%s''; signature=''%s''', [I, BCVariable.Name, BCVariable.Signature]);
    VarList[I] := BCVariable;
   End;
  End;
 End;
End;

(* TSSMReader.ReadDebugData *)
{
 Reads SSM file debug data.
}
Procedure TSSMReader.ReadDebugData(const AStream: TStream);
Var Debug: TBCDebugReader;
Begin
 Debug := TBCDebugReader.Create(AStream);

 Try
  DebugData := Debug.Read;
 Finally
  Debug.Free;
 End;
End;

(* TSSMReader.ReadReferences *)
{
 Reads references data from the stream
}
Procedure TSSMReader.ReadReferences(const AStream: TStream);
Begin
 References := '';

 While (AStream.Can) Do
  References += chr(AStream.read_uint8);
End;

(* TSSMReader.ReadOpcodes *)
{
 Reads and parses opcodes from the stream
}
Procedure TSSMReader.ReadOpcodes(const AStream: TStream);
Var StreamPosition: uint32;
    LabelName     : String;
    MOpcode       : PMOpcode = nil;

    FunctionList: TBCFunctionList;
    LabelList   : TBCLabelList;

    dbgFileList    : TDBGFileList;
    dbgFunctionList: TDBGFunctionList;
    dbgLineDataList: TDBGLineDataList;

    FileID, FunctionID: uint32; // used to fetch the debug data
    TotalSize         : uint32; // ditto
    CurrentLine       : uint32 = 0; // fetched from the debug data if available

    I, ParamC: int32;
Begin
 if (SSCompiler <> nil) Then
 Begin
  With SSCompiler.OpcodeList do
  Begin
   if (Count > 0) Then
    LastCompilerOpcode := Last;
  End;
 End;

 // prepare variables
 FunctionList := SSMData.FunctionList;
 LabelList    := SSMData.LabelList;

 dbgFileList     := DebugData.FileList;
 dbgFunctionList := DebugData.FunctionList;
 dbgLineDataList := DebugData.LineDataList;

 // find file ID in the debug data
 FileID     := High(uint32);
 FunctionID := High(uint32);

 // read opcodes
 While (AStream.Can) Do
 Begin
  LabelName      := '';
  StreamPosition := AStream.Position;

  // check if some function shouldn't be placed at this position
  For I := 0 To FunctionList.Count-1 Do
  Begin
   if (FunctionList[I].Position = StreamPosition) Then
   Begin
    LabelName := FunctionList[I].Signature;
    Break;
   End;

   // @TODO: if (FunctionList[I].Position > StreamPosition) Then Break; (?)
   //        or save previous read function index and begin the next loop fom that index
  End;

  // if not function, maybe some other label
  if (Length(LabelName) = 0) Then
  Begin
   For I := 0 To LabelList.Count-1 Do
   Begin
    if (LabelList[I].Position = StreamPosition) Then // some public label declaration
    Begin
     LabelName := LabelList[I].Name;
     Break;
    End;
   End;

   // @TODO: as to FunctionLists
  End;

  // place label
  if (Length(LabelName) > 0) Then
  Begin
   New(MOpcode);
   MOpcode^.Name       := LabelName;
   MOpcode^.isLabel    := True;
   MOpcode^.isFunction := False;
   MOpcode^.Token      := nil;
   OpcodeList.Add(MOpcode);

   // check for function label
   For I := 0 To dbgFunctionList.Count-1 Do
   Begin
    if (dbgFunctionList[I].LabelName = LabelName) Then
    Begin
     FunctionID  := I;
     CurrentLine := 0;

     MOpcode^.isFunction     := True;
     MOpcode^.FunctionSymbol := nil; // set inside TSSMReader.ParseFunctions()
    End;
   End;
  End;

  // check file debug data
  if (dbgFileList.Count > 0) Then
  Begin
   TotalSize := 0;

   For FileID := 0 To dbgFileList.Count-1 Do
   Begin
    Inc(TotalSize, dbgFileList[FileID].BytecodeSize);

    if (StreamPosition < TotalSize) Then
     Break;
   End;
  End;

  // check line debug data
  For I := 0 To dbgLineDataList.Count-1 Do
  Begin
   if (dbgLineDataList[I].FileID = FileID) and (dbgLineDataList[I].FunctionID = FunctionID) Then
   Begin
    if (dbgLineDataList[I].Opcode = StreamPosition) and (dbgLineDataList[I].Line > CurrentLine) Then
    Begin
     CurrentLine := dbgLineDataList[I].Line;

     Break;
    End;
   End;
  End;

  // special case - function beginning label has to have its token
  if (MOpcode <> nil) and (MOpcode^.isFunction) Then
  Begin
   if (CurrentLine > 0) Then
   Begin
    New(MOpcode^.Token);

    With MOpcode^.Token^ Do
    Begin
     Token    := noToken;
     Line     := CurrentLine;
     Char     := 0;
     FileName := dbgFileList[FileID].FileName;
    End;
   End;
  End;

  // read and prepare opcode
  New(MOpcode);
  MOpcode^.Opcode     := TOpcode_E(AStream.read_uint8);
  MOpcode^.isComment  := False;
  MOpcode^.isLabel    := False;
  MOpcode^.isFunction := False;

  MOpcode^.Compiler := nil;
  MOpcode^.Token    := nil;

  // debug data available?
  if (CurrentLine > 0) Then
  Begin
   New(MOpcode^.Token);

   With MOpcode^.Token^ Do
   Begin
    Token    := noToken;
    Line     := CurrentLine;
    Char     := 0;
    FileName := dbgFileList[FileID].FileName;
   End;
  End;

  ParamC := Opcodes.OpcodeList[ord(MOpcode^.Opcode)].ParamC;
  SetLength(MOpcode^.Args, ParamC);

  // read opcode's parameters
  With MOpcode^ do
  Begin
   For I := 0 To ParamC-1 Do
   Begin
    With Args[I] do
    Begin
     Typ := TPrimaryType(AStream.read_uint8);

     Case Typ of
      ptBoolReg..ptReferenceReg: Value := AStream.read_uint8;
      ptBool, ptChar           : Value := AStream.read_uint8;
      ptInt                    : Value := AStream.read_int64;
      ptFloat                  : Value := AStream.read_float;
      ptString                 : Value := AStream.read_string;
      ptConstantMemRef         : Value := AStream.read_int64;
      ptSymbolMemRef           : Value := String(PChar(@References[AStream.read_int64+1]));
      ptLabelAbsoluteReference : Value := String(PChar(@References[AStream.read_int64+1]));

      else
       Value := AStream.read_int32;
     End;
    End;
   End;
  End;

  // add opcode onto the list
  OpcodeList.Add(MOpcode);
 End;
End;

(* TSSMReader.OnCreateStream *)
Procedure TSSMReader.OnCreateStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);
Begin
 AStream := TMemoryStream.Create;
End;

(* TSSMReader.OnDoneStream *)
Procedure TSSMReader.OnDoneStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);
Var NStream: TStream;
Begin
 Log('Reading SSM archive file: %s (%d bytes)', [AItem.ArchiveFileName, AStream.Size]);

 AStream.Position := 0;

 NStream := TStream(AStream);

 Case AItem.ArchiveFileName of
  '.header'    : ReadHeader(NStream);
  '.ssm_data'  : ReadSSMData(NStream);
  '.debug_data': ReadDebugData(NStream);
  '.references': ReadReferences(NStream);
  '.bytecode'  : ReadOpcodes(NStream);

  else
   raise ESSMParserException.CreateFmt('Unknown archive file name: %s', [AItem.ArchiveFileName]);
 End;

 if (NStream.Can) Then
  Log('There are still some unread data in this file! (%d bytes left)', [AStream.Size-AStream.Position]);

 AStream.Free;
End;

(* TSSMReader.ParseTypes *)
Procedure TSSMReader.ParseTypes;
Var BCType: TBCType;
    Data  : TUnserializer;

    mType     : TType;
    mNamespace: TNamespace;

    NamespaceName, TypeName: String;
Begin
 // log
 Log('Parsing types (%d to parse)...', [SSMData.TypeCount]);

 // check if we have anything to parse
 if (SSMData.TypeCount = 0) Then
  Exit;

 // iterate each type
 For BCType in SSMData.TypeList Do
 Begin
  SplitByDot(BCType.Name, NamespaceName, TypeName); // namespace_name . type_name

  mNamespace := findOrCreateNamespace(NamespaceName);

  Data := TUnserializer.Create(BCType.Signature);
  Try
   mType                         := TType.Create(Data);
   mType.RefSymbol.Name          := TypeName;
   mType.RefSymbol.DeclNamespace := mNamespace;

   mNamespace.SymbolList.Add(TSymbol.Create(stType, mType));
  Finally
   Data.Free;
  End;
 End;
End;

(* TSSMReader.ParseFunctions *)
Procedure TSSMReader.ParseFunctions;
Var BCFunction: TBCFunction;
    Data      : TUnserializer;

    FuncID: int32;
    Param : int8;

    mFunction, mNextFunction: TFunction;

    ParamListNode: Serialization.TNode;

    FuncNamespace                        : TNamespace;
    NamespaceName, FunctionName, TypeName: String;

    FunctionLabel: PMOpcode;
Begin
 // log
 Log('Parsing functions (%d to parse)...', [SSMData.FunctionCount]);

 // check if we have anything to parse
 if (SSMData.FunctionCount = 0) Then
  Exit;

 // iterate each function
 For FuncID := 0 To SSMData.FunctionList.Count-1 Do
 Begin
  BCFunction := SSMData.FunctionList[FuncID];
  Data       := TUnserializer.Create(BCFunction.Signature);

  Try
   // basic check
   if (Data.getRoot[0].getString <> 'function') Then
    raise ESSMParserException.Create('Expected a function signature.');

   // separate namespace and function name
   SplitByDot(Data.getRoot[1].getString, NamespaceName, FunctionName);

   // fetch namespace
   FuncNamespace := findOrCreateNamespace(NamespaceName);

   // create this function
   mFunction           := TFunction.Create;
   mFunction.LabelName := BCFunction.Signature;

   // set refsymbol
   mFunction.RefSymbol.Name          := FunctionName;
   mFunction.RefSymbol.DeclNamespace := FuncNamespace;
// mFunction.RefVar @TODO

   // find label
   FunctionLabel := findLabel(mFunction.LabelName);

   if (FunctionLabel = nil) Then
    raise ESSMParserException.CreateFmt('No function label found! (parsing %s)', [mFunction.RefSymbol.getFullName('::')]);

   // fix function token
   mFunction.RefSymbol.DeclToken := FunctionLabel^.Token;
   mFunction.FirstOpcode         := FunctionLabel;

   // parse return type
   if (Data.getRoot[3].getType = ntValue) Then
   Begin
    // separate namespace and type name
    SplitByDot(Data.getRoot[3].getString, NamespaceName, TypeName);

    // find type
    mFunction.Return := findType(NamespaceName, TypeName);
   End Else
   Begin
    // parse type
    mFunction.Return := TType.Create(Data.getRoot[3]);
   End;

   // check return type
   if (mFunction.Return = nil) Then
    raise ESSMParserException.Create('Couldn''t parse function return type.');

   // parse parameter list
   SetLength(mFunction.ParamList, Data.getRoot[4].getInt);

   ParamListNode := Data.getRoot[5];
   For Param := 0 To High(mFunction.ParamList) Do
   Begin
    if (ParamListNode[Param].getType = ntValue) Then
    Begin
     // split name
     SplitByDot(ParamListNode[Param].getString, NamespaceName, TypeName);

     // find type
     mFunction.ParamList[Param].Typ := findType(NamespaceName, TypeName);
    End Else
    Begin
     // parse type
     mFunction.ParamList[Param].Typ := TType.Create(ParamListNode[Param]);
    End;

    if (mFunction.ParamList[Param].Typ = nil) Then
     raise ESSMParserException.CreateFmt('Couldn''t parse function parameter (param index=%d)', [Param]);
   End;

   // eventualy insert this function to the symbol list
   FuncNamespace.SymbolList.Add(TSymbol.Create(stFunction, mFunction));

   FunctionLabel^.FunctionSymbol := FuncNamespace.SymbolList.Last;
   BCFunction.Symbol             := FuncNamespace.SymbolList.Last;

   SSMData.FunctionList[FuncID] := BCFunction;
  Finally
   Data.Free;
  End;
 End;

 // one more thing has to be done - we need to set bounds for the functions, so that debug data won't be lost
 With SSMData do
 Begin
  For FuncID := 0 To FunctionList.Count-1 Do
  Begin
   mFunction := FunctionList[FuncID].Symbol.mFunction;

   if (FuncID = FunctionList.Count-1) Then
   Begin
    mFunction.LastOpcode := OpcodeList.Last;
   End Else
   Begin
    // end function one opcode before the next one starts and hopefully expect it to be the "ret" opcode
    mNextFunction := FunctionList[FuncID+1].Symbol.mFunction;

    mFunction.LastOpcode := OpcodeList[OpcodeList.indexOf(mNextFunction.FirstOpcode)-1];
   End;
  End;
 End;
End;

(* TSSMReader.ParseVariables *)
Procedure TSSMReader.ParseVariables;
Var BCVariable: TBCVariable;
    Data      : TUnserializer;

    mVariable: TVariable;

    VarNamespace                    : TNamespace;
    NamespaceName, VarName, TypeName: String;
Begin
 // log
 Log('Parsing variables (%d to parse)...', [SSMData.VarCount]);

 // check if we have anything to parse
 if (SSMData.VarCount = 0) Then
  Exit;

 // iterate each varible
 For BCVariable in SSMData.VarList Do
 Begin
  // separate namespace and variable name
  SplitByDot(BCVariable.Name, NamespaceName, VarName);

  // fetch namespace
  VarNamespace := findOrCreateNamespace(NamespaceName);

  // unserialize variable
  Data := TUnserializer.Create(BCVariable.Signature);

  Try
   mVariable                         := TVariable.Create(Data);
   mVariable.RefSymbol.Name          := VarName;
   mVariable.RefSymbol.DeclNamespace := VarNamespace;

   if (mVariable.Typ = nil) Then
   Begin
    // split namespace and type name
    SplitByDot(Data.getRoot[2].getString, NamespaceName, TypeName);

    // find type
    mVariable.Typ := findType(NamespaceName, TypeName);
   End;

   mVariable.LocationData.Location      := vlMemory;
   mVariable.LocationData.MemSymbolName := BCVariable.Signature;

   // insert variable to the symbol list
   VarNamespace.SymbolList.Add(TSymbol.Create(stVariable, mVariable));
  Finally
   Data.Free;
  End;
 End;
End;

(* TSSMReader.findLabel *)
{
 Returns label with specified name from the opcode list (or nil, if such couldn't have been found).
}
Function TSSMReader.findLabel(const Name: String): PMOpcode;
Begin
 For Result in OpcodeList Do
  if (Result^.isLabel) and (Result^.Name = Name) Then
   Exit;

 Exit(nil);
End;

(* TSSMReader.findType *)
{
 Returns type with specified name and namespace (or nil, if such couldn't have been found).
}
Function TSSMReader.findType(const NamespaceName, TypeName: String): TType;
Var Namespace: TNamespace;
    Symbol   : TSymbol;
Begin
 // get namespace
 Namespace := findNamespace(NamespaceName);
 if (Namespace = nil) Then
 Begin
  DevLog(dvWarning, 'Unknown namespace: %s', [NamespaceName]);
  Exit(nil);
 End;

 // get symbol
 Symbol := Namespace.findSymbol(TypeName);
 if (Symbol = nil) Then
 Begin
  DevLog(dvWarning, 'Unknown symbol: %s::%s', [NamespaceName, TypeName]);
  Exit(nil);
 End;

 // check symbol kind
 if (Symbol.Typ <> stType) Then
 Begin
  DevLog(dvWarning, 'Invalid symbol type (symbol: %s::%s)', [NamespaceName, TypeName]);
  Exit(nil);
 End;

 // eventually return the type
 Result := Symbol.mType;
End;

(* TSSMReader.findNamespace *)
{
 Returns namespace with specified name (or nil, if such couldn't have been found).
}
Function TSSMReader.findNamespace(const Name: String): TNamespace;
Begin
 For Result in NamespaceList Do
  if (Result.RefSymbol.Name = Name) Then
   Exit;

 Exit(nil);
End;

(* TSSMReader.findOrCreateNamespace *)
{
 Returns namespace with specified name or creates and returns it.
}
Function TSSMReader.findOrCreateNamespace(const Name: String): TNamespace;
Begin
 Result := findNamespace(Name);

 if (Result = nil) Then
 Begin
  Result                := TNamespace.Create;
  Result.RefSymbol.Name := Name;
  NamespaceList.Add(Result);
 End;
End;

(* TSSMReader.Create *)
Constructor TSSMReader.Create(const fSSCompiler: SSCompiler.TCompiler; const fFileName: String);
Begin
 FileName      := fFileName;
 OpcodeList    := TOpcodeList.Create;
 NamespaceList := TNamespaceList.Create;

 SSCompiler := fSSCompiler;
End;

(* TSSMReader.Load *)
Function TSSMReader.Load: Boolean;
Var FileList: TStringList;

  { LoadFile }
  Function LoadFile(const FileName: String): Boolean;
  Begin
   FileList.Clear;
   FileList.Add(FileName);
   Unzip.UnzipFiles(FileList);

   Exit(LoadOK);
  End;

Begin
 Result := False;
 Log('Loading a SSM file: '+FileName);

 if (not FileExists(FileName)) Then // error: file not found
  Exit;

 // create classes
 FileList := TStringList.Create;
 Unzip    := TUnzipper.Create;

 // set variables
 LoadOK := True;

 Try
  Unzip.FileName := FileName;
  Unzip.Examine;
  Unzip.OnCreateStream := @OnCreateStream;
  Unzip.OnDoneStream   := @OnDoneStream;

  // load header
  if (not LoadFile('.header')) Then
   Exit;

  // load SSM file data
  if (not LoadFile('.ssm_data')) Then
   Exit;

  // load SSM debug data
  if (not LoadFile('.debug_data')) Then
   Exit;

  // load references data
  if (not LoadFile('.references')) Then
   Exit;

  // load bytecode
  if (not LoadFile('.bytecode')) Then
   Exit;

  // parse loaded data
  ParseTypes;
  ParseFunctions;
  ParseVariables;
 Finally
  Unzip.Free;
  FileList.Free;
 End;

 Exit(LoadOK);
End;
End.
