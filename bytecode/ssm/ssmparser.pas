(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 SSM file writer and reader.
*)
{$H+}
Unit SSMParser;

 Interface
 Uses SSCompiler, BCCompiler, Opcodes, Messages, symdef, Classes, List, SysUtils, Stream, Zipper;

 Const SSM_version: uint16 = 2; // 3

 { ESSMParserExeption }
 Type ESSMParserException = Class(Exception);

 { TBCFunction }
 Type TBCFunction =
      Record
       Signature: String; // 'Signature' is basically serialized form of the function
       Position : uint32;
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

 { TBCLineData }
 Type TBCLineData =
      Record
       FileName, FunctionName: String;
       FunctionLine          : uint32;
      End;

 { arrays }
 Type TBCFunctionList = specialize TList<TBCFunction>;
      TBCTypeList     = specialize TList<TBCType>;
      TBCVariableList = specialize TList<TBCVariable>;
      TBCLineDataList = specialize TList<TBCLineData>;

 { TSSMData }
 Type TSSMData =
      Record
       SSM_version: uint16;

       LabelCount   : uint32;
       FunctionCount: uint32;
       TypeCount    : uint32;
       VarCount     : uint32;
       LineDataCount: uint32;

       LabelList   : TBCLabelList;
       FunctionList: TBCFunctionList;
       TypeList    : TBCTypeList;
       VarList     : TBCVariableList;
       LineDataList: TBCLineDataList;
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
        References   : String;
        NamespaceList: TNamespaceList;
        OpcodeList   : TOpcodeList;

       Private { methods }
        Procedure ReadHeader(const AStream: TStream);
        Procedure ReadSSMData(const AStream: TStream);
        Procedure ReadReferences(const AStream: TStream);
        Procedure ReadOpcodes(const AStream: TStream);

        Procedure OnCreateStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);
        Procedure OnDoneStream(Sender: TObject; var AStream: Classes.TStream; AItem: TFullZipFileEntry);

        Procedure ParseTypes;
        Procedure ParseFunctions;
        Procedure ParseVariables;

        Function findType(const NamespaceName, TypeName: String): TType;
        Function findNamespace(const Name: String): TNamespace;
        Function findOrCreateNamespace(const Name: String): TNamespace;

       Public { methods }
        Constructor Create(const fFileName: String);
        Function Load: Boolean;

        Property getFileName: String read FileName;
        Property getNamespaceList: TNamespaceList read NamespaceList;
        Property getOpcodeList: TOpcodeList read OpcodeList;
       End;

 Implementation
Uses Variants, Logging, Serialization;

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

  { AddFile }
  Procedure AddFile(const Stream: TStream; FName: String); inline;
  Begin
   Stream.SaveToFile(FileName+FName);
   Zip.Entries.AddFileEntry(FileName+FName, FName);
  End;

Var DataStream: TStream;

    LabelList   : TBCLabelList;
    FunctionList: TBCFunctionList;
    TypeList    : TBCTypeList;
    VarList     : TBCVariableList;
    LineDataList: TBCLineDataList;

    Namespace: TNamespace;
    Symbol   : TSymbol;

    BCLabel   : TBCLabel;
    BCFunction: TBCFunction;
    BCType    : TBCType;
    BCVariable: TBCVariable;
Begin
 Log('Saving a SSM file to: %s', [FileName]);

 // create classes
 Zip        := TZipper.Create;
 DataStream := TStream.Create;

 Try
  LabelList := TBCLabelList.Create;

  FunctionList := TBCFunctionList.Create;
  TypeList     := TBCTypeList.Create;
  VarList      := TBCVariableList.Create;
  LineDataList := TBCLineDataList.Create;

  Try
   // prepare label and function list
   For BCLabel in BCCompiler.LabelList Do
   Begin
    // skip private labels
    if (not BCLabel.isPublic) Then
     Break;

    // add it into the label list
    LabelList.Add(BCLabel);

    // ... and check if it's a function label
    if (BCLabel.isFunction) and (BCLabel.FunctionSymbol.Visibility = mvPublic) Then
    Begin
     BCFunction.Signature := BCLabel.Name;
     BCFunction.Position  := BCLabel.Position;

     FunctionList.Add(BCFunction);
    End;
   End;

   // prepare type and variable list
   For Namespace in SSCompiler.NamespaceList Do
   Begin
    For Symbol in Namespace.SymbolList Do
    Begin
     // if type
     if (Symbol.Typ = stType) Then // @TODO: why save internal types?
     Begin
      BCType.Name      := Symbol.getFullName;
      BCType.Signature := Symbol.mType.getSerializedForm;

      TypeList.Add(BCType);
     End Else

     // if variable
     if (Symbol.Typ = stVariable) and (not Symbol.isInternal) Then
     Begin
      BCVariable.Name      := Symbol.getFullName;
      BCVariable.Signature := Symbol.mVariable.getSerializedForm;

      VarList.Add(BCVariable);
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
   LineDataList.Free;
  End;

  // save ZIP
  AddFile(BCCompiler.HeaderStream, '.header');
  AddFile(DataStream, '.ssm_data');
  AddFile(BCCompiler.ReferenceStream, '.references');
  AddFile(BCCompiler.BytecodeStream, '.bytecode');

  Zip.FileName := FileName;
  Zip.ZipAllFiles;
 Finally
  // free classes
  DataStream.Free;
  Zip.Free;

  // remove unused files
  DeleteFile(FileName+'.header');
  DeleteFile(FileName+'.ssm_data');
  DeleteFile(FileName+'.references');
  DeleteFile(FileName+'.bytecode');
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
 AStream.ReadByte; // is_runnable
 version_major := AStream.read_uint8;
 version_minor := AStream.read_uint8;

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
  Log('Invalid SSM file version: '+IntToStr(SSMData.SSM_version)+', expecting: '+IntToStr(SSM_version));
  LoadOK := False;
  Exit;
 End;

 With SSMData do
 Begin
  // read few numbers
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

  if (AStream.Can) Then
   Log('This SSM file is possibly broken - some unread data are still in the ''.ssm_data'' (%d bytes left)', [AStream.Size-AStream.Position]);
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
    MOpcode       : PMOpcode;

    FunctionList: TBCFunctionList;
    LabelList   : TBCLabelList;

    I, ParamC: int32;
Begin
 // prepare variables
 FunctionList := SSMData.FunctionList;
 LabelList    := SSMData.LabelList;

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

  // if no function, maybe some label
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
   MOpcode^.isFunction := False;
   MOpcode^.isLabel    := True;
   MOpcode^.Name       := LabelName;
   OpcodeList.Add(MOpcode);
  End;

  // read and prepare opcode
  New(MOpcode);
  MOpcode^.Opcode     := TOpcode_E(AStream.read_uint8);
  MOpcode^.Compiler   := nil;
  MOpcode^.isComment  := False;
  MOpcode^.isLabel    := False;
  MOpcode^.isFunction := False;
  MOpcode^.Token      := nil;

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
 Log('Reading SSM archive file: '+AItem.ArchiveFileName);

 AStream.Position := 0;

 NStream := TStream(AStream);

 Case AItem.ArchiveFileName of
  '.header'    : ReadHeader(NStream);
  '.ssm_data'  : ReadSSMData(NStream);
  '.references': ReadReferences(NStream);
  '.bytecode'  : ReadOpcodes(NStream);

  else
   raise ESSMParserException.CreateFmt('Unknown archive file name: %s', [AItem.ArchiveFileName]);
 End;

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
Var Param: int8;

    BCFunction: TBCFunction;
    Data      : TUnserializer;

    mFunction: TFunction;

    ParamListNode: Serialization.TNode;

    FuncNamespace                        : TNamespace;
    NamespaceName, FunctionName, TypeName: String;
Begin
 // log
 Log('Parsing functions (%d to parse)...', [SSMData.FunctionCount]);

 // check if we have anything to parse
 if (SSMData.FunctionCount = 0) Then
  Exit;

 // iterate each function
 For BCFunction in SSMData.FunctionList Do
 Begin
  Data := TUnserializer.Create(BCFunction.Signature);

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

   mFunction.RefSymbol.Name          := FunctionName;
   mFunction.RefSymbol.DeclNamespace := FuncNamespace;
  // mFunction.RefVar @TODO

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
  Finally
   Data.Free;
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

(* TSSMReader.findType *)
{
 Returns type with specified name and namespace (or nil, if such wasn't be found).
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
 Returns namespace with specified name (or nil, if such wasn't have been found).
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
Constructor TSSMReader.Create(const fFileName: String);
Begin
 FileName      := fFileName;
 OpcodeList    := TOpcodeList.Create;
 NamespaceList := TNamespaceList.Create;
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
