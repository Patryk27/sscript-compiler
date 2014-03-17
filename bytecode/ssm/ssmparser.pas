(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit SSMParser;

 Interface
 Uses SSCompiler, BCCompiler, Opcodes, Messages, symdef, Classes, Stream, Zipper;

 Const SSM_version: uint16 = 2;

 { TBCFunction }
 Type TBCFunction =
      Record
       Signature: String;
       Position : uint32;
      End;

 { TBCType }
 Type TBCType =
      Record
       Name, Signature: String; // 'Signature' is basically serialized form of the type
      End;

 { TBCVariable }
 Type TBCVariable =
      Record
       Name, Signature: String;
      End;

 { TSSMData }
 Type TSSMData =
      Record
       SSM_version: uint16;

       LabelCount: uint32;
       LabelList : Array of TBCLabel;

       FunctionCount: uint32;
       FunctionList : Array of TBCFunction;

       TypeCount: uint32;
       TypeList : Array of TBCType;

       VarCount: uint32;
       VarList : Array of TBCVariable;
      End;

 { TSSMWriter }
 Type TSSMWriter =
      Class
       Private { fields }
        FileName: String;

        Zip: TZipper;

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

        Function findNamespace(const Name: String): TNamespace;

       Public { methods }
        Constructor Create(const fFileName: String);
        Function Load: Boolean;

        Property getFileName: String read FileName;
        Property getNamespaceList: TNamespaceList read NamespaceList;
        Property getOpcodeList: TOpcodeList read OpcodeList;
       End;

 Implementation
Uses SysUtils, Variants, Logging, Serialization;

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

Var DataStream  : TStream;
    LabelList   : Array of TBCLabel;
    FunctionList: Array of TBCFunction;
    TypeList    : Array of TBCType;
    VarList     : Array of TBCVariable;
    I           : int32; // keep this variable "signed"
    Namespace   : TNamespace;
    Symbol      : TSymbol;
Begin
 Log('Saving a SSM file to: '+FileName);

 // create classes
 Zip        := TZipper.Create;
 DataStream := TStream.Create;

 Try
  SetLength(LabelList, 0);
  SetLength(FunctionList, 0);
  SetLength(TypeList, 0);
  SetLength(VarList, 0);

  // prepare label list
  For I := Low(BCCompiler.LabelList) To High(BCCompiler.LabelList) Do
   if (BCCompiler.LabelList[I].isPublic) and (not BCCompiler.LabelList[I].isFunction) Then
   Begin
    SetLength(LabelList, Length(LabelList)+1);
    LabelList[High(LabelList)].Name     := BCCompiler.LabelList[I].Name;
    LabelList[High(LabelList)].Position := BCCompiler.LabelList[I].Position;
   End;

  // prepare function list
  For I := Low(BCCompiler.LabelList) To High(BCCompiler.LabelList) Do
   if (BCCompiler.LabelList[I].isPublic) and (BCCompiler.LabelList[I].isFunction) and (BCCompiler.LabelList[I].FunctionSymbol.mFunction.RefSymbol.Visibility = mvPublic) Then
   Begin
    SetLength(FunctionList, Length(FunctionList)+1);
    FunctionList[High(FunctionList)].Signature := BCCompiler.LabelList[I].Name;
    FunctionList[High(FunctionList)].Position  := BCCompiler.LabelList[I].Position;
   End;

  // prepare type list
  For Namespace in SSCompiler.NamespaceList Do
   For Symbol in Namespace.SymbolList Do
    if (Symbol.Typ = stType) Then
    Begin
     SetLength(TypeList, Length(TypeList)+1);
     TypeList[High(TypeList)].Name      := Symbol.getFullName;
     TypeList[High(TypeList)].Signature := Symbol.mType.getSerializedForm;
    End;

  // prepare var list
  For Namespace in SSCompiler.NamespaceList Do
   For Symbol in Namespace.SymbolList Do
    if (Symbol.Typ = stVariable) and (not Symbol.isInternal) Then
    Begin
     SetLength(VarList, Length(VarList)+1);
     VarList[High(VarList)].Name      := Symbol.getFullName;
     VarList[High(VarList)].Signature := Symbol.mVariable.getSerializedForm;
    End;

  // write SSM data
  With DataStream do
  Begin
   write_uint16(SSM_version);

   write_uint32(Length(LabelList));
   write_uint32(Length(FunctionList));
   write_uint32(Length(TypeList));
   write_uint32(Length(VarList));

   For I := Low(LabelList) To High(LabelList) Do
   Begin
    write_string(LabelList[I].Name);
    write_uint32(LabelList[I].Position);
   End;

   For I := Low(FunctionList) To High(FunctionList) Do
   Begin
    write_string(FunctionList[I].Signature);
    write_uint32(FunctionList[I].Position);
   End;

   For I := Low(TypeList) To High(TypeList) Do
   Begin
    write_string(TypeList[I].Name);
    write_string(TypeList[I].Signature);
   End;

   For I := Low(VarList) To High(VarList) Do
   Begin
    write_string(VarList[I].Name);
    write_string(VarList[I].Signature);
   End;
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
Var I: uint32;
Begin
 SSMData.SSM_version := AStream.read_uint16; // read version

 { check version }
 if (SSMData.SSM_version <> SSM_version) Then
 Begin
  Log('Invalid SSM file version: '+IntToStr(SSMData.SSM_version)+', expecting: '+IntToStr(SSM_version));
  LoadOK := False;
  Exit;
 End;

 With SSMData do
 Begin
  LabelCount    := AStream.read_uint32;
  FunctionCount := AStream.read_uint32;
  TypeCount     := AStream.read_uint32;
  VarCount      := AStream.read_uint32;

  Log(IntToStr(LabelCount)+' labels to be read.');
  Log(IntToStr(FunctionCount)+' functions to be read.');
  Log(IntToStr(TypeCount)+' types to be read.');
  Log(IntToStr(VarCount)+' variables to be read.');

  // allocate arrays
  SetLength(LabelList, LabelCount);
  SetLength(FunctionList, FunctionCount);
  SetLength(TypeList, TypeCount);
  SetLength(VarList, VarCount);

  // read label list
  if (LabelCount > 0) Then
   For I := 0 To LabelCount-1 Do
   Begin
    LabelList[I].Name     := AStream.read_string;
    LabelList[I].Position := AStream.read_uint32;

    DevLog(dvInfo, 'TSSMReader.ReadHeader', 'Label #'+IntToStr(I)+' -> name='''+LabelList[I].Name+'''; position=0x'+IntToHex(LabelList[I].Position, 8));
   End;

  // read function list
  if (FunctionCount > 0) Then
   For I := 0 To FunctionCount-1 Do
   Begin
    FunctionList[I].Signature := AStream.read_string;
    FunctionList[I].Position  := AStream.read_uint32;

    DevLog(dvInfo, 'TSSMReader.ReadHeader', 'Function #'+IntToStr(I)+' -> signature='''+FunctionList[I].Signature+'''; position=0x'+IntToHex(FunctionList[I].Position, 8));
   End;

  // read type list
  if (TypeCount > 0) Then
   For I := 0 To TypeCount-1 Do
   Begin
    TypeList[I].Name      := AStream.read_string;
    TypeList[I].Signature := AStream.read_string;

    DevLog(dvInfo, 'TSSMReader.ReadHeader', 'Type #'+IntToStr(I)+' -> name='''+TypeList[I].Name+'''; signature='''+TypeList[I].Signature+'''');
   End;

  // read variable list
  if (VarCount > 0) Then
   For I := 0 To VarCount-1 Do
   Begin
    VarList[I].Name      := AStream.read_string;
    VarList[I].Signature := AStream.read_string;

    DevLog(dvInfo, 'TSSMReader.ReadHeader', 'Variable #'+IntToStr(I)+' -> name='''+VarList[I].Name+'''; signature='''+VarList[I].Signature+'''');
   End;

  if (AStream.Can) Then
   Log('This SSM file is possibly broken - some unread data are still in the ''.ssm_data'' ('+IntToStr(AStream.Size-AStream.Position)+' bytes left)');
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
Var I, ParamC: int32;
    MOpcode  : PMOpcode;
    LabelName: String;
Begin
 { read opcodes }
 While (AStream.Can) Do
 Begin
  LabelName := '';

  For I := 0 To High(SSMData.FunctionList) Do
   if (SSMData.FunctionList[I].Position = AStream.Position) Then // function label declaration
   Begin
    LabelName := SSMData.FunctionList[I].Signature;
    Break;
   End;

  if (Length(LabelName) = 0) Then
   For I := 0 To High(SSMData.LabelList) Do
    if (SSMData.LabelList[I].Position = AStream.Position) Then // some public label declaration
    Begin
     LabelName := SSMData.LabelList[I].Name;
     Break;
    End;

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
   For I := 0 To ParamC-1 Do
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
   raise Exception.CreateFmt('Unknown archive file name: %s', [AItem.ArchiveFileName]);
 End;

 AStream.Free;
End;

(* TSSMReader.ParseTypes *)
Procedure TSSMReader.ParseTypes;
Var ID                     : uint32;
    Data                   : TUnserializer;
    mType                  : TType;
    mNamespace             : TNamespace;
    NamespaceName, TypeName: String;
Begin
 Log('Parsing types...');

 if (SSMData.TypeCount = 0) Then
  Exit;

 With SSMData do
 Begin
  For ID := Low(TypeList) To High(TypeList) Do
  Begin
   SplitByDot(TypeList[ID].Name, NamespaceName, TypeName); // namespace_name . type_name

   mNamespace := findNamespace(NamespaceName);
   if (mNamespace = nil) Then
   Begin
    mNamespace                := TNamespace.Create;
    mNamespace.RefSymbol.Name := NamespaceName;
    NamespaceList.Add(mNamespace);
   End;

   Data := TUnserializer.Create(TypeList[ID].Signature);

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
End;

(* TSSMReader.ParseFunctions *)
Procedure TSSMReader.ParseFunctions;
Var ID, Q                                : uint32;
    Data                                 : TUnserializer;
    mSymbol                              : TSymbol;
    mFunction                            : TFunction;
    FuncNamespace, TmpNamespace          : TNamespace;
    NamespaceName, FunctionName, TypeName: String;
Begin
 Log('Parsing functions...');

 if (SSMData.FunctionCount = 0) Then
  Exit;

 With SSMData do
 Begin
  For ID := Low(FunctionList) To High(FunctionList) Do
  Begin
   Data := TUnserializer.Create(FunctionList[ID].Signature);

   Try
    if (Data.getRoot[0].getString <> 'function') Then
     raise Exception.Create('TSSMReader.ParseFunctions() failed -> expected a function signature.');

    SplitByDot(Data.getRoot[1].getString, NamespaceName, FunctionName); // namespace_name . function_name

    FuncNamespace := findNamespace(NamespaceName);
    if (FuncNamespace = nil) Then
    Begin
     FuncNamespace                := TNamespace.Create;
     FuncNamespace.RefSymbol.Name := NamespaceName;
     NamespaceList.Add(FuncNamespace);
    End;

    mFunction                         := TFunction.Create; // create a new function
    mFunction.LabelName               := FunctionList[ID].Signature;
    mFunction.RefSymbol.Name          := FunctionName;
    mFunction.RefSymbol.DeclNamespace := FuncNamespace;
   // mFunction.RefVar @TODO

    { parse return type }
    if (Data.getRoot[3].getType = ntValue) Then
    Begin
     SplitByDot(Data.getRoot[3].getString, NamespaceName, TypeName); // namespace_name . type_name

     TmpNamespace := findNamespace(NamespaceName); // find namespace
     if (TmpNamespace = nil) Then
      raise Exception.CreateFmt('TSSMReader.ParseFunctions() failed -> unknown namespace: ''%s'' (tried to parse return type ''%s'' of function ''%s'')', [NamespaceName, TypeName, FunctionName]);

     mSymbol := TmpNamespace.findSymbol(TypeName); // find symbol
     if (mSymbol = nil) Then
      raise Exception.CreateFmt('TSSMReader.ParseFunctions() failed -> unknown type: ''%s'' (tried to parse return type of function ''%s'')', [TypeName, FunctionName]);

     mFunction.Return := mSymbol.mType;
    End Else
    Begin
     mFunction.Return := TType.Create(Data.getRoot[3]);
    End;

    { parse parameter list }
    SetLength(mFunction.ParamList, Data.getRoot[4].getInt);
    if (Length(mFunction.ParamList) > 0) Then
     For Q := 0 To High(mFunction.ParamList) Do
     Begin
      if (Data.getRoot[5][Q].getType = ntValue) Then
      Begin
       SplitByDot(Data.getRoot[5][Q].getString, NamespaceName, TypeName);

       TmpNamespace := findNamespace(NamespaceName); // find namespace
       if (TmpNamespace = nil) Then
        raise Exception.CreateFmt('TSSMReader.ParseFunctions() failed -> unknown namespace: ''%s'' (tried to parse parameter type #%d of function ''%s'')', [NamespaceName, Q, FunctionName]);

       mSymbol := TmpNamespace.findSymbol(TypeName); // find symbol
       if (mSymbol = nil) Then
        raise Exception.CreateFmt('TSSMReader.ParseFunctions() failed -> unknown type: ''%s'' (tried to parse parameter #%d of function ''%s'')', [TypeName, Q, FunctionName]);

       mFunction.ParamList[Q].Typ := mSymbol.mType;
      End Else
      Begin
       mFunction.ParamList[Q].Typ := TType.Create(Data.getRoot[5][Q]);
      End;
     End;

    { add this function to the symbol list }
    FuncNamespace.SymbolList.Add(TSymbol.Create(stFunction, mFunction));
   Finally
    Data.Free;
   End;
  End;
 End;
End;

(* TSSMReader.ParseVariables *)
Procedure TSSMReader.ParseVariables;
Var ID                              : uint32;
    Data                            : TUnserializer;
    mSymbol                         : TSymbol;
    mVariable                       : TVariable;
    TmpNamespace, VarNamespace      : TNamespace;
    NamespaceName, VarName, TypeName: String;
Begin
 Log('Parsing variables...');

 if (SSMData.VarCount = 0) Then
  Exit;

 With SSMData do
 Begin
  For ID := Low(VarList) To High(VarList) Do
  Begin
   SplitByDot(VarList[ID].Name, NamespaceName, VarName); // namespeace_name . variable_name

   VarNamespace := findNamespace(NamespaceName);
   if (VarNamespace = nil) Then
   Begin
    VarNamespace                := TNamespace.Create;
    VarNamespace.RefSymbol.Name := NamespaceName;
    NamespaceList.Add(VarNamespace);
   End;

   Data := TUnserializer.Create(VarList[ID].Signature);

   Try
    mVariable                         := TVariable.Create(Data);
    mVariable.RefSymbol.Name          := VarName;
    mVariable.RefSymbol.DeclNamespace := VarNamespace;

    if (mVariable.Typ = nil) Then
    Begin
     { parse variable type }
     SplitByDot(Data.getRoot[2].getString, NamespaceName, TypeName); // namespace_name . type_name

     TmpNamespace := findNamespace(NamespaceName);
     if (TmpNamespace = nil) Then
      raise Exception.CreateFmt('TSSMReader.ParseVariables() failed -> unknown namespace: ''%s'' (tried to parse type ''%s'' of variable ''%s'')', [NamespaceName, TypeName, VarName]);

     mSymbol := TmpNamespace.findSymbol(TypeName);
     if (mSymbol = nil) Then
      raise Exception.CreateFmt('TSSMReader.ParseVariables() failed -> unknown symbol: ''%s'' (tried to parse type of variable ''%s'')', [TypeName, VarName]);

     mVariable.Typ := mSymbol.mType;
    End;

    mVariable.LocationData.Location      := vlMemory;
    mVariable.LocationData.MemSymbolName := VarList[ID].Signature;

    VarNamespace.SymbolList.Add(TSymbol.Create(stVariable, mVariable));
   Finally
    Data.Free;
   End;
  End;
 End;
End;

(* TSSMReader.findNamespace *)
Function TSSMReader.findNamespace(const Name: String): TNamespace;
Begin
 For Result in NamespaceList Do
  if (Result.RefSymbol.Name = Name) Then
   Exit;

 Exit(nil);
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
