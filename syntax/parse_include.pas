(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_include;

 Interface
 Uses Classes, SysUtils;

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses Logging, SSCompiler, Tokens, Messages, SSMParser, symdef;

(* isZipFile *)
Function isZipFile(const FileName: String): Boolean;
Var Stream: TFileStream;
    Header: Array[0..3] of uint8;
    I     : uint8;
Begin
 Stream := TFileStream.Create(FileName, fmOpenRead);

 Try
  For I := Low(Header) To High(Header) Do
   Header[I] := Stream.ReadByte;

  Result := (Header[0] = $50) and (Header[1] = $4B) and // scan first 4 bytes of file for the ZIP header
            (Header[2] = $03) and (Header[3] = $04);
 Finally
  Stream.Free;
 End;
End;

(* IncludeLibrary *)
Procedure IncludeLibrary(const Compiler: TCompiler; const FileName: String);
Var Reader                       : TSSMReader;
    ParentNamespace, NewNamespace: TNamespace;
    NewSymbol                    : TSymbol;
    I                            : int32;
Begin
 // create reader instance
 Reader := TSSMReader.Create(Compiler, FileName);

 // try to load
 if (not Reader.Load) Then
 Begin
  Compiler.CompileError(eCorruptedSSMFile, [FileName]);
  Exit;
 End;

 { copy symbols }
 // each namespace
 For NewNamespace in Reader.getNamespaceList Do
 Begin
  ParentNamespace := Compiler.findNamespace(NewNamespace.RefSymbol.Name);

  if (ParentNamespace = nil) Then
  Begin
   ParentNamespace := TNamespace.Create;
   Compiler.NamespaceList.Add(ParentNamespace);

   With ParentNamespace.RefSymbol do
   Begin
    Name       := NewNamespace.RefSymbol.Name;
    Visibility := mvPrivate;
    mCompiler  := Compiler;
    Range      := Compiler.getScanner.getCurrentRange;
    DeclToken  := Compiler.getScanner.next_pnt(0);
   End;
  End;

  // each symbol
  For NewSymbol in NewNamespace.SymbolList Do
  Begin
   NewSymbol.Visibility    := mvPrivate; // imported symbols have to be private
   NewSymbol.Range         := Compiler.getScanner.getCurrentRange;
   NewSymbol.DeclNamespace := ParentNamespace;

   ParentNamespace.SymbolList.Add(NewSymbol);
  End;
 End;

 // copy bytecode
 if (Reader.getOpcodeList.Count > 0) Then
 Begin
  For I := 0 To Reader.getOpcodeList.Count-1 Do
   Compiler.OpcodeList.Add(Reader.getOpcodeList[I]);
 End;

 Compiler.SSMReaderList.Add(Reader);
End;

(* IncludeModule *)
Procedure IncludeModule(const Parent, Module: TCompiler);
Var ParentNamespace, NewNamespace, TmpNamespace: TNamespace;
    Symbol, Copy                               : TSymbol;
    AddSymbol                                  : Boolean;
    I                                          : uint32;
Begin
 { copy symbols }
 // each namespace
 For NewNamespace in Module.NamespaceList Do
 Begin
  if (NewNamespace.RefSymbol.Visibility = mvPrivate) Then // ... except the private ones
   Continue;

  TmpNamespace := Parent.findNamespace(NewNamespace.RefSymbol.Name);

  if (TmpNamespace = nil) Then
  Begin
   ParentNamespace := TNamespace.Create;
   Parent.NamespaceList.Add(ParentNamespace);

   With ParentNamespace.RefSymbol do
   Begin
    Name       := NewNamespace.RefSymbol.Name;
    Visibility := mvPrivate;
    mCompiler  := Module;
    DeclToken  := NewNamespace.RefSymbol.DeclToken;
   End;
  End Else
  Begin
   ParentNamespace := TmpNamespace;
  End;

  // each symbol
  For Symbol in NewNamespace.SymbolList Do
  Begin
   if (Symbol.Visibility = mvPrivate) or (Symbol.isInternal) or (ParentNamespace.findSymbol(Symbol.Name) <> nil) Then // but skip private, internal and redeclared ones
    Continue;

   AddSymbol := False;

   Case Symbol.Typ of
    stConstant, stVariable, stType:
     AddSymbol := True;

    stFunction:
     AddSymbol := (Symbol.mFunction.ModuleName = Module.ModuleName);
   End;

   if (AddSymbol) Then
   Begin
    Copy            := TSymbol.Create(Symbol);
    Copy.Visibility := mvPrivate; // imported symbols have to be `private` (it's a copy, so modyfing this flag won't affect the original symbol).
    Copy.Range      := Parent.getScanner.getCurrentRange;

    Copy.getSymdefObject.RefSymbol := Copy;

    ParentNamespace.SymbolList.Add(Copy);
   End;
  End;
 End;

 { copy bytecode }
 if (Module.OpcodeList.Count > 0) Then
 Begin
  For I := 0 To Module.OpcodeList.Count-1 Do
   Parent.OpcodeList.Add(Module.OpcodeList[I]);
 End;
End;

(* IncludeModule *)
Procedure IncludeModule(const Compiler: TCompiler; const FileName: String; const CircularRef: Boolean; const CpTmp: TCompiler);
Var NewC: TCompiler;
Begin
 NewC := TCompiler.Create;

 NewC.CompileCode(FileName, FileName+'.ssc', True, CircularRef, Compiler.Parent, Compiler, CpTmp);

 if (not CircularRef) Then
 Begin
  With Compiler do
  Begin
   SetLength(IncludeList^, Length(IncludeList^)+1);
   IncludeList^[High(IncludeList^)] := NewC;
  End;
 End;

 IncludeModule(Compiler, NewC);
End;

// -------------------------------------------------------------------------- //
(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Var Compiler: TCompiler absolute CompilerPnt;
    FileName: String;
    Found   : Boolean;
    I       : int32;

    CircularRef      : Boolean;
    CpPrevious, CpTmp: TCompiler;
Begin
 With Compiler, getScanner do
 Begin
  eat(_BRACKET1_OP); // (
  FileName := ReplaceDirSep(read.Value); // [string]
  eat(_BRACKET1_CL); // )

  if (CompilePass <> _cp2) Then
   Exit;

  Log('Including file: '+FileName);

  { search for specified file }
  FileName := SearchFile(FileName, Found);

  if (not Found) Then // error: included file does not exist!
  Begin
   CompileError(eUnknownInclude, [FileName]);
   Exit;
  End;

  Log('Included file found at: '+FileName);

  { check for circular references }
  CircularRef := False;

  CpPrevious := nil;
  CpTmp      := Compiler;

  Repeat
   CircularRef := CircularRef or (CpTmp.InputFile = FileName);

   if (CircularRef) Then
    Break;

   CpPrevious := CpTmp;
   CpTmp      := CpTmp.Supervisor;
  Until (CpTmp = nil) or (CpTmp = CpPrevious);

  if (not CircularRef) Then
   CpTmp := nil;

  { check if the file hasn't been already parsed }
  Found := False;
  if (Length(IncludeList^) > 0) Then
   For I := 0 To High(IncludeList^) Do
   Begin
    if (IncludeList^[I].InputFile = FileName) Then
    Begin
     Found := True;
     Break;
    End;
   End;

  if (Found) Then
  Begin
   Log('File had been compiled before - using the already compiled version...');
   IncludeModule(Compiler, IncludeList^[I]);
  End Else
  Begin
   // first compilation of this file
   if (isZipFile(FileName)) Then
    IncludeLibrary(Compiler, FileName) Else
    IncludeModule(Compiler, FileName, CircularRef, CpTmp);
  End;
 End;
End;
End.
