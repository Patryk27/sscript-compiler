(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_include;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, CompilerUnit, Tokens, Messages, symdef, SysUtils;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var FileName: String;
    NewC    : TCompiler;
    NS, I   : LongWord;
    Found   : Boolean;

    PrevNamespace, Tmp: TNamespace;

    Symbol, Copy: TSymbol;
    AddSymbol   : Boolean;

    CircRef: Boolean;
Begin
With TCompiler(Compiler), Parser do
Begin
 eat(_BRACKET1_OP); // (
 FileName := ReplaceDirSep(read.Value); // [string]
 eat(_BRACKET1_CL); // )

 if (CompilePass <> _cp3) Then
  Exit;

 Log('Including file: '+FileName);

 { search file }
 FileName := SearchFile(FileName, Found);
 if (not Found) Then
 Begin
  CompileError(eUnknownInclude, [FileName]); // error: not found!
  Exit;
 End;

 Log('Found file: '+FileName);

 CircRef := (FileName = InputFile) or ((Supervisor <> nil) and (Supervisor.InputFile = FileName)); // is a circular reference?

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

 if (not Found) Then
 Begin
  { compile file }
  NewC := TCompiler.Create;

  NewC.CompileCode(FileName, FileName+'.ssc', Options, True, CircRef, Parent, TCompiler(Compiler));

  if (not CircRef) Then
  Begin
   SetLength(IncludeList^, Length(IncludeList^)+1);
   IncludeList^[High(IncludeList^)] := NewC;
  End;
 End Else
 Begin
  Log('File had been compiled before - using the compiled version...');
  NewC := IncludeList^[I];
 End;

 { for each namespace }
 Log('Including...');
 PrevNamespace := CurrentNamespace;
 For NS := 0 To NewC.NamespaceList.Count-1 Do
 Begin
  if (NewC.NamespaceList[NS].RefSymbol.Visibility <> mvPublic) Then
   Continue;

  Log('Including namespace: '+NewC.NamespaceList[NS].RefSymbol.Name);

  Tmp := findNamespace(NewC.NamespaceList[NS].RefSymbol.Name);
  if (Tmp = nil) Then // new namespace
  Begin
   Log('Included namespace is new in current scope.');
   NamespaceList.Add(TNamespace.Create);

   CurrentNamespace := NamespaceList.Last;
   With NamespaceList.Last do
   Begin
    With RefSymbol do
    Begin
     Name       := NewC.NamespaceList[NS].RefSymbol.Name;
     Visibility := mvPrivate;
     mCompiler  := NewC;
     DeclToken  := NewC.NamespaceList[NS].RefSymbol.DeclToken;
    End;

    SymbolList := TSymbolList.Create;
   End;
  End Else // already existing namespace
  Begin
   Log('Included namespace extends another one.');
   CurrentNamespace := Tmp;
  End;

  With NewC.NamespaceList[NS] do
  Begin
   For Symbol in SymbolList Do // each symbol
    With Symbol do
    Begin
     if (Visibility = mvPrivate) or (isInternal) Then // skip `private` entries
      Continue;

     RedeclarationCheck(Name);

     AddSymbol := False;

     Case Typ of
      { constant, variable, type }
      stConstant, stVariable, stType:
       AddSymbol := True;

      { function }
      stFunction:
       With mFunction do
        AddSymbol := ((ModuleName = NewC.ModuleName) and (LibraryFile = '')) or (LibraryFile <> '');
     End;

     Copy            := TSymbol.Create(Symbol);
     Copy.Visibility := mvPrivate; // imported symbols have to be `private` (it's a copy, so modyfing this flag won't modify the original symbol).

     if (AddSymbol) Then
      getCurrentNamespace.SymbolList.Add(Copy);
    End;
  End;
 End;
 CurrentNamespace := PrevNamespace;

 { bytecode }
 if (NewC.OpcodeList.Count > 0) Then
  For I := 0 To NewC.OpcodeList.Count-1 Do
   OpcodeList.Add(NewC.OpcodeList[I]);
End;
End;
End.
