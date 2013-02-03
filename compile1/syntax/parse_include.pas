Unit Parse_include;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, Tokens, Messages, MTypes, SysUtils;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var FileName: String;
    NewC    : TCompiler;
    I, Q    : LongWord;
    Found   : Boolean;
Begin
With TCompiler(Compiler) do
Begin
 eat(_BRACKET1_OP); // (

 FileName := read.Display;

 eat(_BRACKET1_CL); // )

 FileName := SearchFile(FileName, Found);

 if (not Found) Then
 Begin
  CompileError(eUnknownInclude, [FileName]);
  Exit;
 End;

 NewC := TCompiler.Create;

 SetLength(IncludeList, Length(IncludeList)+1);
 IncludeList[High(IncludeList)] := NewC;

 NewC.CompileCode(FileName, FileName+'.ssc', Options, True, Parent); // compile! ;)

 // new constants
 if (Length(NewC.ConstantList) > 0) Then
  For I := Low(NewC.ConstantList) To High(NewC.ConstantList) Do
   With NewC.ConstantList[I] do
   Begin
    if (Visibility <> mvPublic) Then
     Continue;

    SetLength(ConstantList, Length(ConstantList)+1);
    ConstantList[High(ConstantList)] := NewC.ConstantList[I];
   End;

 // new functions
 if (Length(NewC.FunctionList) > 0) Then
  For I := Low(NewC.FunctionList) To High(NewC.FunctionList) Do
   if (NewC.FunctionList[I].Visibility = mvPublic) Then
    With NewC.FunctionList[I] do
     if ((ModuleName = NewC.ModuleName) and (LibraryFile = '')) or (LibraryFile <> '') Then // we want only functions from this module
     Begin
      SetLength(FunctionList, Length(FunctionList)+1);
      FunctionList[High(FunctionList)] := NewC.FunctionList[I];
     End;

 // copy bytecode
 if (NewC.OpcodeList.Count > 0) Then
  For I := 0 To NewC.OpcodeList.Count-1 Do
   OpcodeList.Add(NewC.OpcodeList[I]);

 // copy string list
 Q := High(StringList)+1;
 SetLength(StringList, Length(StringList)+Length(NewC.StringList));

 if (Length(NewC.StringList) > 0) Then
  For I := Low(NewC.StringList) To High(NewC.StringList) Do
   //if (findStringByContent(NewC.StringList[I].Value) = -1) Then // we don't want strings to be duplicated; it would be such a waste of space.
   Begin
    StringList[Q] := NewC.StringList[I];
    Inc(Q);
   End;
End;
End;
End.
