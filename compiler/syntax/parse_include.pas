(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_include;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, CompilerUnit, ExpressionCompiler, Tokens, Messages, MTypes, SysUtils;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var FileName         : String;
    NewC             : TCompiler;
    NS, I, Q         : LongWord;
    Found            : Boolean;
    TmpNamespace, Tmp: Integer;
Begin
With TCompiler(Compiler) do
Begin
 eat(_BRACKET1_OP); // (

 FileName := read.Display;
 Log('Including file: '+FileName);

 eat(_BRACKET1_CL); // )

 { search file }
 FileName := SearchFile(FileName, Found);
 if (not Found) Then
 Begin
  CompileError(eUnknownInclude, [FileName]); // error: not found!
  Exit;
 End;

 Log('Found file: '+FileName);

 { compile that file }
 NewC := TCompiler.Create;

 SetLength(IncludeList, Length(IncludeList)+1);
 IncludeList[High(IncludeList)] := NewC;

 NewC.CompileCode(FileName, FileName+'.ssc', Options, True, Parent);

 { for each namespace }
 Log('Including...');
 TmpNamespace := CurrentNamespace;
 For NS := Low(NewC.NamespaceList) To High(NewC.NamespaceList) Do
 Begin
  if (NewC.NamespaceList[NS].Visibility <> mvPublic) Then
   Continue;

  Log('Including namespace: '+NewC.NamespaceList[NS].Name);

  Tmp := findNamespace(NewC.NamespaceList[NS].Name);
  if (Tmp = -1) Then // new namespace
  Begin
   Log('Included namespace is new in current scope.');
   SetLength(NamespaceList, Length(NamespaceList)+1);

   CurrentNamespace := High(NamespaceList);
   With NamespaceList[CurrentNamespace] do
   Begin
    Name       := NewC.NamespaceList[NS].Name;
    Visibility := mvPrivate;
    mCompiler  := NewC;
    DeclToken  := NewC.NamespaceList[NS].DeclToken;
    SetLength(GlobalList, 0);
   End;
  End Else // already existing namespace
  Begin
   Log('Included namespace extends another one.');
   CurrentNamespace := Tmp;
  End;

  With NewC.NamespaceList[NS] do
  Begin
   if (Length(GlobalList) = 0) Then
    Continue;

   { global constants }
   For I := Low(GlobalList) To High(GlobalList) Do
    With GlobalList[I] do
    Begin
     if (Typ <> gdConstant) Then // skip not-constants
      Continue;

     With mVariable do
     Begin
      if (Visibility <> mvPublic) Then // constant must be public
       Continue;

      RedeclarationCheck(Name);

      With getCurrentNamespacePnt^ do
       SetLength(GlobalList, Length(GlobalList)+1);

      With getCurrentNamespacePnt^.GlobalList[High(getCurrentNamespace.GlobalList)] do
      Begin
       Typ       := gdConstant;
       mVariable := GlobalList[I].mVariable;
      End;
     End;
    End;

   { functions }
   For I := Low(GlobalList) To High(GlobalList) Do
    With GlobalList[I] do
    Begin
     if (Typ <> gdFunction) Then
      Continue;

     With mFunction do
     Begin
      if (Visibility <> mvPublic) Then // function must be public
       Continue;

      if ((ModuleName = NewC.ModuleName) and (LibraryFile = '')) or (LibraryFile <> '') Then // don't copy functions imported from other modules (as they are useless for us)
      Begin
       RedeclarationCheck(Name);

       With getCurrentNamespacePnt^ do
        SetLength(GlobalList, Length(GlobalList)+1);

       With getCurrentNamespacePnt^.GlobalList[High(getCurrentNamespace.GlobalList)] do
       Begin
        Typ       := gdFunction;
        mFunction := GlobalList[I].mFunction;

        mVariable.Name    := mFunction.Name;
        mVariable.Typ     := NewTypeFromFunction(mFunction);
        mVariable.Value   := MakeIntExpression('@'+mFunction.MName);
        mVariable.RegChar := 'r';
        mVariable.RegID   := 1;
        mVariable.isConst := True;
       End;
      End;
     End;
    End;
  End;
 End;
 CurrentNamespace := TmpNamespace;

 { bytecode }
 if (NewC.OpcodeList.Count > 0) Then
  For I := 0 To NewC.OpcodeList.Count-1 Do
   OpcodeList.Add(NewC.OpcodeList[I]);

 { string list }
 Q := High(StringList)+1;
 SetLength(StringList, Length(StringList)+Length(NewC.StringList));

 if (Length(NewC.StringList) > 0) Then
  For I := Low(NewC.StringList) To High(NewC.StringList) Do
  Begin
   StringList[Q] := NewC.StringList[I];
   Inc(Q);
  End;
End;
End;
End.
