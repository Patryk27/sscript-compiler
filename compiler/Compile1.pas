(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Compile1;

 Interface
 Uses Classes, SysUtils, Variants, FGL, Math, cfgraph,
      Parser, Tokens, CompilerUnit, Opcodes, Messages, Expression, symdef,
      Parse_FUNCTION, Parse_VAR, Parse_CONST, Parse_RETURN, Parse_CODE, Parse_FOR, Parse_IF, Parse_WHILE, Parse_include, Parse_DELETE,
      Parse_NAMESPACE, Parse_TYPE, Parse_TRY_CATCH, Parse_THROW;

 { constants }
 Const Version = '2.2.2 nightly';
       vMajor  = 2.2;
       vMinor  = 2;

 { types }
 Type TCompiler = class;

 Type TOpcodeList = specialize TFPGList<PMOpcode>;
 Type TCompilePass = (_cp1, _cp2, _cp3); // fourth pass is the actual function compiling; as it's performed at the end of each function, there's no need for an additional `cp4` enum

 Type TCompilerArray = Array of TCompiler;
      PCompilerArray = ^TCompilerArray;

 Type TScopeType = (sFunction, sFOR, sIF, sWHILE, sTryCatch);

 Type TScope = Record
                Typ               : TScopeType;
                LoopBegin, LoopEnd: TCFGNode;
               End;

 (* TCompiler *)
 Type TCompiler = Class
                   Private
                    Procedure MakeImports;
                    Procedure CompileAsBytecode;
                    Procedure SaveBytecode(const FileName: String);

                   Public
                    Parser: TParser; // code parser

                    AnyError: Boolean; // 'true' if any error (`CompileError()`) was raised, 'false' otherwise.

                    fCurrentRoot: TCFGNode; // when equal 'nil', nodes added by `CFGAddNode` will be added to current function's flow graph. Otherwise - to this.
                    fCurrentNode: TCFGNode;

                    PrevRootNodes: TCFGNodeList;

                   Public
                    Parent      : TCompiler;
                    Supervisor  : TCompiler;
                    CompileMode : (cmApp, cmLibrary, cmBytecode); // compilation mode
                    CompilePass : TCompilePass;
                    InputFile   : String; // input file name
                    OutputFile  : String; // output file name
                    ModuleName  : String; // module name
                    Options     : TCompileOptions; // compile options
                    IncludePaths: TStringList; // list of include paths

                    CurrentFunction   : TFunction; // currently parsed (or compiled) function
                    CurrentNamespace  : Integer; // namespace in which we are (`namespace namespace_ame;`)
                    SelectedNamespaces: Array of Integer; // selected namespaces (`use namespace1, namespace2 (...);`)

                    OpcodeList   : TOpcodeList; // output code opcode list
                    IncludeList  : PCompilerArray; // global include list
                    NamespaceList: TNamespaceList; // namespace list
                    Scope        : Array of TScope; // current function scopes

                    SomeCounter: LongWord; // used in labels eg.`__while_<somecounter>_begin`, so they don't overwrite each other

                    DoNotGenerateCode: Boolean; // when equal `true`, any `PutOpcode` will not insert bytecode into the bytecode list. Affects also labels! Default: `false`.

                    ParsingFORInitInstruction: Boolean; // don't even ask...

               { -> properties }
                    Property getCurrentRoot: TCFGNode read fCurrentRoot;
                   // Property getCurrentNode: TCFGNode read fCurrentNode;
                    Function getCurrentNode: TCFGNode;

                    Property getCurrentFunction: TFunction read CurrentFunction;

               { -> methods }
                    Function getBoolOption(const Name: TCommandLineOption; oDefault: Boolean=False): Boolean; inline;
                    Function getStringOption(const Name: TCommandLineOption; oDefault: String=''): String; inline;
                    Function getIntOption(const Name: TCommandLineOption; oDefault: Integer): Integer; inline;

                    Function SearchFile(const FileName: String; out Found: Boolean): String;

                   { control flow graph }
                    Procedure setNewRootNode(NewRoot: TCFGNode; const SavePrevious: Boolean=True);
                    Procedure restorePrevRootNode;

                   { parser }
                    Procedure ParseToken;
                    Procedure SkipCodeBlock;
                    Procedure ParseCodeBlock(const AllowOneTokenOnly: Boolean=False);

                   { bytecode }
                    Function PutOpcode(fOpcode: TOpcode_E; fArgs: Array of Const; fToken: PToken_P=nil): PMOpcode;
                    Function PutOpcode(Opcode: TOpcode_E): PMOpcode;
                    Function PutOpcode(Opcode: String; Args: Array of Const; fToken: PToken_P=nil): PMOpcode;
                    Function PutLabel(fName: String; const asNode: Boolean=False): PMOpcode;
                    Procedure PutComment(fComment: String);

                    Function findLabel(Name: String): Integer;

                   { types }
                    Function NewTypeFromFunction(Func: TFunction): TType;

                    Function findLocalType(fName: String; fTokenPos: Int64=-1): Integer;
                    Function findGlobalType(const TypeName: String; NamespaceID: Integer=-1): TType;
                    Procedure findTypeCandidate(const TypeName: String; Namespaces: TIntegerArray; out TypeID, NamespaceID: Integer; const Token: PToken_P=nil);

                    Function getTypeFromExpr(Expr: TExpression): TType;

                   { scope }
                    Procedure NewScope(const Typ: TScopeType; LoopBegin: TCFGNode=nil; LoopEnd: TCFGNode=nil);
                    Procedure RemoveScope;
                    Function inRange(Range: TRange; Position: Int64=-1): Boolean;

                   { variables }
                    Procedure __variable_create(fName: String; fTyp: TType; fMemPos: Integer; fAttributes: TVariableAttributes);

                    Function FetchVariableValue(VariablePnt: TVariable): PExpression;

                    Function findLocalVariable(fName: String): Integer;
                    Procedure findGlobalVariableCandidate(const VarName: String; Namespaces: TIntegerArray; out VarID, NamespaceID: Integer; const Token: PToken_P=nil);

                   { functions }
                    Function findFunction(FuncName: String; NamespaceID: Integer=-1): Integer;
                    Procedure findFunctionByLabel(const LabelName: String; out FuncID, NamespaceID: Integer);

                    Procedure findFunctionCallCandidate(const FuncName: String; Namespaces: TIntegerArray; out FuncID, NamespaceID: Integer; const Token: PToken_P=nil);

                    Procedure CFGAddNode(Node: TCFGNode);

                   { namespaces }
                    Function getCurrentNamespace: TNamespace;
                    Function getDefaultNamespace: TNamespace;

                    Function findNamespace(Name: String): Integer;

                   { global-things }
                    Function inFunction: Boolean;

                    Function findGlobalVariable(VarName: String; NamespaceID: Integer=-1): Integer;
                    Procedure findGlobalCandidate(const IdentName: String; Namespaces: TIntegerArray; out IdentID, NamespaceID: Integer; const Token: PToken_P=nil);

                    Procedure RedeclarationCheck(Name: String; const SkipNamespaces: Boolean=False);

                   { compiling }
                    Procedure CompileCode(fInputFile, fOutputFile: String; fOptions: TCompileOptions; isIncluded: Boolean=False; Pass1Only: Boolean=False; fParent: TCompiler=nil; fSupervisor: TCompiler=nil);

                    Procedure CompileError(Token: TToken_P; Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Token: PToken_P; Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Error: TCompileError);

                    Procedure CompileWarning(Token: TToken_P; Warning: TCompileWarning; Args: Array of Const);
                    Procedure CompileWarning(Token: PToken_P; Warning: TCompileWarning; Args: Array of Const);
                    Procedure CompileWarning(Warning: TCompileWarning; Args: Array of Const);
                    Procedure CompileWarning(Warning: TCompileWarning);

                    Procedure CompileHint(Token: TToken_P; Hint: TCompileHint; Args: Array of Const);
                    Procedure CompileHint(Token: PToken_P; Hint: TCompileHint; Args: Array of Const);
                    Procedure CompileHint(Hint: TCompileHint; Args: Array of Const);
                    Procedure CompileHint(Hint: TCompileHint);

                    Procedure CompileNote(Token: TToken_P; Note: TCompileNote; Args: Array of Const);
                    Procedure CompileNote(Token: PToken_P; Note: TCompileNote; Args: Array of Const);
                    Procedure CompileNote(Note: TCompileNote; Args: Array of Const);
                    Procedure CompileNote(Note: TCompileNote);

                    Procedure GenerateHeaderFile(const fOutputFile: String);
                   End;

 Function ReplaceDirSep(FileName: String): String;
 Function makeModuleName(FileName: String): String;
 Function CopyStringToPChar(const Str: String): PChar;

 Implementation
Uses Compile2, ExpressionCompiler, SSM_parser, Peephole;

(* ReplaceDirSep *)
{
 Replaces `/` and/or `\` to the appropriate directory separator
}
Function ReplaceDirSep(FileName: String): String;
Const Sep = DirectorySeparator;
Begin
 FileName := StringReplace(FileName, '/', Sep, [rfReplaceAll]);
 FileName := StringReplace(FileName, '\', Sep, [rfReplaceAll]);
 FileName := StringReplace(FileName, Sep+Sep, Sep, [rfReplaceAll]);
 Exit(FileName);
End;

(* makeModuleName *)
{
 Creates a module (bytecode label) name based on FileName in parameter.
}
Function makeModuleName(FileName: String): String;
Var Ch: Char;
Begin
 Result := '';
 For Ch in FileName Do // create module name (used in bytecode labels)
  if (Ch in ['a'..'z', 'A'..'Z', '0'..'9', '_']) Then
   Result += Ch Else
   Result += '_';
End;

(* CopyStringToPChar *)
{
 Copies a String into PChar (also automatically allocates memory for the PChar string).
}
Function CopyStringToPChar(const Str: String): PChar;
Var I: Integer;
Begin
 Result := AllocMem(Length(Str)+1);

 For I := 1 To Length(Str) Do
  Result[I-1] := Str[I];
End;

// -------------------------------------------------------------------------- //
(* TCompiler.getCurrentNode *)
Function TCompiler.getCurrentNode: TCFGNode; // @TODO: is this function even used anywhere?
Begin
 if (fCurrentNode = nil) Then
 Begin
  if (fCurrentRoot = nil) Then
  Begin
   DevLog(dvError, 'TCompiler.getCurrentNode', 'everything is nil!');
   Result := nil;
  End Else
   Result := fCurrentRoot;
 End Else
  Result := fCurrentNode;
End;

(* TCompiler.MakeImports *)
{
 Creates import list (loads *.ssm files and adds them into the bytecode)
}
Procedure TCompiler.MakeImports;
Var SSM          : TSSM;
    I, Q         : Integer;
    Can, Found   : Boolean;
    FileName, Tmp: String;
    Comp         : Compile1.TCompiler;

    Namespace: TNamespace;
Begin
 { if `-initcode` specified }
 if (getBoolOption(opt_initcode)) Then
 Begin
  SSM := TSSM.Create;

  FileName := SearchFile('init.ssm', Found);

  if (not Found) Then
   FileName := SearchFile('stdlib\init.ssm', Found);

  if (not Found) Then
   CompileError(eFileNotFound, ['init.ssm']) Else
   Begin
    if (not SSM.Load(FileName, FileName, self, False)) Then
     CompileError(eCorruptedSSMFile, ['init.ssm']);
   End;
  SSM.Free;
 End;

 For Namespace in NamespaceList Do // each namespace
  With Namespace do
  Begin
   For I := 0 To SymbolList.Count-1 Do // each symbol
    if (SymbolList[I].Typ = gsFunction) and (SymbolList[I].mFunction.LibraryFile <> '') Then // is it an imported function?
    Begin
     Can := True;

     For Q := 0 To I-1 Do // we don't want some file to be loaded eg.10 times instead of 1 time (it just skips multiple imports from same file)
      if (SymbolList[Q].Typ = gsFunction) Then
       if (SymbolList[Q].mFunction.LibraryFile = SymbolList[I].mFunction.LibraryFile) Then
        Can := False;

     if (not Can) Then
      Continue; // proceed to the next file

     FileName := SymbolList[I].mFunction.LibraryFile;
     Comp     := TCompiler(SymbolList[I].mCompiler);

     FileName := SearchFile(FileName, Found); // search file

     if (not Found) Then // not found
     Begin
      Tmp := ExtractFilePath(Comp.InputFile)+FileName;
      if (FileExists(Tmp)) Then
       FileName := Tmp;
     End;

     SSM := TSSM.Create;
     if (not SSM.Load(FileName, Comp.ModuleName, self)) Then // try to load SSM
      Comp.CompileError(SymbolList[I].DeclToken, eCorruptedSSMFile, [SymbolList[I].mFunction.LibraryFile]);
     SSM.Free;
    End;
  End;
End;

(* TCompiler.CompileAsBytecode *)
{
 Compiles input file as a bytecode
}
Procedure TCompiler.CompileAsBytecode;
Var Compiler2: Compile2.TCompiler;
Begin
 Log('-> Compiling as bytecode');

 Parse_CODE.Parse(self, True); // parse bytecode

 Compiler2 := Compile2.TCompiler.Create;
 Compiler2.Compile(self, True);
 Compiler2.Free;
End;

(* TCompiler.SaveBytecode *)
{
 Saves verbal (mnemonic) bytecode into the file specified in parameter
}
Procedure TCompiler.SaveBytecode(const FileName: String);
Var OutputCode: TStringList;
    Opcode    : PMOpcode;
    Arg       : TMOpcodeArg;
    Reg, Str  : String;
Begin
 OutputCode := TStringList.Create;

 Log('-> Saving bytecode into file: '+FileName);

 // save bytecode
 OutputCode.Add('{');
 For Opcode in OpcodeList Do
  With Opcode^ do
  Begin
   { label }
   if (isLabel) Then
   Begin
    Case isPublic of
     True: OutputCode.Add(Name+': .public');
     False: OutputCode.Add(Name+':');
    End;
    Continue; // proceed to the next opcode
   End;

   { comment }
   if (isComment) Then
   Begin
    OutputCode.Add('; '+Name);
    Continue;
   End;

   { opcode }
   Str := Opcodes.OpcodeList[ord(Opcode)].Name+'('; // fetch current opcode's name

   { opcode's parameters }
   For Arg in Args Do
   Begin
    Reg := '';

    // normal registers
    Case Arg.Typ of
     ptBoolReg     : Reg := 'eb';
     ptCharReg     : Reg := 'ec';
     ptIntReg      : Reg := 'ei';
     ptFloatReg    : Reg := 'ef';
     ptStringReg   : Reg := 'es';
     ptReferenceReg: Reg := 'er';
     ptStackVal    : Reg := '[';
    End;

    Reg += VarToStr(Arg.Value);

    // special registers
    Case Arg.Typ of
     ptBoolReg: if (Arg.Value = 5) Then Reg := 'if';
     ptIntReg : if (Arg.Value = 5) Then Reg := 'stp';

     ptStackVal: Reg += ']';
    End;

    // other types
    Case Arg.Typ of
     ptLabelAbsoluteReference: Reg := '@'+Arg.Value;
     ptString                : Reg := '"'+Arg.Value+'"';
    End;

    Str += Reg+',';
   End;

   if (Str[Length(Str)] = ',') Then
    Delete(Str, Length(Str), 1);

   Str += ')';

   OutputCode.Add(Str);
  End;

 OutputCode.Add('}');
 OutputCode.SaveToFile(FileName);
 OutputCode.Free;
End;

(* TCompiler.getBoolOption *)
{
 Gets a specified option's value, casts it into `boolean` - when casting was successful, returns that value, when casting
 failed or option isn't found, returns `oDefault`.
}
Function TCompiler.getBoolOption(const Name: TCommandLineOption; oDefault: Boolean=False): Boolean;
Var Option: TCompileOption;
Begin
 Result := oDefault;

 For Option in Options Do
  if (Option.Option = Name) Then
  Begin
   Case LowerCase(VarToStr(Option.Value)) of
    'true', '1': Exit(True); // `true`, `1` => true
    else // anything else => false
     Exit(False);
   End;
  End;
End;

(* TCompiler.getStringOption *)
{
 See TCompiler.getBoolOption
}
Function TCompiler.getStringOption(const Name: TCommandLineOption; oDefault: String=''): String;
Var Option: TCompileOption;
Begin
 Result := oDefault;

 For Option in Options Do
  if (Option.Option = Name) Then
   Exit(VarToStr(Option.Value));
End;

(* TCompiler.getIntOption *)
{
 See TCompiler.getBoolOption
}
Function TCompiler.getIntOption(const Name: TCommandLineOption; oDefault: Integer): Integer;
Var Option: TCompileOption;
Begin
 Result := oDefault;

 For Option in Options Do
  if (Option.Option = Name) Then
   if (not TryStrToInt(VarToStr(Option.Value), Result)) Then
    Exit(oDefault);
End;

(* TCompiler.SearchFile *)
{
 Searches for file `FileName` (based on `-includepath`).
 If found, sets `Found` to true and returns file name by function's result.
 If file couldn't be found, sets `Found` to false and returns `FileName`'s parameter's value.
}
Function TCompiler.SearchFile(const FileName: String; out Found: Boolean): String;
Var Tmp: String;
    I  : Integer;
Begin
 Result := FileName;
 Found  := False;

 DevLog(dvInfo, 'TCompiler.SearchFile', 'Searching for file: `'+FileName+'`');

 For I := 0 To IncludePaths.Count-1 Do
 Begin
  Tmp := ReplaceDirSep(IncludePaths[I]+'\'+FileName);

  DevLog(dvInfo, 'TCompiler.SearchFile', 'Trying: `'+Tmp+'`');

  if (FileExists(Tmp)) Then
  Begin
   DevLog(dvInfo, 'TCompiler.SearchFile', 'Found!');
   Found := True;
   Exit(Tmp);
  End;
 End;
End;

(* TCompiler.setNewRootNode *)
Procedure TCompiler.setNewRootNode(NewRoot: TCFGNode; const SavePrevious: Boolean);
Begin
 if (SavePrevious) Then
 Begin
  PrevRootNodes.Add(fCurrentRoot);
  PrevRootNodes.Add(fCurrentNode);
 End;

 fCurrentRoot := NewRoot;
 fCurrentNode := NewRoot;
End;

(* TCompiler.restorePrevRootNode *)
Procedure TCompiler.restorePrevRootNode;
Begin
 fCurrentNode := PrevRootNodes.Last;
 PrevRootNodes.Remove(fCurrentNode);

 fCurrentRoot := PrevRootNodes.Last;
 PrevRootNodes.Remove(fCurrentRoot);
End;

(* TCompiler.ParseToken *)
{
 Parses current token (basing on current parser's scope)
}
Procedure TCompiler.ParseToken;

{$I parse_break_continue.pas}
{$I parse_macro.pas}
{$I parse_use.pas}

// main block
Var Token : TToken_P;
    TmpVis: TVisibility;
    Node  : TCFGNode;
Begin
 With Parser do
 Begin
  Token := read;

  if (not inFunction) Then // outside any function
  Begin
   Case Token.Token of
    { public }
    _PUBLIC:
    Begin
     TmpVis     := Visibility;
     Visibility := mvPublic;
     ParseToken;
     Visibility := TmpVis;
    End;

    { private }
    _PRIVATE:
    Begin
     TmpVis     := Visibility;
     Visibility := mvPrivate;
     ParseToken;
     Visibility := TmpVis;
    End;

    { other }
    _BRACKET3_OP: Inc(CurrentDeep);
    _BRACKET3_CL: Dec(CurrentDeep);

    _NAMESPACE: Parse_NAMESPACE.Parse(self);
    _USE      : Parse_USE;
    _CONST    : Parse_CONST.Parse(self);
    _VAR      : CompileError(eUnimplemented, ['global/namespace variables']);
    _FUNCTION : Parse_Function.Parse(self);
    _AT       : ParseMacro_Outside;
    _TYPE     : Parse_TYPE.Parse(self);

    else CompileError(eExpectedDeclOrDef, [Token.Value]);
   End;
  End Else // inside some function
  Begin
   Case Token.Token of
    _BRACKET3_OP: Inc(CurrentDeep);
    _BRACKET3_CL: Dec(CurrentDeep);
    _VAR        : Parse_VAR.Parse(self);
    _CONST      : Parse_CONST.Parse(self);
    _RETURN     : Parse_RETURN.Parse(self);
    _COLON      : if (next.Value = 'CODE') Then Parse_CODE.Parse(self) Else CompileError(eUnexpected, [next.Value]);
    _FOR        : Parse_FOR.Parse(self);
    _IF         : Parse_IF.Parse(self);
    _USE        : Parse_USE;
    _WHILE      : Parse_WHILE.Parse(self);
    _DO         : Parse_WHILE.Parse_DO_WHILE(self);
    _DELETE     : Parse_DELETE.Parse(self);
    _BREAK      : ParseBreak;
    _CONTINUE   : ParseContinue;
    _TYPE       : Parse_TYPE.Parse(self);
    _THROW      : Parse_THROW.Parse(self);
    _TRY        : Parse_TRY_CATCH.Parse(self);

    _ELSE, _NAMESPACE, _CATCH: CompileError(eNotAllowed, [Token.Value]);

    _SEMICOLON: ; // at semicolon, don't do anything

    else
    Begin
     Dec(TokenPos);
     Node := TCFGNode.Create(fCurrentNode, cetExpression, ExpressionCompiler.MakeExpression(self)); // parse as expression
     CFGAddNode(Node);
    End;
   End;
  End;
 End;
End;

(* TCompiler.SkipCodeBlock *)
{
 See @TCompiler.ParseCodeBlock
}
Procedure TCompiler.SkipCodeBlock;
Var Deep: Integer = 0;
Begin
 With Parser do
 Begin
  Repeat
   Case read_t of
    _BRACKET3_OP: Inc(Deep);
    _BRACKET3_CL: Dec(Deep);
   End;
  Until (Deep = 0);
 End;
End;

(* TCompiler.ParseCodeBlock *)
{
 Parses the whole code block.

 When `AllowOneTokenOnly` is `true`, allows constructions like this:
  just_something();
 (so it parses only one token, when it is possible)

 When disabled, there have to be brackets (`{` and `}`), e.g.:
  {
   something();
  }
}
Procedure TCompiler.ParseCodeBlock(const AllowOneTokenOnly: Boolean=False);
Var Deep: Integer;
Begin
 With Parser do
 Begin
  if (next_t <> _BRACKET3_OP) Then
   if (AllowOneTokenOnly) Then
   Begin
    if (next_t in [_VAR, _CONST]) Then
    Begin
     {
      eg.if (true)
       var<int> a; else
       var<int> b;

      is an invalid construction
     }
     CompileError(next, eUnexpected, [next.Value]);
    End;

    ParseToken;
    Exit;
   End Else
    CompileError(next, eExpected, ['{', next.Value]);

  Deep := CurrentDeep;
  Repeat
   ParseToken;
  Until (Deep = CurrentDeep);
 End;
End;

(* TCompiler.PutOpcode *)
{
 Creates an opcode basing on parameter list and adds it into the opcode list.
 When opcode is invalid, displays `eBytecode_InvalidOpcode`
}
Function TCompiler.PutOpcode(fOpcode: TOpcode_E; fArgs: Array of Const; fToken: PToken_P=nil): PMOpcode;
Var I, T: Integer;
    Str : String;
    iTmp: Int64;
    fTmp: Extended;
    Item: PMOpcode;

    DoCheck: Boolean;
Begin
 if (fOpcode in [o_loc_file, o_loc_func, o_loc_line]) and (getBoolOption(opt__strip_debug)) Then
  Exit;

 DoCheck := (fToken <> nil); // check only bytecode written by user

 if (fToken = nil) Then
  fToken := Parser.next_pnt;

 New(Item);
 With Item^ do
 Begin
  isLabel   := False; // opcode cannot be a label
  isComment := False; // nor a comment
  Token     := fToken;

  Opcode := fOpcode;
  SetLength(Args, Length(fArgs)); // save args
  For I := 0 To High(fArgs) Do
   With Args[I] do
   Begin
    Str := #0;
    T   := fArgs[I].VType;
    Typ := ptNone;

    if (T = vtAnsiString) Then
     Str := AnsiString(fArgs[I].VAnsiString) Else

    if (T = vtPChar) Then
     Str := fArgs[I].VPChar Else

    if (T = System.vtString) Then
     Str := fArgs[I].VString^;

    if (Str = '') Then
    Begin
     Typ   := ptChar;
     Value := 0;
     Continue;
    End;

    // register
    if (Str[1] = 'e') Then
    Begin
     Case Str[2] of // register type
      'b': Typ := ptBoolReg;
      'c': Typ := ptCharReg;
      'i': Typ := ptIntReg;
      'f': Typ := ptFloatReg;
      's': Typ := ptStringReg;
      'r': Typ := ptReferenceReg;
      else CompileError(eInternalError, ['Unknown register: '+Str]);
     End;
     Value := Str[3]; // register id
    End Else

    // register: stp
    if (Str = 'stp') Then
    Begin
     Typ   := ptIntReg;
     Value := 5;
    End Else

    // register: if
    if (Str = 'if') Then
    Begin
     Typ   := ptBoolReg;
     Value := 5;
    End Else

    // integer number
    if (T = vtInteger) Then
    Begin
     Typ   := ptInt;
     Value := fArgs[I].VInteger;
    End Else

    // int64 number
    if (T = vtInt64) Then
    Begin
     Typ   := ptInt;
     Value := fArgs[I].VInt64^;
    End ELse

    // float (extended) number
    if (T = vtExtended) Then
    Begin
     Typ   := ptFloat;
     Value := fArgs[I].VExtended^;
    End Else

    // string value
    if (T in [vtString, vtAnsiString, vtPChar]) Then
    Begin
     if (Str[1] = '"') Then
     Begin
      Delete(Str, 1, 1); // remove quote chars
      Delete(Str, Length(Str), 1);
      Typ   := ptString;
      Value := Str;
     End Else
     if (Str[1] = '[') Then
     Begin
      Typ   := ptStackVal;
      Value := StrToInt(Copy(Str, 2, Length(Str)-2));
     End Else
     if (Str[1] = '#') Then
     Begin
      Delete(Str, 1, 1);
      Typ := ptChar;

      if (TryStrToInt64(Str, iTmp)) Then
       Value := iTmp Else
       CompileError(Token^, eBytecode_InvalidOpcode, []);
     End Else
     if (Str[1] = ':') Then
     Begin
      Value := Str;
      Typ   := ptInt;
     End Else
     Begin
      Value := Str;

      if (TryStrToInt64(Value, iTmp)) Then
       Typ := ptInt Else
      if (TryStrToFloat(Value, fTmp)) Then
       Typ := ptFloat;
     End;
    End Else

    // char
    if (T = vtChar) Then
    Begin
     Value := fArgs[I].VChar;
     Typ   := ptChar;
    End Else

     CompileError(eInternalError, ['Unknown parameter type: T='+IntToStr(T)]);
   End;

  Compiler := self;
 End;

 { check opcode }
 if (DoCheck) Then
  if (not isValidOpcode(Item^)) Then
   CompileError(Item^.Token^, eBytecode_InvalidOpcode, []);

 if (DoNotGenerateCode) Then
  Exit(Item);

 { ...and add it into the list }
 Result := OpcodeList[OpcodeList.Add(Item)];
End;

(* TCompiler.PutOpcode *)
{
 Adds opcode `Opcode` without any parameters onto the list.
}
Function TCompiler.PutOpcode(Opcode: TOpcode_E): PMOpcode;
Begin
 Result := PutOpcode(Opcode, []);
End;

(* TCompiler.PutOpcode *)
{
 Creates opcode from the `Opcode` string and calls `TCompiler.PutOpcode` with suitable parameters
}
Function TCompiler.PutOpcode(Opcode: String; Args: Array of Const; fToken: PToken_P=nil): PMOpcode;
Begin
 Result := PutOpcode(TOpcode_E(GetOpcodeID(Opcode)), Args, fToken); // find opcode with name stored in variable (parameter) `Opcode` and then put it into the list
End;

(* TCompiler.PutLabel *)
{
 Puts label, either directly into the code (when `asNOde` = false) or as a node
 (so the label will be added when compiling (not parsing!) a function), when `asConstruction` = true.
 At least one function have to be created when enabling `asNode`

 Sample code:
  PutLabel('first', False);
  PutOpcode(opcode_1);
  PutOpcode(opcode_2);
  PutLabel('second', False);
  PutOpcode(opcode_3);

 When asConstruction = false, results in:
  first:
  second:
  opcode_1
  opcode_2
  opcode_3

 When asConstruction = true:
  first:
  opcode_1
  opcode_2
  second:
  opcode_3
}
Function TCompiler.PutLabel(fName: String; const asNode: Boolean=False): PMOpcode;
Var Item: PMOpcode;
    Node: TCFGNode;
Begin
 if (asNode) Then
 Begin
  Node                     := TCFGNode.Create(fCurrentNode, cetBytecode, nil, Parser.next_pnt(-1));
  Node.Bytecode.OpcodeName := '';
  Node.Bytecode.LabelName  := fName;

  CFGAddNode(Node);

  Exit(nil);
 End Else
 Begin
  New(Item);
  With Item^ do
  Begin
   Name      := fName;
   isLabel   := True;
   isComment := False;

   isPublic := False;
  End;

  if (DoNotGenerateCode) Then
   Exit(Item);

  Exit(OpcodeList[OpcodeList.Add(Item)]);
 End;
End;

(* TCompiler.PutComment *)
{
 Puts a comment into the bytecode
}
Procedure TCompiler.PutComment(fComment: String);
Var Item: PMOpcode;
Begin
 New(Item);
 With Item^ do
 Begin
  Name      := fComment;
  isComment := True;
  isLabel   := False;
 End;
 OpcodeList.Add(Item);
End;

(* TCompiler.findLabel *)
{
 Searches for a label with specified name; returns its position in the bytecode (when found), or `-1` (when not found).
}
Function TCompiler.findLabel(Name: String): Integer;
Var I: LongWord;
Begin
 Result := -1;

 For I := 0 To OpcodeList.Count-1 Do
  if (OpcodeList[I]^.isLabel) and (OpcodeList[I]^.Name = Name) Then
   Exit(I);
End;

(* TCompiler.NewTypeFromFunction *)
{
 Creates new function-pointer-type and adds it into the type list.
}
Function TCompiler.NewTypeFromFunction(Func: TFunction): TType;
Begin
 Result := TType.Create;

 Result.RegPrefix  := 'r';
 Result.InternalID := TYPE_INT_id;
 Result.FuncParams := Func.ParamList;
 Result.FuncReturn := Func.Return;
 Result.Attributes := [taFunction];
End;

(* TCompiler.findLocalType *)
{
 Searches for a local type with specified name and deep.
}
Function TCompiler.findLocalType(fName: String; fTokenPos: Int64=-1): Integer;
Var I: Integer;
Begin
 Result := -1;

 if (not inFunction) Then
 Begin
  DevLog(dvInfo, 'TCompiler.findLocalType', 'called outside function');
  Exit;
 End;

 With getCurrentFunction do // in current function
  For I := 0 To SymbolList.Count-1 Do // each symbol
   With SymbolList[I] do
    if (Typ = lsType) Then // if type
     With mType do
      if (inRange(Range, fTokenPos) and (Name = fName)) Then
       Exit(I);
End;

(* TCompiler.findGlobalType *)
{
 Searches the symbol table for type named `Name` in the global (`self`) and currently parsed namespace.
 If found, returns its ID; if not found, returns -1
}
Function TCompiler.findGlobalType(const TypeName: String; NamespaceID: Integer=-1): TType;

  // Search
  Procedure Search(const Namespace: TNamespace);
  Var Symbol: TGlobalSymbol;
  Begin
   For Symbol in Namespace.SymbolList Do
    if (Symbol.Typ = gsType) and (AnsiCompareStr(Symbol.Name, TypeName) = 0) Then
    Begin
     Result := Symbol.mType;
     Exit;
    End;
  End;

Begin
 Result := nil;

 Search(NamespaceList[0]);

 if (Result = nil) Then // if no symbol found so far
  if (NamespaceID <> -1) Then
   Search(NamespaceList[NamespaceID]);
End;

(* TCompiler.findTypeCandidate *)
Procedure TCompiler.findTypeCandidate(const TypeName: String; Namespaces: TIntegerArray; out TypeID, NamespaceID: Integer; const Token: PToken_P=nil);
Begin
 findGlobalCandidate(TypeName, Namespaces, TypeID, NamespaceID, Token);

 if (TypeID <> -1) Then
  if not (NamespaceList[NamespaceID].SymbolList[TypeID].Typ = gsType) Then
  Begin
   TypeID      := -1;
   NamespaceID := -1;
  End;
End;

(* TCompiler.getTypeFromExpr *)
{
 Gets the type from TExpression; works only for constant (already folded) expressions (ie. only a parent without any childrens).
}
Function TCompiler.getTypeFromExpr(Expr: TExpression): TType;
Begin
 if (Expr.Left <> nil) or (Expr.Right <> nil) Then
  CompileError(eInternalError, ['A folded expression was expected!']);

 Case Expr.Typ of
  mtBool  : Exit(TYPE_BOOL);
  mtChar  : Exit(TYPE_CHAR);
  mtInt   : Exit(TYPE_INT);
  mtFloat : Exit(TYPE_FLOAT);
  mtString: Exit(TYPE_STRING);

  else
   CompileError(eInternalError, ['Invalid expression type']);
 End;
End;

(* TCompiler.NewScope *)
{
 Makes a new scope typed `Typ`; when scope is a loop (supporting `continue` and `break`), there are required also
 `LoopBegin` and `LoopEnd`, which are nodes for - respectively - `continue` and `break` instructions
}
Procedure TCompiler.NewScope(const Typ: TScopeType; LoopBegin: TCFGNode=nil; LoopEnd: TCFGNode=nil);
Begin
 SetLength(Scope, Length(Scope)+1);
 Scope[High(Scope)].Typ       := Typ;
 Scope[High(Scope)].LoopBegin := LoopBegin;
 Scope[High(Scope)].LoopEnd   := LoopEnd;
End;

(* TCompiler.RemoveScope *)
{
 Removes the top (current) scope
}
Procedure TCompiler.RemoveScope;
Begin
 if (Length(Scope) = 0) Then
  CompileError(eInternalError, ['Cannot remove scope - no scope has been set.']);

 SetLength(Scope, High(Scope));
End;

(* TCompiler.inRange *)
Function TCompiler.inRange(Range: TRange; Position: Int64): Boolean;
Begin
 if (Position < 0) Then
  Position := Parser.next(-1).Position;

 Exit(Math.inRange(Position, Range.PBegin, Range.PEnd));
End;

(* TCompiler.__variable_create *)
{
 Creates a variable in current function
}
Procedure TCompiler.__variable_create(fName: String; fTyp: TType; fMemPos: Integer; fAttributes: TVariableAttributes);
Begin
 With getCurrentFunction do
 Begin
  SymbolList.Add(TLocalSymbol.Create(lsVariable));

  With SymbolList.Last do
  Begin
   Name := fName;

   With mVariable do
   Begin
    Typ        := fTyp;
    Range      := Parser.getCurrentRange;
    MemPos     := fMemPos;
    Attributes := fAttributes;
   End;
  End;
 End;
End;

(* TCompiler.FetchVariableValue *)
{
 Tries to fetch the variable's value from previous nodes.

 @TODO:
 if (x == 30)
 {
  y = 2*x; // `x` have to be `30`, so it can be directly inserted
 }
}
Function TCompiler.FetchVariableValue(VariablePnt: TVariable): PExpression;
Var Node   : TCFGNode;
    Visited: TStringList;
    VarName: String;
    Assign : PExpression;
Begin
 Result := nil;

 if (VariablePnt.isVolatile) Then // don't optimize volatile variables
  Exit;

 VarName := TVariable(VariablePnt).RefSymbol.Name;
 Node    := getCurrentNode;

 Visited := TStringList.Create;
 Try
  While (Node <> nil) Do
  Begin
   if (Visited.IndexOf(Node.getName) <> -1) Then // node has been already visited
    Break;

   Visited.Add(Node.getName);

   if (Node.Typ = cetCondition) Then
   Begin
    if (isVariableModified(VariablePnt, Node.Child[0], Node.Child[2])) and (AnythingFromNodePointsAt(Node.Child[0], Node.Child[2], Node)) Then
     Exit(nil);

    if (isVariableModified(VariablePnt, Node.Child[1], Node.Child[2])) and (AnythingFromNodePointsAt(Node.Child[0], Node.Child[2], Node)) Then
     Exit(nil);
   End;

   if (Node.Value <> nil) Then
   Begin
    if (Node.Value^.isVariableModified(VarName, False)) Then // @TODO: if the right side is known, we can parse it at the compile-time
     Exit(nil);

    Assign := Node.Value^.FindAssignment(VarName);

    if (Assign <> nil) and (Assign^.Right^.isConstant) Then
     Exit(Assign^.Right);
   End;

   Node := Node.Parent;
  End;
 Finally
  Visited.Free;
 End;
End;

(* TCompiler.findLocalVariable *)
{
 Finds a local variable or constant with specified name (`fName`) in specified scope (`fDeep`).
 When `fDeep` equals `-1`, it's set to current scope.
}
Function TCompiler.findLocalVariable(fName: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 if (getCurrentFunction = nil) Then
  Exit;

 With getCurrentFunction do
 Begin
  For I := 0 To SymbolList.Count-1 Do
   if (SymbolList[I].Typ in [lsVariable, lsConstant]) Then
    With SymbolList[I], mVariable do
     if (inRange(Range) and (Name = fName)) Then
      Exit(I);
 End;
End;

(* TCompiler.findGlobalVariableCandidate *)
{
 See @TCompiler.findGlobalCandidate
 This function searches global constants and variables.
}
Procedure TCompiler.findGlobalVariableCandidate(const VarName: String; Namespaces: TIntegerArray; out VarID, NamespaceID: Integer; const Token: PToken_P=nil);
Begin
 findGlobalCandidate(VarName, Namespaces, VarID, NamespaceID, Token);

 if (VarID <> -1) Then
  if not (NamespaceList[NamespaceID].SymbolList[VarID].Typ in [gsVariable, gsConstant]) Then
  Begin
   VarID       := -1;
   NamespaceID := -1;
  End;
End;

(* TCompiler.findFunction *)
{
 Searches for a function named `Name` in namespace `NamespaceID` and returns its ID (when found), or `-1` (when not found).
 When `NamespaceID` equals `-1`, it's set to currently parsed namespace.
}
Function TCompiler.findFunction(FuncName: String; NamespaceID: Integer=-1): Integer;
Var I: Integer;
Begin
 Result := -1;

 if (NamespaceID = -1) Then
  NamespaceID := CurrentNamespace;

 With NamespaceList[NamespaceID] do
  For I := 0 To SymbolList.Count-1 Do
   With SymbolList[I] do
    if (Typ = gsFunction) and (Name = FuncName) Then
     Exit(I);
End;

(* TCompiler.findFunctionByLabel *)
{
 Searches for function with label-name `LabelName` and returns that function's ID (when found) and its namespace, or `-1` (when not found).
}
Procedure TCompiler.findFunctionByLabel(const LabelName: String; out FuncID, NamespaceID: Integer);
Var NS, Func: Integer;
Begin
 FuncID      := -1;
 NamespaceID := -1;

 For NS := 0 To NamespaceList.Count-1 Do // each namespace
  With NamespaceList[NS] do
   For Func := 0 To SymbolList.Count-1 Do // each symbol in namespace
    With SymbolList[Func] do
     if (Typ = gsFunction) and (mFunction.MangledName = LabelName) Then
     Begin
      FuncID      := Func;
      NamespaceID := NS;
      Exit;
     End;
End;

(* TCompiler.findFunctionCallCandidate *)
{
 Searches for a function call candidate.
 When a such candidate cannot be found, sets `FuncID` and `NamespaceID` to `-1`.

 Detects only 'ambiguous function call', so if function couldn't have been found (`FuncID = -1`), you must display a
 `function not found` error by yourself.

 Also, when `Token == nil`, it's automatically set to current token.
}
Procedure TCompiler.findFunctionCallCandidate(const FuncName: String; Namespaces: TIntegerArray; out FuncID, NamespaceID: Integer; const Token: PToken_P=nil);
Begin
 findGlobalCandidate(FuncName, Namespaces, FuncID, NamespaceID, Token);

 if (FuncID <> -1) Then
  if not (NamespaceList[NamespaceID].SymbolList[FuncID].Typ = gsFunction) Then
  Begin
   FuncID      := -1;
   NamespaceID := -1;
  End;
End;

(* TCompiler.CFGAddNode *)
{
 Adds a note to current function's flow graph.
}
Procedure TCompiler.CFGAddNode(Node: TCFGNode);
Begin
 if (Node = nil) Then
  CompileError(eInternalError, ['Node = nil']);

 if (fCurrentRoot = nil) Then
 Begin
  fCurrentRoot := Node;
  fCurrentNode := Node;
 End Else

 if (fCurrentNode = nil) Then
  fCurrentNode := Node Else
  Begin
   fCurrentNode.Child.Add(Node);
   fCurrentNode := Node;
  End;
End;

(* TCompiler.getCurrentNamespace *)
{
 Returns current namespace (ie. - namespace which currently being compiled).
}
Function TCompiler.getCurrentNamespace: TNamespace;
Begin
 Result := NamespaceList[CurrentNamespace];
End;

(* TCompiler.getDefaultNamespace *)
{
 Returns the default ("self") namespace.
}
Function TCompiler.getDefaultNamespace: TNamespace;
Begin
 Result := NamespaceList[0];
End;

(* TCompiler.findNamespace *)
{
 Searches for a namespace with specified name.
 Returns its ID when found, or `-1` when not found.
}
Function TCompiler.findNamespace(Name: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := 0 To NamespaceList.Count-1 Do
  if (NamespaceList[I].Name = Name) Then
   Exit(I);
End;

(* TCompiler.inFunction *)
{
 Return `true` when parser is inside any function, or `false` when it's outside.
}
Function TCompiler.inFunction: Boolean;
Begin
 Result := Length(Scope) > 0;
End;

(* TCompiler.findGlobalVariable *)
{
 Searches for a global variable or a constant named `Name` in namespace `NamespaceID` and
 returns its ID (when found), or `-1` when variable doesn't exist.
 When `NamespaceID` equals `-1`, it's set to currently compiled namespace.
}
Function TCompiler.findGlobalVariable(VarName: String; NamespaceID: Integer=-1): Integer;
Var I: Integer;
Begin
 Result := -1;

 if (NamespaceID = -1) Then
  NamespaceID := CurrentNamespace;

 With NamespaceList[NamespaceID] do
  For I := 0 To SymbolList.Count-1 Do
   With SymbolList[I] do
    if (Typ in [gsConstant, gsVariable]) and (Name = VarName) Then
     Exit(I);
End;

(* TCompiler.findGlobalCandidate *)
{
 This function searches for global variables, constants, functions and types.
}
Procedure TCompiler.findGlobalCandidate(const IdentName: String; Namespaces: TIntegerArray; out IdentID, NamespaceID: Integer; const Token: PToken_P=nil);
Type TCandidate = Record
                   ID, Namespace: Integer;
                   Symbol       : TSymbol;
                  End;
Var List  : Array of TCandidate;
    Tmp, I: Integer;
    Tok   : PToken_P;
Begin
 IdentID     := -1;
 NamespaceID := -1;

 { search in namespaces }
 SetLength(List, 0);
 For NamespaceID in Namespaces Do
 Begin
  // search in namespace
  Tmp := -1;

  With NamespaceList[NamespaceID] do
   For I := 0 To SymbolList.Count-1 Do
    if (SymbolList[I].Name = IdentName) Then
     Tmp := I;

  // found?
  if (Tmp <> -1) Then
  Begin
   SetLength(List, Length(List)+1);
   With List[High(List)] do
   Begin
    ID        := Tmp;
    Namespace := NamespaceID;

    With NamespaceList[NamespaceID] do
     Symbol := SymbolList[Tmp];
   End;
  End;
 End;

 { not found }
 if (Length(List) = 0) Then
 Begin
  IdentID     := -1;
  NamespaceID := -1;
 End Else

 { found }
 if (Length(List) = 1) Then
 Begin
  IdentID     := List[0].ID;
  NamespaceID := List[0].Namespace;
 End Else

 { ambiguous reference }
 Begin
  if (Token = nil) Then
   Tok := Parser.next_pnt(-1) Else
   Tok := Token;

  CompileError(Tok^, eAmbiguousIdentifier, [IdentName]);
  CompileNote(Tok^, nCandidates, []);

  For Tmp := Low(List) To High(List) Do
   With List[Tmp].Symbol do
    TCompiler(mCompiler).CompileNote(DeclToken, nCandidate, [NamespaceList[List[Tmp].Namespace].Name+'::'+Name]);

  NamespaceID := -1;
  IdentID     := -1;
 End;
End;

(* TCompiler.RedeclarationCheck *)
{
 Returns `true`, when an identifier is redeclared.
}
Procedure TCompiler.RedeclarationCheck(Name: String; const SkipNamespaces: Boolean=False);
Var ID : Integer;
    Typ: TType;
Begin
 // function
 ID := findFunction(Name);
 if (ID <> -1) Then
 Begin
  CompileError(eRedeclaration, [Name]);
  With getCurrentNamespace.SymbolList[ID] do
   TCompiler(mCompiler).CompileError(DeclToken, ePrevDeclared, []);
  Exit;
 End;

 if (inFunction) Then
 Begin
  // local variable or constant
  ID := findLocalVariable(Name);

  // local type
  if (ID = -1) Then
   ID := findLocalType(Name);

  if (ID <> -1) Then
  Begin
   CompileError(eRedeclaration, [Name]);
   CompileError(getCurrentFunction.SymbolList[ID].DeclToken, ePrevDeclared, []);
   Exit;
  End;
 End Else
 Begin
  // global variable or constant
  ID := findGlobalVariable(Name);
  if (ID <> -1) Then
  Begin
   CompileError(eRedeclaration, [Name]);
   With getCurrentNamespace.SymbolList[ID] do
    if (mCompiler <> nil) Then
     TCompiler(mCompiler).CompileError(DeclToken, ePrevDeclared, []);
   Exit;
  End;

  // global type
  Typ := findGlobalType(Name);
  if (Typ <> nil) Then
  Begin
   CompileError(eRedeclaration, [Name]);
   With Typ.RefSymbol do
    if (mCompiler <> nil) Then // `mCompiler = nil` should be true only for internal types (like `bool`, `string` (...))
     TCompiler(mCompiler).CompileError(DeclToken, ePrevDeclared, []);
   Exit;
  End;

  // namespace
  if (not SkipNamespaces) Then
  Begin
   ID := findNamespace(Name);
   if (ID <> -1) Then
   Begin
    CompileError(eRedeclaration, [Name]);
    With NamespaceList[ID] do
     TCompiler(mCompiler).CompileError(DeclToken, ePrevDeclared, []);
   End;
  End;
 End;
End;

(* TCompiler.CompileCode *)
{
 Compiles code.

 Required parameters:
   fInputFile  -> input *.ss file
   fOutputFile -> output compiled file
   fOptions    -> compiler options

 Parameters set automatically (leave them alone):
   isIncluded  -> when `true`, no output code is saved into any file
   Pass1Only   -> pretty self-explanatory
   fParent     -> parent compiler
   fSupervisor -> see @Parse_include
}
Procedure TCompiler.CompileCode(fInputFile, fOutputFile: String; fOptions: TCompileOptions; isIncluded: Boolean=False; Pass1Only: Boolean=False; fParent: TCompiler=nil; fSupervisor: TCompiler=nil);
Var Compiler2: Compile2.TCompiler;

    VBytecode        : String = '';
    UnfinishedComment: Boolean = False;

    // AddPrimaryType
    Procedure AddPrimaryType(Typ: TType);
    Begin
     Typ.RefSymbol.isInternal := True;
     NamespaceList[0].SymbolList.Add(TGlobalSymbol.Create(gsType, Typ));
    End;

    // CheckMain
    Function CheckMain: Boolean;
    Var FuncID: Integer;
    Begin
     if (CompileMode <> cmApp) Then
      Exit(True);

     FuncID := findFunction('main', 0); // search for `main` function in the `self` namespace

     if (FuncID = -1) Then // not found!
      Exit(False);

     With NamespaceList[0].SymbolList[FuncID].mFunction do
     Begin
      Result :=
      (Length(ParamList) = 0) and
      (type_equal(Return, TYPE_INT));
     End;
    End;

    // ParseCommandLine
    Procedure ParseCommandLine;
    Var I  : Integer;
        Str: String;
    Begin
     { parse `-Cm` }
     Str := getStringOption(opt_Cm, 'app');
     Case Str of
      'app'     : CompileMode := cmApp;
      'lib'     : CompileMode := cmLibrary;
      'bytecode': CompileMode := cmBytecode;

      else
      Begin
       Writeln('Unknown compile mode (-Cm): `', Str, '`; default set to `app`');
       CompileMode := cmApp;
      End;
     End;

     { parse `-includepath` }
     IncludePaths               := TStringList.Create;
     IncludePaths.Delimiter     := ';';
     IncludePaths.DelimitedText := getStringOption(opt_includepath, '$file;$compiler');

     For I := 0 To IncludePaths.Count-1 Do
     Begin
      Str := IncludePaths[I];

      Str := StringReplace(Str, '$file', ExtractFilePath(InputFile), [rfReplaceAll]);
      Str := StringReplace(Str, '$main', ExtractFilePath(Parent.InputFile), [rfReplaceAll]);
      Str := StringReplace(Str, '$compiler', ExtractFilePath(ParamStr(0)), [rfReplaceAll]);

      IncludePaths[I] := Str;
     End;

     { parse `-bytecode` }
     VBytecode := getStringOption(opt_bytecode);
    End;

    // CreateGlobalConstant
    Procedure CreateGlobalConstant(const cName: String; const cType: TType; const cExpr: PExpression);
    Begin
     With NamespaceList.Last do
     Begin
      SymbolList.Add(TGlobalSymbol.Create(gsConstant));
      With SymbolList.Last do
      Begin
       Name       := cName;
       isInternal := True;

       With mVariable do
       Begin
        Typ        := cType;
        Value      := cExpr;
        Visibility := mvPrivate;

        Include(Attributes, vaConst);
       End;
      End;
     End;
    End;

   // CreateSymbols
    Procedure CreateSymbols;
    Begin
     { init default namespace }
     NamespaceList := TNamespaceList.Create;
     NamespaceList.Add(TNamespace.Create);

     With NamespaceList.Last do
     Begin
      Name       := 'self';
      Visibility := mvPublic;
      SymbolList := TGlobalSymbolList.Create;
     End;

     CurrentNamespace := 0;

     SetLength(SelectedNamespaces, 1);
     SelectedNamespaces[0] := 0;

     { create primary type-table }
     AddPrimaryType(TYPE_ANY);
     AddPrimaryType(TYPE_VOID);
     AddPrimaryType(TYPE_BOOL);
     AddPrimaryType(TYPE_CHAR);
     AddPrimaryType(TYPE_INT);
     AddPrimaryType(TYPE_FLOAT);
     AddPrimaryType(TYPE_STRING);

     { create global constants }
     With NamespaceList.Last do
     Begin
      CreateGlobalConstant('null', TYPE_INT, MakeIntExpression(0));

      CreateGlobalConstant('__file', TYPE_STRING, MakeStringExpression(InputFile));
      CreateGlobalConstant('__date', TYPE_STRING, MakeStringExpression(DateToStr(Date)));
      CreateGlobalConstant('__time', TYPE_STRING, MakeStringExpression(TimeToStr(Time)));
      CreateGlobalConstant('__major_version__', TYPE_STRING, MakeStringExpression(FloatToStr(vMajor)));
      CreateGlobalConstant('__minor_version__', TYPE_STRING, MakeStringExpression(FloatToStr(vMinor)));
      CreateGlobalConstant('__version__', TYPE_STRING, MakeStringExpression(Version));

      CreateGlobalConstant('__major__', TYPE_FLOAT, MakeFloatExpression(vMajor));
      CreateGlobalConstant('__minor__', TYPE_FLOAT, MakeFloatExpression(vMinor));
     End;
    End;

   // ResetParser
    Procedure ResetParser; inline;
    Begin
     With Parser do
     Begin
      TokenPos   := 0;
      Visibility := mvPrivate;
     End;
    End;

   // Pass1
    Procedure Pass1;
    Begin
     Log('Compilation pass 1');
     ResetParser;
     CompilePass := _cp1;

     With Parser do
      While (Parser.Can) Do
       ParseToken;
    End;

   // Pass2
    Procedure Pass2;
    Begin
     Log('Compilation pass 2');
     ResetParser;
     CompilePass := _cp2;

     With Parser do
      While (Parser.Can) Do
       ParseToken;
    End;

   // Pass3
    Procedure Pass3;
    Begin
     Log('Compilation pass 3');
     ResetParser;
     CompilePass := _cp3;

     With Parser do
      While (Parser.Can) Do
       ParseToken;
    End;

Var Str: String;
Begin
 fInputFile  := ReplaceDirSep(fInputFile);
 fOutputFile := ReplaceDirSep(fOutputFile);

 InputFile   := fInputFile;
 OutputFile  := fOutputFile;
 Options     := fOptions;
 SomeCounter := 0;
 ModuleName  := '';
 AnyError    := False;
 Parent      := fParent;
 Supervisor  := fSupervisor;

 PrevRootNodes := TCFGNodeList.Create;

 CurrentFunction           := nil;
 DoNotGenerateCode         := False;
 ParsingFORInitInstruction := False;

 if (isIncluded) Then
  Log('Module: '+InputFile) Else
  Log('Main file: '+InputFile+' => '+OutputFile);

 Parser := TParser.Create(self, InputFile, UnfinishedComment);
 ResetParser;

 if (UnfinishedComment) Then
 Begin
  CompileError(Parser.getLastToken, eUnfinishedComment, []);
  Exit;
 End;

 Try
  { no parent specified }
  if (Parent = nil) Then
  Begin
   DevLog(dvInfo, 'TCompiler.CompileCode', 'no `parent` specified!');
   Parent := self;

   if (isIncluded) Then
   Begin
    DevLog(dvFatal, 'TCompiler.CompileCode', 'no `parent` specified and compiling as an unit!');
    CompileError(eInternalError, ['Parent = nil']);
   End;
  End;

  ParseCommandLine;

  { create classes }
  OpcodeList := TOpcodeList.Create;

  { allocate arrays }
  if (Parent = self) Then
  Begin
   New(IncludeList);
   SetLength(IncludeList^, 0);
  End Else
   IncludeList := Parent.IncludeList;

  SetLength(Scope, 0);

  { When `-Cm=bytecode` is specified: }
  if (CompileMode = cmBytecode) Then
  Begin
   CompileAsBytecode;
   Exit; // stop compiler
  End;

  PutOpcode(o_loc_file, ['"'+ExtractRelativePath(ExtractFilePath(Parent.InputFile), InputFile)+'"']); // write file location

  if (not isIncluded) Then // are we the main file?
  Begin
   { compiling as a library }
   if (CompileMode = cmLibrary) Then
   Begin
    Log('-> Compiling as a library');
    ModuleName := '_';
   End Else

   { compiling as a program }
   Begin
    // the beginning of the program must be the "init" and "main" call
    if (getBoolOption(opt_initcode)) Then
     PutOpcode(o_call, [':__init']);

    PutOpcode(o_call, [':__function_self_main_'+ModuleName+'_int_']);
    PutOpcode(o_stop); // and, if we back from main(), the program ends (virtual machine stops).
   End;
  End Else // if included (not the main file)
  Begin
   ModuleName := makeModuleName(fInputFile);
  End;

  { create symbol list }
  CreateSymbols;

  { clear parser variables }
  Parser.CurrentDeep := 0;

  { compile code }
  Pass1;

  if (not AnyError) Then
   Pass2;

  if (not Pass1Only) Then
  Begin
   if (not AnyError) Then
    Pass3;
  End;

  { go next? }
  if (not isIncluded) and (AnyError) Then
  Begin
   Log('-> There were errors compiling this program; stopped.');
   Exit;
  End;

  if (not isIncluded) Then
  Begin
   // make imports
   MakeImports;

   Log('-> Bytecode generated.');

   if (getBoolOption(opt__bytecode_optimize)) Then
   Begin
    Log('-> Optimizing bytecode.');
    Peephole.OptimizeBytecode(self);
   End;
  End;

  { if specified - save bytecode }
  if (VBytecode <> '') and (not isIncluded) Then
   SaveBytecode(VBytecode);

  { compile bytecode }
  if (not isIncluded) Then
  Begin
   { ... but at first - check for valid "main" function declaration }
   if (not CheckMain) Then
   Begin
    CompileError(eNoValidMainFunctionFound);
    Exit;
   End;

   Compiler2 := Compile2.TCompiler.Create;
   Compiler2.Compile(self, CompileMode = cmLibrary);
   Compiler2.Free;

   Str := getStringOption(opt_header);

   { if specified - generate output header file }
   if (Str <> '') Then
   Begin
    Case CompileMode of
     cmLibrary:
     Begin
      Log('-> Generating output header file to: '+Str);
      GenerateHeaderFile(Str);
     End;

     else
      Writeln('Cannot generate the output header file, because input file is not a library.');
    End;
   End;
  End;
 Finally
 End;
End;

(* TCompiler.CompileError *)
{
 Displays an error; when passed error is a `eInternalError` or belongs to `error_stop`, function also stops the compiler.
}
Procedure TCompiler.CompileError(Token: TToken_P; Error: TCompileError; Args: Array of Const);
Var Str: String;
Begin
 if (Parent <> nil) Then
  Parent.AnyError := True;

 Str := InputFile+'('+IntToStr(Token.Line)+','+IntToStr(Token.Char)+') Error: '+Format(CompileError_fmt[Error], Args);
 if (Error = eInternalError) Then
  raise Exception.Create(Str);

 Writeln(Str);

 if (Error in error_stop) Then
  raise Exception.Create(''); // used to stop the compiler
End;

(* TCompiler.CompileError *)
{
 See above
}
Procedure TCompiler.CompileError(Token: PToken_P; Error: TCompileError; Args: Array of Const);
Begin
 if (Token = nil) Then
  CompileError(Error, Args) Else
  CompileError(Token^, Error, Args);
End;

(* TCompiler.CompileError *)
{
 See above
}
Procedure TCompiler.CompileError(Error: TCompileError; Args: Array of Const);
Begin
 CompileError(Parser.next(0), Error, Args);
End;

(* TCompiler.CompileError *)
{
 See above
}
Procedure TCompiler.CompileError(Error: TCompileError);
Begin
 CompileError(Error, []);
End;

(* TCompiler.CompileWarning *)
{
 Displays a warning
}
Procedure TCompiler.CompileWarning(Token: TToken_P; Warning: TCompileWarning; Args: Array of Const);
Begin
 Writeln(InputFile+'('+IntToStr(Token.Line)+','+IntToStr(Token.Char)+') Warning: '+Format(CompileWarning_fmt[Warning], Args));
End;

(* TCompiler.CompileWarning *)
{
 See above
}
Procedure TCompiler.CompileWarning(Token: PToken_P; Warning: TCompileWarning; Args: Array of Const);
Begin
 if (Token = nil) Then
  CompileWarning(Warning, Args) Else
  CompileWarning(Token^, Warning, Args);
End;

(* TCompiler.CompileWarning *)
{
 See above
}
Procedure TCompiler.CompileWarning(Warning: TCompileWarning; Args: Array of Const);
Begin
 CompileWarning(Parser.next(0), Warning, Args);
End;

(* TCompiler.CompileWarning *)
{
 See above
}
Procedure TCompiler.CompileWarning(Warning: TCompileWarning);
Begin
 CompileWarning(Warning, []);
End;

(* TCompiler.CompileHint *)
{
 Displays a hint
}
Procedure TCompiler.CompileHint(Token: TToken_P; Hint: TCompileHint; Args: Array of Const);
Begin
 Writeln(InputFile+'('+IntToStr(Token.Line)+','+IntToStr(Token.Char)+') Hint: '+Format(CompileHint_fmt[Hint], Args));
End;

(* TCompiler.CompileHint *)
{
 See above
}
Procedure TCompiler.CompileHint(Token: PToken_P; Hint: TCompileHint; Args: Array of Const);
Begin
 if (Token = nil) Then
  CompileHint(Hint, Args) Else
  CompileHint(Token^, Hint, Args);
End;

(* TCompiler.CompileHint *)
{
 See above
}
Procedure TCompiler.CompileHint(Hint: TCompileHint; Args: Array of Const);
Begin
 CompileHint(Parser.next(0), Hint, Args);
End;

(* TCompiler.CompileHint *)
{
 See above
}
Procedure TCompiler.CompileHint(Hint: TCompileHint);
Begin
 CompileHint(Hint, []);
End;

(* TCompiler.CompileNote *)
{
 Displays a note
}
Procedure TCompiler.CompileNote(Token: TToken_P; Note: TCompileNote; Args: Array of Const);
Begin
 Writeln(InputFile+'('+IntToStr(Token.Line)+','+IntToStr(Token.Char)+') Note: '+Format(CompileNote_fmt[Note], Args));
End;

(* TCompiler.CompileNote *)
{
 See above
}
Procedure TCompiler.CompileNote(Token: PToken_P; Note: TCompileNote; Args: Array of Const);
Begin
 if (Token = nil) Then
  CompileNote(Note, Args) Else
  CompileNote(Token^, Note, Args);
End;

(* TCompiler.CompileNote *)
{
 See above
}
Procedure TCompiler.CompileNote(Note: TCompileNote; Args: Array of Const);
Begin
 CompileNote(Parser.next(0), Note, Args);
End;

(* TCompiler.CompileNote *)
Procedure TCompiler.CompileNote(Note: TCompileNote);
Begin
 CompileNote(Note, []);
End;

(* TCompiler.GenerateHeaderFile *)
{
 Generates a header file (based on current functions list) and saves it into file `fOutputFile.`
}
Procedure TCompiler.GenerateHeaderFile(const fOutputFile: String);
Var Output: TStringList;
    Str   : String;

    Item, I: Integer;

    Typ  : TGlobalSymbolType;
    mFunc: TFunction;
    mCnst: TVariable;
    mType: TType;

    Namespace: TNamespace;
Begin
 Output := TStringList.Create;

 With Output do
 Begin
  Add('@visibility("public")');
  Add('');

  For Namespace in NamespaceList Do
  Begin
   With Namespace do
   Begin
    if (Visibility <> mvPublic) Then
     Continue;

    if (Name <> 'self') Then // `self` is the global namespace
    Begin
     Add('namespace '+Name);
     Add('{');
    End;

    For Item := 0 To SymbolList.Count-1 Do
    Begin
     if (SymbolList[Item].Visibility <> mvPublic) or (SymbolList[Item].isInternal) Then
      Continue;

     Typ := SymbolList[Item].Typ;
     Case Typ of
      gsType    : mType := SymbolList[Item].mType;
      gsConstant: mCnst := SymbolList[Item].mVariable;
      gsFunction: mFunc := SymbolList[Item].mFunction;
     End;

     { global type }
     if (Typ = gsType) Then
      With mType do
      Begin
       if (taEnum in Attributes) Then // special case: enumeration types
       Begin
        Str := 'type<enum> '+RefSymbol.Name+' = {';

        For mCnst in EnumItemList Do
         Str += mCnst.RefSymbol.Name+'='+IntToStr(mCnst.Value^.Value)+', ';

        System.Delete(Str, Length(Str)-1, 2);

        Str += '};';
       End Else
        Str := 'type<'+mType.asString+'> '+RefSymbol.Name+';';
      End;

     { global constant }
     if (Typ = gsConstant) Then
      With mCnst do
       if not (vaEnumItem in Attributes) Then
        Str := 'const '+RefSymbol.Name+' = '+getValueFromExpression(Value, True)+';';

     { function }
     if (Typ = gsFunction) Then
      With mFunc do
      Begin
       if (ModuleName <> self.ModuleName) Then
        Continue;

       Str := 'function<'+Return.asString+'> '+RefSymbol.Name+'(';

       // parameter list
       For I := Low(ParamList) To High(ParamList) Do
       Begin
        if (ParamList[I].isConst) Then
         Str += 'const ';
        if (ParamList[I].isVar) Then
         Str += 'var ';

        Str += ParamList[I].Typ.asString;

        if (ParamList[I].DefaultValue <> nil) Then
         Str += ' = '+getValueFromExpression(ParamList[I].DefaultValue, True);

        if (I <> High(ParamList)) Then
         Str += ', ';
       End;

       Str += ') [library="'+ExtractFileName(self.OutputFile)+'"];';
      End;

     if (Str <> '') Then
     Begin
      if (Name <> '') Then
       Str := ' '+Str;

      Add(Str);
     End;
    End;

    if (Name <> 'self') Then
     Add('}');
   End;
  End;
 End;

 Output.SaveToFile(fOutputFile);
 Output.Free;
End;
End.
