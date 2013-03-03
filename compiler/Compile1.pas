(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Compile1;

 Interface
 Uses Classes, SysUtils, Variants, FGL,
      Tokens, CompilerUnit, Scanner, Opcodes, Messages, MTypes,
      Parse_FUNCTION, Parse_VAR, Parse_CONST, Parse_RETURN, Parse_CODE, Parse_FOR, Parse_IF, Parse_WHILE, Parse_include, Parse_DELETE,
      Parse_NAMESPACE, Parse_TYPE;

 { constants }
 Const Version  = '2.2 nightly'; // version of the compiler
       iVersion = 2.2;

       bytecode_version_major = 0;
       bytecode_version_minor = 41;

 { types }
 // TMScope
 Type TMScopeType = (sFunction, sFOR, sIF, sWHILE);

 Type TMScope = Record
                 Typ               : TMScopeType;
                 loopBegin, LoopEnd: String;
                End;

 // TOpcodeVector
 Type TOpcodeList = specialize TFPGList<PMOpcode>;

 // TCompiler
 Type TCompiler = Class
                   Private
                    Procedure Preparse;
                    Procedure MakeImports;
                    Procedure CompileAsBytecode;
                    Procedure SaveBytecode(const FileName: String);

                   Public
                    TokenList: Array of TToken_P; // list of tokens (with stripped comments)
                    TokenPos : LongWord; // current token ID (counting from 0)
                    AnyError : Boolean;

                   Public
                    Parent            : TCompiler;
                    CompileMode       : (cmApp, cmLibrary, cmBytecode);
                    InputFile         : String; // input file name
                    OutputFile        : String; // output file name
                    ModuleName        : String; // module name
                    Options           : TCompileOptions; // compile options
                    Interpreter       : Pointer; // pointer to a expression's interpreter class (in ExpressionCompiler.pas)
                    IncludePaths      : TStringList; // list of include paths
                    CurrentNamespace  : Integer; // namespace in which we are (`namespace namespace_name;`)
                    SelectedNamespaces: Array of Integer; // selected namespaces (`use namespace1, namespace2 (...);`)

                    CurrentDeep: Integer; // current brackets' deep (`{` = +1, `}` = -1)
                    Visibility : TMVisibility; // current visibility

                    StringList: Array of TMString; // list of strings used in input file code
                    OpcodeList: TOpcodeList; // output code opcode list

                    ExportList : Array of TMExport; // exports
                    IncludeList: Array of TCompiler; // include

                    NamespaceList: Array of TMNamespace; // namespaces

                    Scope: Array of TMScope; // scope list

                    SomeCounter: LongWord; // used in labels eg.`__while_<somecounter>_begin`, so they don't overwrite each other

               { -> properties }
                    Property getPosition: LongWord read TokenPos; // current token position
                    Property getVisibility: TMVisibility read Visibility; // current visibility state

               { -> methods }
                    Function getBoolOption(const Name: TCommandLineOption; oDefault: Boolean=False): Boolean;
                    Function getStringOption(const Name: TCommandLineOption; oDefault: String=''): String;
                    Function getIntOption(const Name: TCommandLineOption; oDefault: Integer): Integer;

                    Function SearchFile(const FileName: String; out Found: Boolean): String;

                    Procedure setPosition(fTokenPos: LongWord);
                    Procedure AddString(fName, fValue: String);
                    Function AddString(fValue: String): String;
                    Procedure AddConstruction(C: TMConstruction);

                   { parser }
                    Function read: TToken_P;
                    Function read_t: TToken;
                    Function next(const I: Integer=0): TToken_P;
                    Function next_t(const I: Integer=0): TToken;
                    Function read_ident: String;
                    Function read_type(const AllowArrays: Boolean=True): PMType;
                    Procedure eat(Token: TToken);
                    Procedure semicolon;
                    Function getToken(const I: Integer=0): TToken_P;

                    Procedure ParseToken;
                    Procedure ParseCodeBlock(const AllowOneTokenOnly: Boolean=False);

                   { bytecode }
                    Procedure PutOpcode(fOpcode: TOpcode_E; fArgs: Array of Const; fTokenPos: LongWord=0);
                    Procedure PutOpcode(Opcode: TOpcode_E);
                    Procedure PutOpcode(Opcode: String; Args: Array of Const; fTokenPos: LongWord=0);
                    Procedure PutLabel(fName: String; const asConstruction: Boolean=False);
                    Procedure PutComment(fComment: String);

                    Function findLabel(Name: String): Integer;

                   { types }
                    Function NewTypeFromFunction(Func: TMFunction): PMType;

                    Function findGlobalType(TypeName: String): PMType;
                    Function findType(TypeName: String): PMType;

                    Function getTypeDeclaration(const Typ: TMType; DecStringArray: Boolean=True): String;
                    Function getTypeDeclaration(const Typ: PMType; DecStringArray: Boolean=True): String;
                    Function getBytecodeType(const Typ: TMType): String;
                    Function getBytecodeType(const Typ: PMType): String;
                    Function getTypePrefix(ID: PMType): Char;
                    Function getTypeFromExpr(Expr: TMExpression): PMType;

                    Function getArrayBaseType(ID: PMType): PMType;

                    Function CompareTypes(pT2, pT1: PMType): Boolean;

                    Function isTypeVoid(Typ: PMType): Boolean;
                    Function isTypeString(Typ: PMType): Boolean;
                    Function isTypeNumerical(Typ: PMType): Boolean;
                    Function isTypeBool(Typ: PMType): Boolean;
                    Function isTypeInt(Typ: PMType): Boolean;
                    Function isTypeFloat(Typ: PMType): Boolean;
                    Function isTypeChar(Typ: PMType): Boolean;
                    Function isTypeArray(Typ: PMType; const RegardStringAsArray: Boolean=True): Boolean;
                    Function isTypeObject(Typ: PMType): Boolean;
                    Function isTypeFunctionPointer(Typ: PMType): Boolean;

                   { scope }
                    Procedure NewScope(const Typ: TMScopeType; LoopBegin: String=''; LoopEnd: String='');
                    Procedure RemoveScope;

                   { variables }
                    Function findFreeRegister(cRegChar: Char): Integer;
                    Function findLocalVariable(fName: String; fDeep: Integer=-1): Integer;
                    Function getVariableType(ID: Integer): PMType;
                    Function getVariableRegID(ID: Integer): Integer;
                    Function getVariableRegChar(ID: Integer): Char;
                    Function getVariableValue(ID: Integer): PMExpression;
                    Function getGlobalVariableDefinition(Namespace, ID: Integer; const AddNamespace: Boolean=False): String;
                    Function isVariableConstant(ID: Integer): Boolean;

                    Procedure __variable_create(fName: String; fTyp: PMType; fRegID: Integer; fIsParam: Boolean);

                    Function isConstantValue(Expr: TMExpression): Boolean;

                    Procedure findGlobalVariableCandidate(const VarName: String; Namespaces: TMIntegerArray; out VarID, NamespaceID: Integer; const Token: PToken_P=nil);

                   { strings }
                    Function findStringByName(Name: String): Integer;
                    Function findStringByContent(Value: String): Integer;

                   { functions }
                    Function getCurrentFunction: TMFunction;
                    Function getCurrentFunctionPnt: PMFunction;
                    Function getFunctionDefinition(Namespace, ID: Integer; const AddNamespace: Boolean=False): String;

                    Function findFunction(FuncName: String; NamespaceID: Integer=-1): Integer;
                    Procedure findFunctionByLabel(const LabelName: String; out FuncID, NamespaceID: Integer);

                    Procedure findFunctionCandidate(const FuncName: String; Namespaces: TMIntegerArray; out FuncID, NamespaceID: Integer; const Token: PToken_P=nil);

                   { namespaces }
                    Function getCurrentNamespace: TMNamespace;
                    Function getCurrentNamespacePnt: PMNamespace;
                    Function getDefaultNamespace: TMNamespace;
                    Function getDefaultNamespacePnt: PMNamespace;
                    Function getNamespaceName(ID: Integer): String;

                    Function findNamespace(Name: String): Integer;

                   { global-things }
                    Function inFunction: Boolean;

                    Function findGlobalVariable(VarName: String; NamespaceID: Integer=-1): Integer;
                    Procedure findGlobalCandidate(const IdentName: String; Namespaces: TMIntegerArray; out IdentID, NamespaceID: Integer; const Token: PToken_P=nil);

                    Procedure RedeclarationCheck(Name: String; const SkipNamespaces: Boolean=False);

                   { compiling }
                    Procedure CompileCode(fInputFile, fOutputFile: String; fOptions: TCompileOptions; isIncluded: Boolean=False; fParent: TCompiler=nil);

                    Procedure CompileError(Token: TToken_P; Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Error: TCompileError);

                    Procedure CompileHint(Token: TToken_P; Hint: TCompileHint; Args: Array of Const);
                    Procedure CompileHint(Hint: TCompileHint; Args: Array of Const);
                    Procedure CompileHint(Hint: TCompileHint);

                    Procedure CompileNote(Token: TToken_P; Note: TCompileNote; Args: Array of Const);
                    Procedure CompileNote(Note: TCompileNote; Args: Array of Const);
                    Procedure CompileNote(Note: TCompileNote);

                    Procedure GenerateHeaderFile(const fOutputFile: String);
                   End;

 Function makeModuleName(FileName: String): String;
 Function CopyStringToPChar(const Str: String): PChar;

 Implementation
Uses Compile2, ExpressionCompiler, SSM_parser, Peephole;
Type TVarRecArray = Array of TVarRec;
     PVarRecArray = ^TVarRecArray;

(* makeModuleName *)
{
 Creates a module (bytecode label) name, based on FileName in parameter.
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
 Copies a String into PChar (also automatically allocates memory).
}
Function CopyStringToPChar(const Str: String): PChar;
Var I: Integer;
Begin
 Result := AllocMem(Length(Str)+1);

 For I := 1 To Length(Str) Do
  Result[I-1] := Str[I];
End;

(* TCompiler.Preparse *)
{
 Preparses code (removes comments etc.)
}
Procedure TCompiler.Preparse;
Var Scanner: TScanner; // token scanner
    Code   : TStringList; // TScanner needs a TStringList to parse code

    Token           : TToken_P; // current token
    isLongComment   : Boolean=False; // are we in long comment: `/* ... */` (we need to know it, because all tokens in comments are skipped)?
    ShortCommentLine: LongWord=0; // are we in short comment: `//` (it holds this type comment's line)

    I: Integer;
Begin
 // load code from file
 Code := TStringList.Create;
 Code.LoadFromFile(InputFile); // `InputFile` is already set in the `CompileCode`

 // parse it into the token table
 SetLength(TokenList, 0);
 TokenPos := 0;

 Scanner := TScanner.Create(Code);

 if (not Scanner.Can) Then
 Begin
  SetLength(TokenList, 1);
  TokenList[0].Token := _EOF;
 End;

 While (Scanner.Can) do
 Begin
  if (TokenPos > High(TokenList)) Then // we run out of the array, so we need to expand it
  Begin
   SetLength(TokenList, Length(TokenList)+100);
   // This solution is so lame, that I'm even not able to find an adjective to describe it
   // @TODO: use a generic list instead of an array
   For I := High(TokenList)-99 To High(TokenList) Do
    if (I >= 0) Then
     TokenList[I].Token := noToken;
  End;

  Token := Scanner.getNextToken_P;

  if (Token.Token = noToken) Then
   Continue;

  Case Token.Token of
   _DOUBLE_SLASH  { // }: if (not isLongComment) Then ShortCommentLine := Token.Line+1;
   else
    if (Token.Line+1 <> ShortCommentLine) Then
    Begin
     if (Token.Token = _LONGCMT_OPEN { /* }) Then
      isLongComment := True Else
     if (Token.Token = _LONGCMT_CLOSE { /* }) Then
      isLongComment := False Else

     if (not isLongComment) Then
     Begin
      TokenList[TokenPos] := Token;
      Inc(TokenPos);
     End;
    End;
  End;
 End;

 TokenPos := 0;

 // free objects
 Scanner.Free;
 Code.Free;
End;

(* TCompiler.MakeImports *)
{
 Creates import list (opens *.ssm files and adds them into the bytecode)
}
Procedure TCompiler.MakeImports;
Var SSM          : TSSM;
    NS, I, Q     : Integer;
    Can, Found   : Boolean;
    FileName, Tmp: String;
    Comp         : Compile1.TCompiler;
Begin
 if (getBoolOption(opt_initcode)) Then
 Begin
  SSM := TSSM.Create;

  FileName := SearchFile('init.ssm', Found);

  if (not Found) Then
   FileName := SearchFile('stdlib\init.ssm', Found);

  if (not SSM.Load(FileName, FileName, self, False)) Then
   CompileError(eCorruptedSSMFile, ['init.ssm']);

  SSM.Free;
 End;

 For NS := Low(NamespaceList) To High(NamespaceList) Do
 Begin
  With NamespaceList[NS] Do
  Begin
   For I := Low(GlobalList) To High(GlobalList) Do
    if (GlobalList[I].Typ = gdFunction) and (GlobalList[I].mFunction.LibraryFile <> '') Then
    Begin
     Can := True;

     For Q := Low(GlobalList) To I-1 Do // we don't want some file to be loaded eg.10 times instead of 1 time (it just skips multiple imports from same file)
      if (GlobalList[Q].mFunction.LibraryFile = GlobalList[I].mFunction.LibraryFile) Then
       Can := False;

     if (not Can) Then
      Continue; // proceed to the next file

     FileName := GlobalList[I].mFunction.LibraryFile;
     Comp     := TCompiler(GlobalList[I].mFunction.mCompiler);

     FileName := SearchFile(FileName, Found);

     if (not Found) Then
     Begin
      Tmp := ExtractFilePath(Comp.InputFile)+FileName;
      if (FileExists(Tmp)) Then
       FileName := Tmp;
     End;

     SSM := TSSM.Create;
     if (not SSM.Load(FileName, Comp.ModuleName, self)) Then
      Comp.CompileError(GlobalList[I].mFunction.DeclToken, eCorruptedSSMFile, [GlobalList[I].mFunction.LibraryFile]);
     SSM.Free;
    End;
  End;
 End;
End;

(* TCompiler.CompileAsBytecode *)
{
 Compiles input file as a bytecode
}
Procedure TCompiler.CompileAsBytecode;
Var I        : LongWord;
    Item     : PMOpcode;
    Compiler2: Compile2.TCompiler;
Begin
 Log('-> Compiling as a bytecode');

 Preparse;
 CurrentDeep := 0;

 With getCurrentNamespacePnt^ do
 Begin
  SetLength(GlobalList, 1); // at least one function have to be created (otherwise `AddConstruction` would fail)
  With GlobalList[0] do
  Begin
   Typ := gdFunction;

   With mFunction do
   Begin
    Name  := 'main';
    MName := '__function_main';

    SetLength(ParamList, 0);
    SetLength(ConstructionList, 0);
   End;
  End;

  Parse_CODE.Parse(self, True); // parse bytecode

  With GlobalList[0].mFunction do
  Begin
   if (Length(ConstructionList) > 0) Then
    For I := Low(ConstructionList) To High(ConstructionList) Do
     With ConstructionList[I] do
      Case Typ of
    (* ctLabel *)
       ctLabel:
       Begin
        New(Item);
        With Item^ do
        Begin
         Name    := PChar(Values[0]);
         isLabel := True;
        End;
        OpcodeList.Add(Item);
       End;

    (* ctInlineBytecode *)
       ctInlineBytecode: PutOpcode(PChar(Values[0]), PVarRecArray(Values[1])^, LongWord(Values[2]));
      End;
  End;
 End;

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
    StrID     : Integer;
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
    // @TODO: `.public`
    OutputCode.Add(Name+':');
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
     ptString      :
     Begin
      StrID := findStringByName(Arg.Value);
      if (StrID <> -1) Then
       Arg.Value := '"'+StringList[StrID].Value+'"' Else
       Arg.Value := '"<invalid string>"';
     End;

     ptNone:
     Begin
      if (Copy(Arg.Value, 1, 8) = 'string__') Then
      Begin
       StrID := findStringByName(Arg.Value);
       if (StrID <> -1) Then
        Arg.Value := '"'+StringList[StrID].Value+'"' Else
        Arg.Value := '"<invalid string>"';
      End;
     End;
    End;

    Reg += VarToStr(Arg.Value);

    // special registers
    Case Arg.Typ of
     ptBoolReg: if (Arg.Value = 5) Then Reg := 'if';
     ptIntReg : if (Arg.Value = 5) Then Reg := 'stp';

     ptStackVal: Reg += ']';
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

 For I := 0 To IncludePaths.Count-1 Do
 Begin
  Tmp := StringReplace(IncludePaths[I]+'\'+FileName, '\\', '\', [rfReplaceAll]);
  if (FileExists(Tmp)) Then
  Begin
   Found := True;
   Exit(Tmp);
  End;
 End;
End;

(* TCompiler.setPosition *)
{
 Sets current parser's token position.
}
Procedure TCompiler.setPosition(fTokenPos: LongWord);
Begin
 TokenPos := fTokenPos;
End;

(* TCompiler.AddString *)
{
 Adds string named `fName` with value `fValue` into the compiler's string list
}
Procedure TCompiler.AddString(fName, fValue: String);
Var I: Integer;
Begin
 I := findStringByContent(fValue);
 if (I <> -1) Then { string with this content has been added somewhere earlier }
  Exit;

 SetLength(StringList, High(StringList)+2);
 With StringList[High(StringList)] do
 Begin
  Name  := fName+'_'+ModuleName;
  Value := fValue;
 End;
End;

(* TCompiler.AddString *)
{
 Adds string with value `fValue` into the compiler's string list.
 Name is created by the count of strings in the list (string__0, string__1, ...) and returned by result
}
Function TCompiler.AddString(fValue: String): String;
Var I: Integer;
Begin
 I := findStringByContent(fValue);
 if (I <> -1) Then { string with this content has been added somewhere earlier }
  Exit(StringList[I].Name);

 SetLength(StringList, High(StringList)+2);
 With StringList[High(StringList)] do
 Begin
  Name   := 'string__'+IntToStr(High(StringList))+'_'+ModuleName;
  Value  := fValue;
  Result := Name;
 End;
End;

(* TCompiler.AddConstruction *)
{
 Adds construction into the current function's construction list.
}
Procedure TCompiler.AddConstruction(C: TMConstruction);
Begin
 if (getCurrentFunctionPnt = nil) Then
  CompileError(eInternalError, ['No function found!']);

 With getCurrentFunctionPnt^ do // current function
 Begin
  SetLength(ConstructionList, Length(ConstructionList)+1);
  ConstructionList[High(ConstructionList)] := C;
 End;
End;

(* TCompiler.read *)
{
 Reads a token; skips any `noTokens` and shows an error on unfinished strings.
}
Function TCompiler.read: TToken_P;
Begin
 if (TokenPos > High(TokenList)) Then
  CompileError(eEOF);

 Result := TokenList[TokenPos];
 Inc(TokenPos);

 if (Result.Token = noToken) Then // skip `noToken`-s
  Result := read;

 if (Result.Token = _INVALID_STRING) Then
  CompileError(eStringExceedsLine, []);
End;

(* TCompiler.read_t *)
{
 Reads a token's kind; see @TCompiler.read
}
Function TCompiler.read_t: TToken;
Begin
 Result := read.Token;
End;

(* TCompiler.next *)
{
 Returns a next - or previous (when `I` is negative) - token.
}
Function TCompiler.next(const I: Integer=0): TToken_P;
Begin
 Result := TokenList[TokenPos+I];
End;

(* TCompiler.next_t *)
{
 Works just as TCompiler.next, but gets only a token kind.
}
Function TCompiler.next_t(const I: Integer=0): TToken;
Begin
 Result := next(I).Token;
End;

(* TCompiler.read_ident *)
{
 Reads an identifier; displays error `eExpectedIdentifier` when current token isn't an identifier.
}
Function TCompiler.read_ident: String;
Begin
 if (next_t <> _IDENTIFIER) Then
  CompileError(next, eExpectedIdentifier, [next.Display]);
 Result := read.Display;
End;

(* TCompiler.read_type *)
{
 Reads a type name or a full type (based on current token) and returns its ID.
}
Function TCompiler.read_type(const AllowArrays: Boolean=True): PMType;
Var Base : PMType;
    Typ  : TMType;
    Token: TToken_P;

    FuncReturn: PMType;
    FuncParams: TMParamList;

    isArray, isStringBased, isFunction: Boolean;

Label GenerateType;
Begin
 Result := nil;
 Base   := nil;
 Token  := read;

 isArray       := False;
 isStringBased := False;
 isFunction    := False;

 Typ := getEmptyType;

 { read current token }
 Case Token.Token of
  _IDENTIFIER: Base := findType(Token.Display);
  _FUNCTION  : isFunction := True;
  else CompileError(next, eExpectedIdentifier, [Token.Display]);
 End;

 { function type }
 if (isFunction) Then
 Begin
  if (next_t = _LOWER) Then
  Begin
  { specialized function }
   eat(_LOWER);
   FuncReturn := read_type(); // return type
   eat(_GREATER);
   eat(_BRACKET1_OP);
   SetLength(FuncParams, 0);

   While (true) Do // parameter list
   Begin
    Token := next;

    if (Token.Token = _BRACKET1_CL) Then
     Break;

    SetLength(FuncParams, Length(FuncParams)+1);
    FuncParams[High(FuncParams)].Typ := read_type();

    if (isTypeVoid(FuncParams[High(FuncParams)].Typ)) Then
     CompileError(eVoidNoNameParam);

    if (next_t = _BRACKET1_CL) Then
     Break;

    eat(_COMMA);
   End;
   eat(_BRACKET1_CL);

   Typ.RegPrefix  := 'r';
   Typ.InternalID := TYPE_INT_id;
   Typ.isFunction := True;
   Typ.FuncReturn := FuncReturn;
   Typ.FuncParams := FuncParams;
  End Else
  Begin
  { unspecialized function }
   Typ.RegPrefix       := 'r';
   Typ.InternalID      := TYPE_INT_id;
   Typ.FuncReturn      := TypeInstance(TYPE_VOID);
   Typ.isFunction      := True;
   Typ.isUnspecialized := True;
  End;

  if ((next_t = _BRACKET2_OP) and (AllowArrays)) Then // is it an array declaration?
  Begin
   Typ.Name := getTypeDeclaration(Typ);

   New(Base);
   Base^ := Typ;
  End Else
   goto GenerateType;
 End;

 { check for primary type existence }
 if (Base = nil) Then
 Begin
  CompileError(next, eExpectedType, [Token.Display]);
  Exit;
 End;

 Typ           := Base^;
 isStringBased := (Typ = TYPE_STRING);

 if (next_t = _BRACKET2_OP) Then
 Begin
  if (isTypeVoid(Base)) Then // `void` array cannot be created (it would destroy our universe)
  Begin
   CompileError(next, eVoidArray, []);
   Exit;
  End;

  if (Base^.InternalID = TYPE_ANY_id) Then // ... as well, as `any`-typed array
  Begin
   CompileError(next, eInternalError, ['Cannot create an `any`-typed array!']);
   Exit;
  End;
 End;

 { is it an array (is the next token a `[`)? }
 While (next_t = _BRACKET2_OP) and (AllowArrays) Do
 Begin
  eat(_BRACKET2_OP);
  eat(_BRACKET2_CL);

  Inc(Typ.ArrayDimCount);
 End;

 if (isStringBased) Then // is it an array?
  isArray := Typ.ArrayDimCount > 1 Else
  isArray := Typ.ArrayDimCount > 0;

 if (isArray) Then
 Begin
  Typ.RegPrefix := 'r';
  Typ.ArrayBase := Base;

  if (isStringBased) Then
   Typ.ArrayBase := TypeInstance(TYPE_STRING);
 End;

 { set result }
GenerateType:
 Typ.Name := getTypeDeclaration(Typ);

 New(Result);
 Result^ := Typ;
End;

(* TCompiler.eat *)
{
 'eats' a specified token.
 (ie. if current token isn't token passed in the parameter, displays a syntax error).
}
Procedure TCompiler.eat(Token: TToken);
Begin
 if (read_t <> Token) Then
  CompileError(eExpected, [getTokenDisplay(Token), next(-1).Display]);
End;

(* TCompiler.semicolon *)
{
 Eats a semicolon (`_SEMICOLON` token)
}
Procedure TCompiler.semicolon;
Begin
 eat(_SEMICOLON);
End;

(* TCompiler.getToken *)
{
 Works the same as TCompiler.next
}
Function TCompiler.getToken(const I: Integer=0): TToken_P;
Begin
 Result := TokenList[TokenPos+I];
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
    TmpVis: TMVisibility;
Begin
 Token := read;

 if (Length(Scope) = 0) Then // outside the function
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

   else CompileError(eExpectedDeclOrDef, [Token.Display]);
  End;
 End Else // inside function
 Begin
  Case Token.Token of
   _BRACKET3_OP: Inc(CurrentDeep);
   _BRACKET3_CL: Dec(CurrentDeep);
   _VAR        : Parse_VAR.Parse(self);
   _CONST      : Parse_CONST.Parse(self);
   _RETURN     : Parse_RETURN.Parse(self);
   _COLON      : if (next.Display = 'CODE') Then Parse_CODE.Parse(self) Else CompileError(eUnexpected, [next.Display]);
   _FOR        : Parse_FOR.Parse(self);
   _IF         : Parse_IF.Parse(self);
   _ELSE       : CompileError(eNotAllowed, ['else']);
   _NAMESPACE  : CompileError(eNotAllowed, ['namespace']);
   _USE        : Parse_USE;
   _WHILE      : Parse_WHILE.Parse(self);
   _DO         : Parse_WHILE.Parse_DO_WHILE(self);
   _DELETE     : Parse_DELETE.Parse(self);
   _BREAK      : ParseBreak;
   _CONTINUE   : ParseContinue;
   _TYPE       : Parse_TYPE.Parse(self);

   _SEMICOLON: // at pure semicolon, don't do anything

   else
   Begin
    setPosition(getPosition-1); // go back 1 token
    AddConstruction(ExpressionCompiler.MakeConstruction(self)); // parse as expression
   End;
  End;
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
 if (next_t <> _BRACKET3_OP) Then
  if (AllowOneTokenOnly) Then
  Begin
   ParseToken;
   Exit;
  End Else
   CompileError(next, eExpected, ['{', next.Display]);

 Deep := CurrentDeep;
 Repeat
  ParseToken;
 Until (Deep = CurrentDeep);
End;

(* TCompiler.PutOpcode *)
{
 Creates an opcode basing on parameter list and adds it into the opcode list.
 When opcode is invalid, displays `eBytecode_InvalidOpcode`
}
Procedure TCompiler.PutOpcode(fOpcode: TOpcode_E; fArgs: Array of Const; fTokenPos: LongWord=0);
Var I, T: Integer;
    Str : String;
    iTmp: Integer;
    fTmp: Extended;
    Item: PMOpcode;

    DoCheck: Boolean;
Begin
 DoCheck := (fTokenPos <> 0); // check only bytecode written by user

 if (fTokenPos = 0) Then
  fTokenPos := TokenPos;

 if (fTokenPos = 0) Then
  Inc(fTokenPos);

 New(Item);
 With Item^ do
 Begin
  isLabel   := False; // opcode cannot be a label
  isComment := False; // nor a comment
  Token     := @TokenList[fTokenPos-1];

  Opcode := fOpcode;
  SetLength(Args, Length(fArgs)); // save args
  For I := 0 To High(fArgs) Do
   With Args[I] do
   Begin
    Str := #0;
    T   := fArgs[I].VType;
    Typ := ptNone;

    if (T = vtAnsiString) Then
     Str := AnsiString(fArgs[I].VAnsiString);

    if (T = vtPChar) Then
     Str := fArgs[I].VPChar;

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
      Value := AddString(Str);
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

      if (TryStrToInt(Str, iTmp)) Then
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

      if (TryStrToInt(Value, iTmp)) Then
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

 { ...and add it into the list }
 OpcodeList.Add(Item);
End;

(* TCompiler.PutOpcode *)
{
 Adds opcode `Opcode` without any parameters onto the list.
}
Procedure TCompiler.PutOpcode(Opcode: TOpcode_E);
Begin
 PutOpcode(Opcode, []);
End;

(* TCompiler.PutOpcode *)
{
 Creates opcode from the `Opcode` string and calls `TCompiler.PutOpcode` with suitable parameters
}
Procedure TCompiler.PutOpcode(Opcode: String; Args: Array of Const; fTokenPos: LongWord=0);
Begin
 PutOpcode(TOpcode_E(GetOpcodeID(Opcode)), Args, fTokenPos); // find opcode with name stored in variable (parameter) `Opcode` and then put it into the list
End;

(* TCompiler.PutLabel *)
{
 Puts label, either directly into the code (when `asConstruction` = false) or as a construction
 (so the label will be added when compiling (not parsing!) a function), when `asConstruction` = true.
 At least one function have to be created, when enabling `asConstruction`

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
Procedure TCompiler.PutLabel(fName: String; const asConstruction: Boolean=False);
Var Item: PMOpcode;
    C   : TMConstruction;
Begin
 if (asConstruction) Then
 Begin
  C.Typ := ctLabel;
  SetLength(C.Values, 2);
  C.Values[0] := CopyStringToPChar(fName);
  C.Values[1] := @TokenList[TokenPos-1];
  AddConstruction(C);
 End Else
 Begin
  New(Item);
  With Item^ do
  Begin
   Name      := fName;
   isLabel   := True;
   isComment := False;
  End;
  OpcodeList.Add(Item);
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
Function TCompiler.NewTypeFromFunction(Func: TMFunction): PMType;
Var Typ: TMType;
Begin
 Typ := getEmptyType;

 Typ.RegPrefix  := 'r';
 Typ.InternalID := TYPE_INT_id;
 Typ.FuncParams := Func.ParamList;
 Typ.FuncReturn := Func.Return;
 Typ.isFunction := True;
 Typ.DeclToken  := Func.DeclToken;
 Typ.mCompiler  := Func.mCompiler;

 Typ.Name := getTypeDeclaration(Typ);

 New(Result);
 Result^ := Typ;
End;

(* TCompiler.findType *)
{
 See @TCompiler.findGlobalTypeByName
}
Function TCompiler.findType(TypeName: String): PMType;
Begin
 Result := findGlobalType(TypeName); // @TODO: local types
End;

(* TCompiler.findGlobalType *)
{
 Searches the type table for type named `Name` in global and currently parsed namespace.
 If found, returns its ID; if not found, returns -1
}
Function TCompiler.findGlobalType(TypeName: String): PMType;

 Procedure Search(Namespace: TMNamespace);
 Var I: Integer;
 Begin
  With Namespace do
   For I := Low(GlobalList) To High(GlobalList) Do
    if (GlobalList[I].Typ = gdType) and (GlobalList[I].mType.Name = TypeName) Then
    Begin
     Result := @GlobalList[I].mType;
     Exit;
    End;
 End;


Begin
 Result := nil;

 Search(NamespaceList[0]);
 Search(getCurrentNamespace);
End;

(* TCompiler.getTypeDeclaration *)
{
 Returns type declaration.
 Eg.`int[]` or `function<void>(string)`.
}
Function TCompiler.getTypeDeclaration(const Typ: TMType; DecStringArray: Boolean=True): String;
Var I: Integer;
Begin
 Result := '';

 { is function? }
 if (Typ.isFunction) Then
 Begin
  if (Typ.isUnspecialized) Then
   Result := 'unspecialized function' Else
  Begin
   Result := 'function<'+getTypeDeclaration(Typ.FuncReturn)+'>(';

   For I := Low(Typ.FuncParams) To High(Typ.FuncParams) Do
   Begin
    Result += getTypeDeclaration(Typ.FuncParams[I].Typ);
    if (I <> High(Typ.FuncParams)) Then
     Result += ', ';
   End;

   Result += ')';
  End;

  For I := 1 To Typ.ArrayDimCount Do
   Result += '[]';

  Exit;
 End;

 { is primary? }
 if (Typ.ArrayDimCount = 0) or ((Typ.ArrayDimCount = 1) and (Typ.InternalID = TYPE_STRING_id)) Then
 Begin
  Result += PrimaryTypeNames[Typ.InternalID];
 End Else
 Begin
  { is array? }
  if (isTypeString(Typ.ArrayBase)) and (DecStringArray) Then
   I := Typ.ArrayDimCount-1 Else
   I := Typ.ArrayDimCount;

  if (Typ.ArrayBase = nil) Then
   CompileError(eInternalError, ['Typ.ArrayBase = nil']);

  Result += getTypeDeclaration(Typ.ArrayBase);

  For I := 1 To I Do
   Result += '[]';
 End;
End;

(* TCompiler.getTypeDeclaration *)
{
 See @TCompiler.getTypeDeclaration
}
Function TCompiler.getTypeDeclaration(const Typ: PMType; DecStringArray: Boolean=True): String;
Begin
 if (Typ = nil) Then
  Exit('erroneous type');

 Result := getTypeDeclaration(Typ^, DecStringArray);
End;

(* TCompiler.getBytecodeType *)
{
 Returns type declaration, that can be used as a label's name.
 Eg.instead of "int[]" returns "int_arraytype".
}
Function TCompiler.getBytecodeType(const Typ: TMType): String;
Var I: Integer;
Begin
 Result := '';

 { is function? }
 if (Typ.isFunction) Then
 Begin
  if (Typ.isUnspecialized) Then
   Result := 'unspecialized_function' Else
  Begin
   Result := 'function_'+getBytecodeType(Typ.FuncReturn)+'_';

   For I := Low(Typ.FuncParams) To High(Typ.FuncParams) Do
    Result += getBytecodeType(Typ.FuncParams[I].Typ)+'_';
  End;

  For I := 1 To Typ.ArrayDimCount Do
   Result += '_arraytype_';

  Exit;
 End;

 { is primary? }
 if (Typ.ArrayDimCount = 0) or ((Typ.ArrayDimCount = 1) and (Typ.InternalID = TYPE_STRING_id)) Then
 Begin
  Result += PrimaryTypeNames[Typ.InternalID]+'_';
 End Else
 Begin
  { is array? }
  if (Typ.ArrayBase = nil) Then
   CompileError(eInternalError, ['Typ.ArrayBase = nil']);

  Result += getBytecodeType(Typ.ArrayBase);

  For I := 1 To Typ.ArrayDimCount Do
   Result += '_arraytype_';
 End;
End;

(* TCompiler.getBytecodeType *)
{
 See @TCompiler.getBytecodeType
}
Function TCompiler.getBytecodeType(const Typ: PMType): String;
Begin
 if (Typ = nil) Then
  Exit('erroneous type');

 Result := getBytecodeType(Typ^);
End;

(* TCompiler.getTypePrefix *)
{
 Gets type register prefix; when ID is not valid, returns 'i'.
}
Function TCompiler.getTypePrefix(ID: PMType): Char;
Begin
 With getCurrentNamespace do
 Begin
  if (ID = nil) Then
   Exit('i');

  Exit(ID^.RegPrefix);
 End;
End;

(* TCompiler.getTypeFromExpr *)
{
 Gets a type from TMExpression; works only for constant (already folded) expressions (only a parent without children).
}
Function TCompiler.getTypeFromExpr(Expr: TMExpression): PMType;
Begin
 if (Expr.Left <> nil) or (Expr.Right <> nil) Then
  CompileError(eInternalError, ['Expected folded expression']);

 Case Expr.Typ of
  mtBool  : Exit(TypeInstance(TYPE_BOOL));
  mtChar  : Exit(TypeInstance(TYPE_CHAR));
  mtInt   : Exit(TypeInstance(TYPE_INT));
  mtFloat : Exit(TypeInstance(TYPE_FLOAT));
  mtString: Exit(TypeInstance(TYPE_STRING));
  else CompileError(eInternalError, ['Invalid expression type']);
 End;
End;

(* TCompiler.getArrayBaseType *)
{
 Returns array's base type.
}
Function TCompiler.getArrayBaseType(ID: PMType): PMType;
Begin
 Result := ID^.ArrayBase;
End;

(* TCompiler.CompareTypes *)
{
 Compares two types.

 Function should be understood as:
 [ CompareTypes(Type 1, Type 2) ]
 Result = true, when `Type 1` can be assigned into `Type 2`.

 ----------------------------------
 ... also, `T2_id, T1_id` is a correct order in the parameter list :P
}
Function TCompiler.CompareTypes(pT2, pT1: PMType): Boolean;
Var I     : Integer;
    T1, T2: TMType;
Begin
 Result := True;

 if (pT1 = nil) or (pT2 = nil) Then
  Exit(False);

 T1 := pT1^;
 T2 := pT2^;

// if (T1.isUnspecialized) Then
//  CompileError(eInternalError, ['Invalid type comparing: CompareTypes("'+getTypeDeclaration(pT2)+'", "'+getTypeDeclaration(pT1)+'")']);

 { strict types }
 if (T1.isStrict) or (T2.isStrict) Then
  Exit(T1 = T2);

 { 'any' types }
 if (T1 = TYPE_ANY) or (T2 = TYPE_ANY) Then // any or any => true
  Exit(True);

 { comparing null-type }
 if (T1 = TYPE_NULL) Then
 Begin
  Exit(isTypeFunctionPointer(pT2) or
       isTypeObject(pT2));
 End Else

 { compare function-pointers }
 if (isTypeFunctionPointer(pT1) and isTypeFunctionPointer(pT2)) Then
 Begin
  if (T1.isUnspecialized) or (T2.isUnspecialized) Then
   Exit(True);

  Result := (T1.FuncReturn^ = T2.FuncReturn^) and
            (Length(T1.FuncParams) = Length(T2.FuncParams));

  if (not Result) Then
   Exit(False);

  For I := Low(T1.FuncParams) To High(T2.FuncParams) Do
   if not (T1.FuncParams[I].Typ^ = T2.FuncParams[I].Typ^) Then
    Exit(False);

  Exit(True);
 End Else

 { comparing function-pnt with non-func-pnt }
 if (isTypeFunctionPointer(pT1) and (not isTypeFunctionPointer(pT2))) Then
 Begin
  Exit(isTypeInt(pT2));
 End Else

 { compare arrays }
 if (isTypeArray(pT1) and isTypeArray(pT2)) Then
 Begin
  Exit(
  (T2.ArrayBase^.InternalID = T1.ArrayBase^.InternalID) and // arrays' base types must be correct
  (T1.ArrayDimCount = T2.ArrayDimCount) // and also their dimensions amount must be the same
  );
 End Else

 { comparing array with non-array always returns false (except `string` with `char`) }
 if (isTypeArray(pT1) and (not isTypeArray(pT2))) or
    (isTypeArray(pT2) and (not isTypeArray(pT1))) Then
 Begin
  if (isTypeChar(pT2)) and (isTypeString(pT1)) Then // char to string => true
   Exit(True);

  Exit(False);
 End Else

 { compare-table for simple (primary) types }
 Begin
  if (isTypeVoid(pT1)) and (not isTypeVoid(pT2)) Then // void to non void => false
   Exit(False);

  if (isTypeInt(pT1)) and (isTypeFloat(pT2)) Then // int to float => true
   Exit(True);

  if (isTypeInt(pT1)) and (isTypeChar(pT2)) Then // int to char => true
   Exit(True);

  if (isTypeChar(pT1)) and (isTypeInt(pT2)) Then // char to int => true
   Exit(True);

  if (isTypeInt(pT1)) and (isTypeBool(pT2)) Then // int to bool => true (as it's supported by the VM)
   Exit(True);
 End;

 { compare types }
 Exit(T1 = T2);
End;

(* TCompiler.isTypeVoid *)
{
 Returns `true`, when type passed in parameter is `void`-derived; in other case, returns `false`.
}
Function TCompiler.isTypeVoid(Typ: PMType): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);
 Exit(Typ^.InternalID in [TYPE_NULL_id, TYPE_VOID_id]);
End;

(* TCompiler.isTypeString *)
{
 Returns `true`, when type passed in parameter is `string`-derived; in other case, returns `false`.
}
Function TCompiler.isTypeString(Typ: PMType): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);
 Exit(Typ^.InternalID = TYPE_STRING_id);
End;

(* TCompiler.isTypeNumerical *)
{
 Returns `true`, when type passed in parameter is a numerical type (int, float, char and pointers), or numerical-derived; in other case, returns `false`.
}
Function TCompiler.isTypeNumerical(Typ: PMType): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);
 Exit((Typ^.InternalID in [TYPE_INT_id, TYPE_FLOAT_id, TYPE_CHAR_id]) or (Typ^.isFunction));
End;

(* TCompiler.isTypeBool *)
{
 Returns `true`, when type passed in parameter is `bool`-derived; in other case, returns `false`.
}
Function TCompiler.isTypeBool(Typ: PMType): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);
 Exit(Typ^.InternalID in [TYPE_BOOL_id, TYPE_INT_id]);
End;

(* TCompiler.isTypeInt *)
{
 Returns `true`, when type passed in parameter is `int`-derived or is a pointer; in other case, returns `false`.
}
Function TCompiler.isTypeInt(Typ: PMType): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);

 Exit((Typ^.InternalID = TYPE_INT_id) or (Typ^.isFunction));
End;

(* TCompiler.isTypeFloat *)
{
 Returns `true`, when type passed in parameter is `float`-derived; in other case, returns `false`.
 @Note: Also returns `false` when passed type is `int`!
}
Function TCompiler.isTypeFloat(Typ: PMType): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);
 Exit(Typ^.InternalID = TYPE_FLOAT_id);
End;

(* TCompiler.isTypeChar *)
{
 Returns `true`, when type passed in parameter is `char`-derived; in other case, returns `false`.
 @Note: Also returns `false` when passed type is `int`!
}
Function TCompiler.isTypeChar(Typ: PMType): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);
 Exit(Typ^.InternalID = TYPE_CHAR_id);
End;

(* TCompiler.isTypeArray *)
{
 Returns `true`, when type passed in parameter is an array; in other case, returns `false`.
}
Function TCompiler.isTypeArray(Typ: PMType; const RegardStringAsArray: Boolean=True): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);

 if (isTypeString(Typ) and (Typ^.ArrayDimCount = 1)) Then
  Exit(RegardStringAsArray);

 Exit(Typ^.ArrayDimCount > 0);
End;

(* TCompiler.isTypeObject *)
{
 Returns `true`, when type passed in parameter is an object.
}
Function TCompiler.isTypeObject(Typ: PMType): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);

 Result := isTypeArray(Typ); // for now, only arrays are some-kind-of-objects

 if (isTypeString(Typ)) and (Typ^.ArrayDimCount = 1) Then // ... excepts strings - despite the fact, that strings are arrays, they're not objects
  Exit(False);
End;

(* TCompiler.isTypeFunctionPointer *)
{
 Returns `true`, when type passed in parameter is a function pointer.
}
Function TCompiler.isTypeFunctionPointer(Typ: PMType): Boolean;
Begin
 if (Typ = nil) Then
  Exit(False);
 Exit(Typ^.isFunction);
End;

(* TCompiler.NewScope *)
{
 Makes a new scope typed `Typ`; when scope is a loop (supporting `continue` and `break`), there are needed also
 `LoopBegin` and `LoopEnd`, which are labels' names (without preceding `:`) for - respectively - `continue` and `break`
}
Procedure TCompiler.NewScope(const Typ: TMScopeType; LoopBegin: String=''; LoopEnd: String='');
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
  CompileError(eInternalError, ['Length(Scope) = 0']);
 SetLength(Scope, High(Scope));
End;

(* TCompiler.findFreeRegister *)
{
 Searches for a free register (third or fourth) of type `cRegChar`.
 Should be called only for local (or temporary) variable allocation.
 When any free register is found, returns its ID; in other case, returns -1.
}
Function TCompiler.findFreeRegister(cRegChar: Char): Integer;
Var I       : Integer;
    FreeRegs: Set of 1..4 = [];
Begin
 For I := 3 To 4 Do // first 2 registers (ei1/ei2, ef1/ef2 ...) of each type are used for calculations, so we cannot use them as a variable holders
  Include(FreeRegs, I);

 With getCurrentFunction do // search in current function
 Begin
  For I := Low(VariableList) To High(VariableList) Do
   With VariableList[I] do
    if (Deep <= CurrentDeep) and (RegChar = cRegChar) and (RegID > 0) Then // if register is already allocated, we exclude it from the `FreeRegs` list
     Exclude(FreeRegs, RegID);

  For I in FreeRegs Do // return first free register
   Exit(I);
 End;

 Result := -1; // no free register found! :<
End;

(* TCompiler.findLocalVariable *)
{
 Finds a local variable with specified name (`fName`) in specified scope (`fDeep`).
 When `fDeep` equals `-1`, it is set to current scope.
}
Function TCompiler.findLocalVariable(fName: String; fDeep: Integer=-1): Integer;
Var I: Integer;
Begin
 Result := -1;

 if (fDeep = -1) Then
  fDeep := CurrentDeep;

 With getCurrentFunction do
 Begin
  For I := Low(VariableList) To High(VariableList) Do
   if (VariableList[I].Deep <= fDeep) and (VariableList[I].Name = fName) Then
    Exit(I);
 End;
End;

(* TCompiler.getVariableType *)
{
 Gets specified variable's type
}
Function TCompiler.getVariableType(ID: Integer): PMType;
Begin
 Result := getCurrentFunction.VariableList[ID].Typ;
End;

(* TCompiler.getVariableRegID *)
{
 Gets specified variable's register's ID (1..4)
}
Function TCompiler.getVariableRegID(ID: Integer): Integer;
Begin
 Result := getCurrentFunction.VariableList[ID].RegID;
End;

(* TCompiler.getVariableRegChar *)
{
 Gets specified variable's register's char (b, c, i, f, s, r)
}
Function TCompiler.getVariableRegChar(ID: Integer): Char;
Begin
 Result := getCurrentFunction.VariableList[ID].RegChar;
End;

(* TCompiler.getVariableValue *)
{
 Return variable's value (if known); for now, used only for constants.
}
Function TCompiler.getVariableValue(ID: Integer): PMExpression;
Begin
 Result := getCurrentFunction.VariableList[ID].Value;
End;

(* TCompiler.getGlobalVariableDefinition *)
{
 Returns global variable's or constant's definition.
 Eg.:
 "var<int[]> somearray;"
 or
 "const<string> something = "asdf";"

 When `AddNamespace` is `true`, this could be eg.:
 "default namespace :: var<int[]> somearray;"
 or
 "std :: const<string> something = "asdf";"
}
Function TCompiler.getGlobalVariableDefinition(Namespace, ID: Integer; const AddNamespace: Boolean=False): String;
Var mVar: TMVariable;
Begin
 mVar := NamespaceList[Namespace].GlobalList[ID].mVariable;

 Result := '';
 if (AddNamespace) Then
  if (NamespaceList[Namespace].Name = '') Then
   Result := 'default namespace :: ' Else
   Result := NamespaceList[Namespace].Name+' :: ';

 if (mVar.isConst) Then
  Result += 'const<' Else
  Result += 'var<';

 Result += getTypeDeclaration(mVar.Typ)+'> '+mVar.Name;

 if (mVar.isConst) Then
  Result += ' = '+getValueFromExpression(self, mVar.Value, True);

 Result += ';';
End;

(* TCompiler.isVariableConstant *)
{
 Returns `true`, when a variable with specified ID is a constant.
}
Function TCompiler.isVariableConstant(ID: Integer): Boolean;
Begin
 Result := getCurrentFunction.VariableList[ID].isConst;
End;

(* TCompiler.__variable_create *)
{
 Creates a variable in current function
}
Procedure TCompiler.__variable_create(fName: String; fTyp: PMType; fRegID: Integer; fIsParam: Boolean);
Begin
 With getCurrentFunctionPnt^ do
 Begin
  SetLength(VariableList, Length(VariableList)+1);
  With VariableList[High(VariableList)] do
  Begin
   Name    := fName;
   Typ     := fTyp;
   RegID   := fRegID;
   RegChar := getTypePrefix(Typ);
   isParam := fIsParam;
   isConst := False;
  End;
 End;
End;

(* TCompiler.isConstantValue *)
{
 Returns `true`, when an expression is a constant value.
 Passed expression must be already folded.
}
Function TCompiler.isConstantValue(Expr: TMExpression): Boolean;
Begin
 Result := (Expr.Left = nil) and (Expr.Right = nil) and (Expr.Typ in [mtBool, mtChar, mtInt, mtFloat, mtString]);
End;

(* TCompiler.findGlobalVariableCandidate *)
{
 See @TCompiler.findFunctionCandidate
 This function searches also a global constants.
}
Procedure TCompiler.findGlobalVariableCandidate(const VarName: String; Namespaces: TMIntegerArray; out VarID, NamespaceID: Integer; const Token: PToken_P=nil);
Type TCandidate = Record
                   VarID, Namespace: Integer;
                  End;
Var List: Array of TCandidate;
    Tmp : Integer;
    Tok : PToken_P;
Begin
 VarID       := -1;
 NamespaceID := -1;

 { search }
 SetLength(List, 0);
 For NamespaceID in Namespaces Do
 Begin
  Tmp := findGlobalVariable(VarName, NamespaceID);

  if (Tmp <> -1) Then
  Begin
   SetLength(List, Length(List)+1);
   List[High(List)].VarID     := Tmp;
   List[High(List)].Namespace := NamespaceID;
  End;
 End;

 { variable or constant not found }
 if (Length(List) = 0) Then
 Begin
  VarID       := -1;
  NamespaceID := -1;
 End Else

 { variable found }
 if (Length(List) = 1) Then
 Begin
  VarID       := List[0].VarID;
  NamespaceID := List[0].Namespace;
 End Else

 { ambiguous reference }
 Begin
  if (Token = nil) Then
   Tok := @TokenList[TokenPos-1] Else
   Tok := Token;

  CompileError(Tok^, eAmbiguousVariable, [VarName]);
  CompileNote(Tok^, nCandidates, []);

  For Tmp := Low(List) To High(List) do
   CompileNote(Tok^, nCandidate, [getGlobalVariableDefinition(List[Tmp].Namespace, List[Tmp].VarID, True)]);
 End;
End;

(* TCompiler.findStringByName *)
{
 Searches for string named `Name` in the string table; returns its ID when found, or `-1` when not found.
}
Function TCompiler.findStringByName(Name: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := Low(StringList) To High(StringList) Do
  if (StringList[I].Name = Name) Then
   Exit(I);
End;

(* TCompiler.findStringByContent *)
{
 Searches for string with content `Value` in the string table; returns its ID when found, or `-1` when not found.
 Used in optimizations.
}
Function TCompiler.findStringByContent(Value: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := Low(StringList) To High(StringList) Do
  if (StringList[I].Value = Value) Then
   Exit(I);
End;

(* TCompiler.getCurrentFunction *)
{
 Returns currently parsed function
}
Function TCompiler.getCurrentFunction: TMFunction;
Var I: Integer;
Begin
 Result.Name := '';

 With getCurrentNamespace do
  For I := High(GlobalList) Downto Low(GlobalList) Do
   if (GlobalList[I].Typ = gdFunction) Then
    Exit(GlobalList[I].mFunction);
End;

(* TCompiler.getCurrentFunctionPnt *)
{
 Returns a pointer to currently parsed function (or `nil` if not found)
}
Function TCompiler.getCurrentFunctionPnt: PMFunction;
Var I: Integer;
Begin
 Result := nil;

 With getCurrentNamespace do
  For I := High(GlobalList) Downto Low(GlobalList) Do
   if (GlobalList[I].Typ = gdFunction) Then
    Exit(@GlobalList[I].mFunction);
End;

(* TCompiler.getFunctionDefinition *)
{
 Returns function's definition.
 Eg.:
 "function<void> main()"
 or
 "function<void> func(string[], int)"

 When `AddNamespace` is `true`, these could be eg.:
 "default namespace :: function<void> main()"
 or
 "std :: function<void> func(string[], int)"
}
Function TCompiler.getFunctionDefinition(Namespace, ID: Integer; const AddNamespace: Boolean=False): String;
Var Func: TMFunction;
    I   : Integer;
Begin
 Func := NamespaceList[Namespace].GlobalList[ID].mFunction;

 Result := '';

 if (AddNamespace) Then
  if (NamespaceList[Namespace].Name = '') Then
   Result += 'default namespace :: ' Else
   Result += NamespaceList[Namespace].Name+ ' :: ';

 Result += 'function<'+getTypeDeclaration(Func.Return)+'> '+Func.Name+'(';

 For I := Low(Func.ParamList) To High(Func.ParamList) Do
 Begin
  Result += getTypeDeclaration(Func.ParamList[I].Typ);

  if (I <> High(Func.ParamList)) Then
   Result += ', ';
 End;

 Result += ')';
End;

(* TCompiler.findFunction *)
{
 Searches for a function named `Name` in namespace `NamespaceID` and returns its ID (when found), or `-1` (when not found).
 When `NamespaceID` equals `-1`, it's set ti currently parsed namespace.
}
Function TCompiler.findFunction(FuncName: String; NamespaceID: Integer=-1): Integer;
Var I: Integer;
Begin
 Result := -1;

 if (NamespaceID = -1) Then
  NamespaceID := CurrentNamespace;

 With NamespaceList[NamespaceID] do
  For I := Low(GlobalList) To High(GlobalList) Do
   With GlobalList[I] do
    if (Typ = gdFunction) and (mFunction.Name = FuncName) Then
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

 For NS := Low(NamespaceList) To High(NamespaceList) Do
  With NamespaceList[NS] do
   For Func := Low(GlobalList) To High(GlobalList) Do
    With GlobalList[Func] do
     if (Typ = gdFunction) and (mFunction.MName = LabelName) Then
     Begin
      FuncID      := Func;
      NamespaceID := NS;
      Exit;
     End;
End;

(* TCompiler.findFunctionCandidate *)
{
 Searches for a function call candidate, basing on current namespaces and passed function's name.
 When a such candidate is not found, sets `FuncID` and `NamespaceID` to `-1`.

 Detects only 'ambiguous function call', so if function couldn't be found (`FuncID = -1`), you must display a
 `function not found` error by yourself.

 Also, when `Token == nil`, it's automatically set to current token.
}
Procedure TCompiler.findFunctionCandidate(const FuncName: String; Namespaces: TMIntegerArray; out FuncID, NamespaceID: Integer; const Token: PToken_P=nil);
Type TCandidate = Record
                   Func, Namespace: Integer;
                  End;
Var List: Array of TCandidate;
    Tmp : Integer;
    Tok : PToken_P;
Begin
 FuncID      := -1;
 NamespaceID := -1;

 SetLength(List, 0);
 For NamespaceID in Namespaces Do
 Begin
  Tmp := findFunction(FuncName, NamespaceID);

  if (Tmp <> -1) Then
  Begin
   SetLength(List, Length(List)+1);
   List[High(List)].Func      := Tmp;
   List[High(List)].Namespace := NamespaceID;
  End;
 End;

 { function not found }
 if (Length(List) = 0) Then
 Begin
  FuncID      := -1;
  NamespaceID := -1;
 End Else

 { function found }
 if (Length(List) = 1) Then
 Begin
  FuncID      := List[0].Func;
  NamespaceID := List[0].Namespace;
 End Else

 { ambiguous call }
 Begin
  FuncID      := -1;
  NamespaceID := -1;

  if (Token = nil) Then
   Tok := @TokenList[TokenPos-1] Else
   Tok := Token;

  CompileError(Tok^, eAmbiguousCall, [FuncName]);
  CompileNote(Tok^, nCandidates, []);

  For Tmp := Low(List) To High(List) Do
   CompileNote(Tok^, nCandidate, [getFunctionDefinition(List[Tmp].Namespace, List[Tmp].Func, True)]);
 End;
End;

(* TCompiler.getCurrentNamespace *)
{
 Returns current namespace (ie. - namespace which is currently parsed).
}
Function TCompiler.getCurrentNamespace: TMNamespace;
Begin
 Result := NamespaceList[CurrentNamespace];
End;

(* TCompiler.getCurrentNamespacePnt *)
{
 Returns pointer to current namespace.
}
Function TCompiler.getCurrentNamespacePnt: PMNamespace;
Begin
 Result := @NamespaceList[CurrentNamespace];
End;

(* TCompiler.getDefaultNamespace *)
{
 Returns default ("global") namespace.
}
Function TCompiler.getDefaultNamespace: TMNamespace;
Begin
 Result := NamespaceList[0];
End;

(* TCompiler.getDefaultNamespacePnt *)
{
 Returns pointer to default ("global") namespace.
}
Function TCompiler.getDefaultNamespacePnt: PMNamespace;
Begin
 Result := @NamespaceList[0];
End;

(* TCompiler.getNamespaceName *)
{
 Returns namespace's name
}
Function TCompiler.getNamespaceName(ID: Integer): String;
Begin
 Result := NamespaceList[ID].Name;
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

 For I := Low(NamespaceList) To High(NamespaceList) Do
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
  For I := Low(GlobalList) To High(GlobalList) Do
   With GlobalList[I] do
    if (Typ in [gdConstant, gdVariable]) and (mVariable.Name = VarName) Then
     Exit(I);
End;

(* TCompiler.findGlobalCandidate *)
{
 See @TCompiler.findFunctionCandidate
 This function searches for global variables, constants and functions.
}
Procedure TCompiler.findGlobalCandidate(const IdentName: String; Namespaces: TMIntegerArray; out IdentID, NamespaceID: Integer; const Token: PToken_P=nil);
Type TCandidate = Record
                   ID, Namespace: Integer;
                   Typ          : (tVar, tFunc);
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
   For I := Low(GlobalList) To High(GlobalList) Do
    Case GlobalList[I].Typ of
     gdVariable, gdConstant:
      if (GlobalList[I].mVariable.Name = IdentName) Then
       Tmp := I;

     gdFunction:
      if (GlobalList[I].mFunction.Name = IdentName) Then
       Tmp := I;
    End;

  // found?
  if (Tmp <> -1) Then
  Begin
   SetLength(List, Length(List)+1);
   With List[High(List)] do
   Begin
    ID        := Tmp;
    Namespace := NamespaceID;

    Case NamespaceList[NamespaceID].GlobalList[Tmp].Typ of
     gdVariable, gdConstant: Typ := tVar;
     gdFunction            : Typ := tFunc;
    End;
   End;
  End;
 End;

 { variable, constant or function not found }
 if (Length(List) = 0) Then
 Begin
  IdentID     := -1;
  NamespaceID := -1;
 End Else

 { var, cnst or func found }
 if (Length(List) = 1) Then
 Begin
  IdentID     := List[0].ID;
  NamespaceID := List[0].Namespace;
 End Else

 { ambiguous reference }
 Begin
  if (Token = nil) Then
   Tok := @TokenList[TokenPos-1] Else
   Tok := Token;

  CompileError(Tok^, eAmbiguousIdentifier, [IdentName]);
  CompileNote(Tok^, nCandidates, []);

  For Tmp := Low(List) To High(List) do
  Begin
   NamespaceID := List[Tmp].Namespace;
   IdentID     := List[Tmp].ID;

   With NamespaceList[NamespaceID].GlobalList[IdentID] do
    Case List[Tmp].Typ of
     tVar : TCompiler(mVariable.mCompiler).CompileNote(mVariable.DeclToken, nCandidate, [getGlobalVariableDefinition(NamespaceID, IdentID, True)]);
     tFunc: TCompiler(mFunction.mCompiler).CompileNote(mFunction.DeclToken, nCandidate, [getFunctionDefinition(NamespaceID, IdentID, True)]);
    End;
  End;

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
    Typ: PMType;
Begin
 // function
 ID := findFunction(Name);
 if (ID <> -1) Then
 Begin
  CompileError(eRedeclaration, [Name]);
  With getCurrentNamespace.GlobalList[ID].mFunction do
   TCompiler(mCompiler).CompileError(DeclToken, ePrevDeclared, []);
  Exit;
 End;

 if (Name = 'any') or (Name = 'null') or (Name = 'void') or (Name = 'bool') or
    (Name = 'int') or (Name = 'float') or (Name = 'string') Then
     Begin
      CompileError(eRedeclaration, [Name]);
      Exit;
     End;

 if (inFunction) Then
 Begin
  // local variable or constant
  ID := findLocalVariable(Name);
  if (ID <> -1) Then
  Begin
   CompileError(eRedeclaration, [Name]);
   CompileError(getCurrentFunction.VariableList[ID].DeclToken, ePrevDeclared, []);
   Exit;
  End;

  // local type
  // @TODO
 End Else
 Begin
  // global variable or constant
  ID := findGlobalVariable(Name);
  if (ID <> -1) Then
  Begin
   CompileError(eRedeclaration, [Name]);
   With getCurrentNamespace.GlobalList[ID].mVariable do
    TCompiler(mCompiler).CompileError(DeclToken, ePrevDeclared, []);
   Exit;
  End;

  // global type
  Typ := findGlobalType(Name);
  if (Typ <> nil) Then
  Begin
   CompileError(eRedeclaration, [Name]);
   With Typ^ do
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

Needed parameters:
 fInputFile  -> input *.ss file
 fOutputFile -> output compiled file
 fOptions    -> compiler options

Parameters set automatically (leave them alone):
 isIncluded -> when `true`, no output code is saved into any file; instead, generated bytecode is sent into the parent compiler.
 fParent    -> parent compiler
}
Procedure TCompiler.CompileCode(fInputFile, fOutputFile: String; fOptions: TCompileOptions; isIncluded: Boolean=False; fParent: TCompiler=nil);
Var Compiler2: Compile2.TCompiler;

    MFunc: TMFunction;

    VBytecode, Str: String;

    I, NS: Integer;

    // AddPrimaryType
    Procedure AddPrimaryType(Typ: PMType);
    Begin
     With NamespaceList[0] do
     Begin
      SetLength(GlobalList, Length(GlobalList)+1);
      GlobalList[High(GlobalList)].Typ        := gdType;
      GlobalList[High(GlobalList)].mType      := Typ^;
      GlobalList[High(GlobalList)].isInternal := True;
     End;
    End;

    // CheckMain
    Function CheckMain: Boolean;
    Var FuncID: Integer;
    Begin
     if (CompileMode <> cmApp) Then
      Exit(True);

     FuncID := findFunction('main', 0); // search for `main` function in global namespace

     if (FuncID = -1) Then // not found!
      Exit(False);

     With NamespaceList[0].GlobalList[FuncID].mFunction do
     Begin
      Result :=
      (Length(ParamList) = 0) and
      (isTypeInt(Return)) and
      (not isTypeObject(Return));
     End;
    End;

Begin
 InputFile   := fInputFile;
 OutputFile  := fOutputFile;
 Options     := fOptions;
 SomeCounter := 0;
 Visibility  := mvPrivate;
 ModuleName  := '';
 AnyError    := False;
 Parent      := fParent;

 if (isIncluded) Then
  Log('Module: '+InputFile) Else
  Log('Main file: '+InputFile+' :: '+OutputFile);

 { no parent specified }
 if (Parent = nil) Then
 Begin
  Parent := self;

  if (isIncluded) Then
   CompileError(eInternalError, ['Parent = nil']);
 End;

 { quick compiler's type-check }
 {$IF (sizeof(Byte) <> 1) or (sizeof(Char) <> 1) or (sizeof(Integer) <> 4) or (sizeof(LongWord) <> 4) or (sizeof(Extended) <> 10)}
 {$WARNING Invalid types sizes!}
 {$WARNING You can try to compile this compiler anyway (just remove this `$FATA` below), but I'm not responsible for any damage...}
 {$FATAL :<}
 {$ENDIF}

 { parse `-Cm` }
 Str := getStringOption(opt_Cm, 'app');
 Case Str of
  'app'     : CompileMode := cmApp;
  'lib'     : CompileMode := cmLibrary;
  'bytecode': CompileMode := cmBytecode;

  else
  Begin
   Log('Unknown compile mode (-Cm): '+Str+'; default set to `app`');
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

 { create classes }
 OpcodeList := TOpcodeList.Create;

 { allocate arrays }
 SetLength(ExportList, 0);
 SetLength(IncludeList, 0);

 SetLength(StringList, 0);
 SetLength(Scope, 0);

 SetLength(NamespaceList, 1);

 // init default namespace
 NamespaceList[0].Name       := '';
 NamespaceList[0].Visibility := mvPublic;
 SetLength(NamespaceList[0].GlobalList, 0);
 CurrentNamespace := 0;

 SetLength(SelectedNamespaces, 1);
 SelectedNamespaces[0] := 0;

 Interpreter := ExpressionCompiler.TInterpreter.Create(self);

 { When a `-Cm bytecode` is specified: }
 if (CompileMode = cmBytecode) Then
 Begin
  CompileAsBytecode;
  Exit; // stop compiler
 End;

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
   // the beginning of the program must be an "init" and "main" function call
   if (getBoolOption(opt_initcode)) Then
    PutOpcode(o_call, [':__init']);

   PutOpcode(o_call, [':__function_self_main_'+ModuleName+'_int_']);
   PutOpcode(o_stop); // and, if we back from main(), the program ends (virtual machine stops).
  End;
 End Else // if included (not the main file)
 Begin
  ModuleName := makeModuleName(fInputFile);
 End;

 { preparse code (parse tokens, remove comments etc.) }
 Preparse;

 { create primary type-table }
 AddPrimaryType(TypeInstance(TYPE_ANY));
 AddPrimaryType(TypeInstance(TYPE_NULL));
 AddPrimaryType(TypeInstance(TYPE_VOID));
 AddPrimaryType(TypeInstance(TYPE_BOOL));
 AddPrimaryType(TypeInstance(TYPE_CHAR));
 AddPrimaryType(TypeInstance(TYPE_INT));
 AddPrimaryType(TypeInstance(TYPE_FLOAT));
 AddPrimaryType(TypeInstance(TYPE_STRING));

 { create global constants }
 With NamespaceList[0] do
 Begin
  Name := 'self';

  SetLength(GlobalList, Length(GlobalList)+1);

  With GlobalList[High(GlobalList)] do
  Begin
   Typ := gdConstant;

   With mVariable do
   Begin
    Name       := 'null';
    Typ        := TypeInstance(TYPE_NULL);
    Value      := MakeIntExpression(0);
    RegChar    := 'r';
    isConst    := True;
    Visibility := mvPrivate;
   End;

   isInternal := True;
  End;

  // @TODO: special constants
 End;

 { clear variables }
 CurrentDeep := 0;

 { compile code }
 Repeat
  ParseToken;
 Until (TokenList[getPosition].Token = noToken);

 { create an export list }
 For NS := Low(NamespaceList) To High(NamespaceList) Do
 Begin
  if (NamespaceList[NS].Visibility <> mvPublic) Then
   Continue;

  With NamespaceList[NS] do
   For I := Low(GlobalList) To High(GlobalList) Do
   Begin
    if (GlobalList[I].Typ <> gdFunction) Then
     Continue;

    MFunc := GlobalList[I].mFunction;

    if (MFunc.ModuleName = ModuleName) and (MFunc.Visibility = mvPublic) Then // export only public functions from the compiled module (eg.if we included `somemodule.ss`, we don't want to export functions located in it)
    Begin
     SetLength(ExportList, Length(ExportList)+1);
     ExportList[High(ExportList)].Name := MFunc.MName;
     // @Note: ExportList[...].Pos will be set in Compile2
    End;
   End;
 End;

 { compile further? }
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
     Log('-> Cannot generate output header file, because input file is not a library.');
   End;
  End;
 End;
End;

(* TCompiler.CompileError *)
{
 Displays an error; when passed error is a `eInternalError` or belongs to `error_stop`, function also stops the compiler.
}
Procedure TCompiler.CompileError(Token: TToken_P; Error: TCompileError; Args: Array of Const);
Var Str: String;
Begin
 Parent.AnyError := True;

 Str := InputFile+'('+IntToStr(Token.Line+1)+','+IntToStr(Token.Char)+') Error: '+Format(CompileError_fmt[Error], Args);
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
Procedure TCompiler.CompileError(Error: TCompileError; Args: Array of Const);
Begin
 CompileError(getToken(-1), Error, Args);
End;

(* TCompiler.CompileError *)
{
 See above
}
Procedure TCompiler.CompileError(Error: TCompileError);
Begin
 CompileError(Error, []);
End;

(* TCompiler.CompileHint *)
{
 Displays a hint
}
Procedure TCompiler.CompileHint(Token: TToken_P; Hint: TCompileHint; Args: Array of Const);
Begin
 Writeln(InputFile+'('+IntToStr(Token.Line+1)+','+IntToStr(Token.Char)+') Hint: '+Format(CompileHint_fmt[Hint], Args));
End;

(* TCompiler.CompileHint *)
{
 See above
}
Procedure TCompiler.CompileHint(Hint: TCompileHint; Args: Array of Const);
Begin
 CompileHint(getToken(-1), Hint, Args);
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
 Writeln(InputFile+'('+IntToStr(Token.Line+1)+','+IntToStr(Token.Char)+') Note: '+Format(CompileNote_fmt[Note], Args));
End;

(* TCompiler.CompileNote *)
{
 See above
}
Procedure TCompiler.CompileNote(Note: TCompileNote; Args: Array of Const);
Begin
 CompileNote(getToken(-1), Note, Args);
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
Var Output     : TStringList;
    mFunc      : TMFunction;
    mCnst      : TMVariable;
    mType      : TMType;
    Typ        : TMGlobalDeclarationType;
    NS, Item, I: Integer;
    Str        : String;
Begin
 Output := TStringList.Create;

 With Output do
 Begin
  Add('@visibility("public")');
  Add('');

  For NS := Low(NamespaceList) To High(NamespaceList) Do
  Begin
   With NamespaceList[NS] do
   Begin
    if (Visibility <> mvPublic) Then
     Continue;

    if (Name <> 'self') Then // `self` is the global namespace
    Begin
     Add('namespace '+Name);
     Add('{');
    End;

    For Item := Low(GlobalList) To High(GlobalList) Do
    Begin
     Typ := GlobalList[Item].Typ;
     Case Typ of
      gdType    : mType := GlobalList[Item].mType;
      gdConstant: mCnst := GlobalList[Item].mVariable;
      gdFunction: mFunc := GlobalList[Item].mFunction;
     End;

     { global type }
     if (Typ = gdType) Then
      With mType do
      Begin
       if (Visibility <> mvPublic) or (GlobalList[Item].isInternal) Then
        Continue;

       Str := 'type<'+getTypeDeclaration(@mType)+'> '+Name+';';
      End;

     { global constant }
     if (Typ = gdConstant) Then
      With mCnst do
      Begin
       if (Visibility <> mvPublic) or (GlobalList[Item].isInternal) Then
        Continue;

       Str := 'const<'+getTypeDeclaration(Typ)+'> '+Name+' = '+getValueFromExpression(self, Value, True)+';';
      End;

     { function }
     if (Typ = gdFunction) Then
      With mFunc do
      Begin
       if (ModuleName <> self.ModuleName) or (Visibility <> mvPublic) {or (GlobalList[Item].isInternal)} Then
        Continue;

       Str := 'function<'+getTypeDeclaration(Return)+'> '+Name+'(';

       For I := Low(ParamList) To High(ParamList) Do
       Begin
        Str += getTypeDeclaration(ParamList[I].Typ);

        if (I <> High(ParamList)) Then
         Str += ', ';
       End;

       Str += ') in "'+ExtractFileName(self.OutputFile)+'";';
      End;

     if (Name <> '') Then
      Str := ' '+Str;

     Add(Str);
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
