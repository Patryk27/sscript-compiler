(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Compile1;

 Interface
 Uses Classes, SysUtils, Variants, FGL,
      Tokens, CompilerUnit, Scanner, Opcodes, Messages, MTypes,
      Parse_Function, Parse_VAR, Parse_CONST, Parse_RETURN, Parse_CODE, Parse_FOR, Parse_IF, Parse_WHILE, Parse_include;

 { constants }
 Const DEF_STACKSIZE = 1000000; // default stack size for compiled app
       Version       = '2.1'; // version of the compiler
       iVersion      = 2.1;

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
                    Procedure SaveBytecode(const FileName: String);

                   Public
                    TokenList: Array of TToken_P; // list of tokens (with stripped comments)
                    TokenPos : LongWord; // current token ID (counting from 0)
                    AnyError : Boolean;

                   Public
                    Parent      : TCompiler;
                    InputFile   : String; // input file name
                    OutputFile  : String; // output file name
                    ModuleName  : String; // module name
                    Options     : TCompileOptions; // compile options
                    Interpreter : Pointer; // pointer to a expression's interpreter class (in ExpressionCompiler.pas_
                    IncludePaths: TStringList; // list of include paths

                    CurrentDeep: Integer; // current brackets' deep (`{` = +1, `}` = -1)
                    Visibility : TMVisibility; // current visibility

                    StringList  : Array of TMString; // list of strings used in input file code
                    OpcodeList  : TOpcodeList; // output code opcode list
                    FunctionList: Array of TMFunction; // list of functions in current module/file (including those included from other files during compilation)
                    ExportList  : Array of TMExport; // exports
                    IncludeList : Array of TCompiler; // includes
                    ConstantList: Array of TMVariable; // global constants

                    TypeTable: Array of TMType; // type list

                    Scope: Array of TMScope; // scope list

                    SomeCounter: LongWord; // used in labels eg.`__while_<somecounter>`, so they don't overwrite each other

               { -> properties }
                    Property getPosition: LongWord read TokenPos; // current token position

               { -> methods }
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
                    Function read_type: TVType;
                    Procedure eat(Token: TToken);
                    Procedure semicolon;
                    Function getToken(const I: Integer=0): TToken_P;

                    Procedure ParseToken;
                    Procedure ParseCodeBlock(const AllowOneLineOnly: Boolean=False);

                    { output }
                    Procedure PutOpcode(fOpcode: TOpcode_E; fArgs: Array of Const; fTokenPos: LongWord=0);
                    Procedure PutOpcode(Opcode: TOpcode_E);
                    Procedure PutOpcode(Opcode: String; Args: Array of Const; fTokenPos: LongWord=0);
                    Procedure PutLabel(fName: String; const asConstruction: Boolean=False);
                    Procedure PutComment(fComment: String);

                    { type handling functions }
                    Procedure NewType(fName: String; fRegPrefix: Char; fInternalID, fArrayDimCount: Byte);
                    Function findTypeByName(Name: String): TVType;
                    Function getTypeName(ID: TVType): String;
                    Function getTypePrefix(ID: TVType): Char;
                    Function getTypeFromExpr(Expr: TMExpression): TVType;
                    Function CompareTypes(T1, T2: TVType): Boolean;
                    Function isTypeVoid(ID: TVType): Boolean;
                    Function isTypeString(ID: TVType): Boolean;
                    Function isTypeNumerical(ID: TVType): Boolean;
                    Function isTypeBool(ID: TVType): Boolean;
                    Function isTypeInt(ID: TVType): Boolean;
                    Function isTypeFloat(ID: TVType): Boolean;
                    Function isTypeArray(ID: TVType): Boolean;
                    Function isTypeChar(ID: TVType): Boolean;

                    { scope handling functions }
                    Procedure NewScope(const Typ: TMScopeType; LoopBegin: String=''; LoopEnd: String='');
                    Procedure RemoveScope;

                    { variable handling functions }
                    Function findFreeRegister(cRegChar: Char): Integer;
                    Function findVariable(fName: String; fDeep: Integer=-1): Integer;
                    Function getVariableType(ID: Integer): TVType;
                    Function getVariableRegID(ID: Integer): Integer;
                    Function getVariableRegChar(ID: Integer): Char;
                    Function isVariableConstant(ID: Integer): Boolean;
                    Procedure __variable_create(fName: String; fTyp: TVType; fRegID: Integer; fIsParam: Boolean);
                    Procedure __variable_setvalue_reg(VarID: Integer; RegID: Byte; RegChar: Char; const PushedValues: Integer=0);
                    Procedure __variable_getvalue_reg(VarID: Integer; RegID: Byte; RegChar: Char; const PushedValues: Integer=0);
                    Procedure __variable_getvalue_stack(VarID: Integer; const PushedValues: Integer=0);

                    Function isConstValue(Expr: TMExpression): Boolean;

                    { string handling functions }
                    Function findStringByName(Name: String): Integer;
                    Function findStringByContent(Value: String): Integer;

                    { functions for function handling (we need to go deeper! :)) }
                    Function getCurrentFunction: TMFunction;
                    Function getCurrentFunctionPnt: PMFunction;

                    Function findFunction(Name: String): Integer;
                    Function findFunctionByLabel(LabelName: String): Integer;

                    { handling global-things }
                    Function inFunction: Boolean;

                    Function findGlobalConstant(Name: String): Integer;

                    { compiling }
                    Procedure CompileCode(fInputFile, fOutputFile: String; fOptions: TCompileOptions; isIncluded: Boolean=False; fParent: TCompiler=nil);

                    Procedure CompileError(Token: TToken_P; Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Error: TCompileError);

                    Procedure GenerateHeaderFile(const fOutputFile: String);
                   End;

 Function makeModuleName(FileName: String): String;

 Implementation
Uses Compile2, ExpressionCompiler, SSM_parser, Peephole;
Type TVarRecArray = Array of TVarRec;
     PVarRecArray = ^TVarRecArray;

{ Log }
(*
 Displays text in parameter when a `-quiet` is disabled
*)
Procedure Log(Text: String);
Begin
 if (not getBoolOption('quiet', False)) Then
  Writeln(Text);
End;

{ makeModuleName }
(*
 Creates a module (bytecode label) name, based on FileName in parameter.
*)
Function makeModuleName(FileName: String): String;
Var Ch: Char;
Begin
 Result := '';
 For Ch in FileName Do // create module name (used in bytecode labels)
  if (Ch in ['a'..'z', 'A'..'Z', '0'..'9', '_']) Then
   Result += Ch Else
   Result += '_';
End;

{ TCompiler.Preparse }
(*
 Preparses code (removes comments etc.)
*)
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

  Token := Scanner.getNextToken_P(True, False); // ParseKeywords, not ParseComments

  if (Token.Token = noToken) Then
   Continue;

  Case Token.Token of
   _LONGCMT_OPEN  { /* }: isLongComment := True;
   _LONGCMT_CLOSE { /* }: isLongComment := False;
   _DOUBLE_SLASH  { // }: ShortCommentLine := Token.Line+1;
   else
    if (not isLongComment) and (Token.Line+1 <> ShortCommentLine) Then // add new token into the list
    Begin
     TokenList[TokenPos] := Token;
     Inc(TokenPos);
    End;
  End;
 End;

 TokenPos := 0;

 // free objects
 Scanner.Free;
 Code.Free;
End;

{ TCompiler.MakeImports }
(*
 Creates import list (opens *.ssm files and adds them into the bytecode)
*)
Procedure TCompiler.MakeImports;
Var SSM          : TSSM;
    I, Q         : Integer;
    Can, Found   : Boolean;
    FileName, Tmp: String;
    Comp         : Compile1.TCompiler;
Begin
 if not (_NINIT in Options) Then
 Begin
  SSM := TSSM.Create;

  FileName := SearchFile('init.ssm', Found);

  if (not Found) Then
   FileName := SearchFile('stdlib\init.ssm', Found);

  if (not SSM.Load(FileName, FileName, self, False)) Then
   CompileError(eCorruptedSSMFile, ['init.ssm']);

  SSM.Free;
 End;

 For I := Low(FunctionList) To High(FunctionList) Do
  if (FunctionList[I].LibraryFile <> '') Then
  Begin
   Can := True;

   For Q := Low(FunctionList) To I-1 Do // we don't want some file to be loaded eg.10 times instead of 1 time (searching for multiple imports from same file)
    if (FunctionList[Q].LibraryFile = FunctionList[I].LibraryFile) Then
     Can := False;

   if (not Can) Then
    Continue; // proceed to the next file

   FileName := FunctionList[I].LibraryFile;
   Comp     := TCompiler(FunctionList[I].mCompiler);

   FileName := SearchFile(FileName, Found);

   if (not Found) Then
   Begin
    Tmp := ExtractFilePath(Comp.InputFile)+FileName;
    if (FileExists(Tmp)) Then
     FileName := Tmp;
   End;

   SSM := TSSM.Create;
   if (not SSM.Load(FileName, Comp.ModuleName, self)) Then
    Comp.CompileError(FunctionList[I].DeclToken, eCorruptedSSMFile, [FunctionList[I].LibraryFile]);
   SSM.Free;
  End;
End;

{ TCompiler.SaveBytecode }
(*
 Saves verbal (mnemonic) bytecode into the file specified in parameter
*)
Procedure TCompiler.SaveBytecode(const FileName: String);
Var OutputCode: TStringList;
    Opcode    : PMOpcode;
    Arg       : TMOpcodeArg;
    Reg, Str  : String;
    StrID     : Integer;
Begin
 OutputCode := TStringList.Create;

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
   Str := Opcodes.OpcodeList[ord(Opcode)].Name+'('; // fetch opcode name

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

{ TCompiler.SearchFile }
(*
 Searches for file `FileName` (based on `-includepath`).
 If found, sets `Found` to true and returns file name by the result.
 If file couldn't be found, sets `Found` to false and returns `FileName`.
*)
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

{ TCompiler.setPosition }
(*
 Sets current parser's token position.
*)
Procedure TCompiler.setPosition(fTokenPos: LongWord);
Begin
 TokenPos := fTokenPos;
End;

{ TCompiler.AddString }
(*
 Adds string named `fName` with value `fValue` into the compiler's string list
*)
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

{ TCompiler.AddString }
(*
 Adds string with value `fValue` to the compiler's string list.
 Name is created by the count of strings in the list (string__0, string__1, ...) and returned by result
*)
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

{ TCompiler.AddConstruction }
(*
 Adds construction into the current function's construction list.
*)
Procedure TCompiler.AddConstruction(C: TMConstruction);
Begin
 if (Length(FunctionList) = 0) Then
  CompileError(eInternalError, ['Length(FunctionList) = 0']);

 With getCurrentFunctionPnt^ do // current function
 Begin
  SetLength(ConstructionList, Length(ConstructionList)+1);
  ConstructionList[High(ConstructionList)] := C;
 End;
End;

{ TCompiler.read }
(*
 Reads a token; skips any `noTokens` and shows an error on unfinished strings.
*)
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

{ TCompiler.read_t }
(*
 Reads only a token kind; see @TCompiler.read
*)
Function TCompiler.read_t: TToken;
Begin
 Result := read.Token;
End;

{ TCompiler.next }
(*
 Gets a next or previous (when `I` is negative) token
*)
Function TCompiler.next(const I: Integer=0): TToken_P;
Begin
 Result := TokenList[TokenPos+I];
End;

{ TCompiler.next_t }
(*
 Works just as TCompiler.next, but gets only a token kind
*)
Function TCompiler.next_t(const I: Integer=0): TToken;
Begin
 Result := next(I).Token;
End;

{ TCompiler.read_ident }
(*
 Reads an identifier; displays error `eExpectedIdentifier` when current token isn't an identifier.
*)
Function TCompiler.read_ident: String;
Begin
 if (next_t <> _IDENTIFIER) Then
  CompileError(eExpectedIdentifier, [next.Display]);
 Result := read.Display;
End;

{ TCompiler.read_type }
(*
 Reads a type name or a full type (based on current token); returns its ID.
*)
Function TCompiler.read_type: TVType;
Var Token: TToken_P;
Begin
 Result := -1;
 Token  := read;

 Case Token.Token of
  _IDENTIFIER: Result := findTypeByName(Token.Display);
  else CompileError(eExpectedIdentifier, [Token.Display]);
 End;

 if (Result = -1) Then
 Begin
  CompileError(eUnknownType, [Token.Display]);
  Exit(TYPE_ANY);
 End;
End;

{ TCompiler.eat }
(*
 'eats' a specified token; if current token isn't `Token`, displays a syntax error
*)
Procedure TCompiler.eat(Token: TToken);
Begin
 if (read_t <> Token) Then
  CompileError(eExpected, [getTokenDisplay(Token), next(-1).Display]);
End;

{ TCompiler.semicolon }
(*
 Eats a semicolon
*)
Procedure TCompiler.semicolon;
Begin
 eat(_SEMICOLON);
End;

{ TCompiler.getToken }
(*
 Works the same as TCompiler.next
*)
Function TCompiler.getToken(const I: Integer=0): TToken_P;
Begin
 Result := TokenList[TokenPos+I];
End;

{ TCompiler.ParseToken }
(*
 Parses current token (basing on current parser's scope)
*)
Procedure TCompiler.ParseToken;

{ ParseBreak }
Procedure ParseBreak;
Var I: Integer;
    C: TMConstruction;
Begin
 For I := High(Scope) Downto Low(Scope) Do
  if (Scope[I].Typ in [sFOR, sWHILE]) Then
  Begin
   C.Typ := ctJump;
   SetLength(C.Values, 1);
   C.Values[0] := GetMem(128);
   StrPCopy(C.Values[0], PChar(':'+Scope[I].LoopEnd));
   AddConstruction(C);
   eat(_SEMICOLON);
   Exit;
  End;

 CompileError(eNotAllowed, ['break']);
End;

{ ParseContinue }
Procedure ParseContinue;
Var I: Integer;
    C: TMConstruction;
Begin
 For I := High(Scope) Downto Low(Scope) Do
  if (Scope[I].Typ in [sFOR, sWHILE]) Then
  Begin
   C.Typ := ctJump;
   SetLength(C.Values, 1);
   C.Values[0] := GetMem(128);
   StrPCopy(C.Values[0], PChar(':'+Scope[I].LoopBegin));
   AddConstruction(C);
   eat(_SEMICOLON);
   Exit;
  End;

 CompileError(eNotAllowed, ['continue']);
End;

{ ParseMacro_Outside }
Procedure ParseMacro_Outside;

// @visibility
Procedure _visibility;
Var Str: String;
Begin
 Str := read.Display;
 Case Str of
  'public' : Visibility := mvPublic;
  'private': Visibility := mvPrivate;
  else Visibility := mvPrivate;
 End;
End;

Var Name: String;
Begin
 if (next_t = _IDENTIFIER) Then
 Begin
  Name := read_ident;
  eat(_BRACKET1_OP);
  Case Name of
   'visibility': _visibility;
   else CompileError(eUnknownMacro, [Name]);
  End;
  eat(_BRACKET1_CL);
 End Else
  Parse_include.Parse(self);
End;

Var Token : TToken_P;
    TmpVis: TMVisibility;
Begin
 Token := read;

 if (Length(Scope) = 0) Then // outside the function
 Begin
  Case Token.Token of
   _PUBLIC:
   Begin
    TmpVis     := Visibility;
    Visibility := mvPublic;
    ParseToken;
    Visibility := TmpVis;
   End;

   _PRIVATE:
   Begin
    TmpVis     := Visibility;
    Visibility := mvPrivate;
    ParseToken;
    Visibility := TmpVis;
   End;

   _CONST   : Parse_CONST.Parse(self, True);
   _FUNCTION: Parse_Function.Parse(self);
   _AT      : ParseMacro_Outside;

   else CompileError(eExpectedDeclOrDef, [Token.Display]);
  End;
 End Else // inside the function
 Begin
  Case Token.Token of
   _BRACKET3_OP: Inc(CurrentDeep);
   _BRACKET3_CL: Dec(CurrentDeep);
   _VAR        : Parse_VAR.Parse(self);
   _CONST      : Parse_CONST.Parse(self, False);
   _RETURN     : Parse_RETURN.Parse(self);
   _COLON      : if (next.Display = 'CODE') Then Parse_CODE.Parse(self) Else CompileError(eUnexpected, [next.Display]);
   _FOR        : Parse_FOR.Parse(self);
   _IF         : Parse_IF.Parse(self);
   _ELSE       : CompileError(eNotAllowed, ['else']);
   _WHILE      : Parse_WHILE.Parse(self);
   _DO         : Parse_WHILE.Parse_DO_WHILE(self);
   _BREAK      : ParseBreak;
   _CONTINUE   : ParseContinue;

   _SEMICOLON:

   Else
   Begin
    setPosition(getPosition-1); // go back 1 token
    AddConstruction(ExpressionCompiler.MakeConstruction(self)); // parse as expression
   End;
  End;
 End;
End;

{ TCompiler.ParseCodeBlock }
(*
 Parses the whole code block.

 When `AllowOneLineOnly` is `true`, allows constructions like this:
  just_something();
 (parses only one token)

 When disabled, there have to be brackets (`{` and `}`), e.g.:
  {
   something();
  }
*)
Procedure TCompiler.ParseCodeBlock(const AllowOneLineOnly: Boolean=False);
Var Deep: Integer;
Begin
 if (next_t <> _BRACKET3_OP) Then
  if (AllowOneLineOnly) Then
  Begin
   ParseToken;
   Exit;
  End Else
   CompileError(eExpected, ['{', next.Display]);

 Deep := CurrentDeep;
 Repeat
  ParseToken;
 Until (Deep = CurrentDeep);
End;

{ TCompiler.PutOpcode }
(*
 Creates opcode based on parameter list and adds it into the list.
 When opcode is invalid, displays `eBytecode_InvalidOpcode`
*)
Procedure TCompiler.PutOpcode(fOpcode: TOpcode_E; fArgs: Array of Const; fTokenPos: LongWord=0);
Var I, T: Integer;
    Str : String;
    iTmp: Integer;
    fTmp: Extended;
    Item: PMOpcode;

    DoCheck: Boolean;
Begin
 DoCheck := (fTokenPos <> 0); // check only bytecode generated by user

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

 // check opcode
 if (DoCheck) Then
  if (not isValidOpcode(Item^)) Then
   CompileError(Item^.Token^, eBytecode_InvalidOpcode, []);

 // add into the list
 OpcodeList.Add(Item);
End;

{ TCompiler.PutOpcode }
(*
 Adds opcode `Opcode` onto the list, without any parameters
*)
Procedure TCompiler.PutOpcode(Opcode: TOpcode_E);
Begin
 PutOpcode(Opcode, []);
End;

{ TCompiler.PutOpcode }
(*
 Creates opcode from the `Opcode` string and calls `TCompiler.PutOpcode` with suitable parameters
*)
Procedure TCompiler.PutOpcode(Opcode: String; Args: Array of Const; fTokenPos: LongWord=0);
Begin
 PutOpcode(TOpcode_E(GetOpcodeID(Opcode)), Args, fTokenPos); // find opcode with name stored in variable (parameter) `Opcode` and then put it into the list
End;

{ TCompiler.PutLabel }
(*
 Puts label, either directly into the code (when `asConstruction` = false) or as a construction
 (so the label will be created next to the previous opcodes), when `asConstruction` = true.

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
*)
Procedure TCompiler.PutLabel(fName: String; const asConstruction: Boolean=False);
Var Item: PMOpcode;
    C   : TMConstruction;
Begin
 if (asConstruction) Then
 Begin
  C.Typ := ctLabel;
  SetLength(C.Values, 1);
  GetMem(C.Values[0], Length(fName)+1);
  StrPCopy(C.Values[0], fName);
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

{ TCompiler.PutComment }
(*
 Puts a comment into the bytecode
*)
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

{ TCompiler.NewType }
(*
 Adds new type into the type list; should be used only for creating an internal types
*)
Procedure TCompiler.NewType(fName: String; fRegPrefix: Char; fInternalID, fArrayDimCount: Byte);
Begin
 SetLength(TypeTable, High(TypeTable)+2);
 With TypeTable[High(TypeTable)] do
 Begin
  Name      := fName;
  RegPrefix := fRegPrefix;

  InternalID    := fInternalID;
  ArrayDimCount := fArrayDimCount;

  isStrict := False;
 End;
End;

{ TCompiler.findTypeByName }
(*
 Searches the type table for type named `Name`.
 If found, returns its type ID; if not found, returns -1
*)
Function TCompiler.findTypeByName(Name: String): TVType;
Var I: Integer;
Begin
 Result := -1;

 For I := Low(TypeTable) To High(TypeTable) Do
  if (TypeTable[I].Name = Name) Then
   Exit(I);
End;

{ TCompiler.getTypeName }
(*
 Gets type name by type ID; also checks for valid ID (if ID is not valid, returns `<erroneous type>`)
*)
Function TCompiler.getTypeName(ID: TVType): String;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit('<erroneous type>');

 Exit(TypeTable[ID].Name);
End;

{ TCompiler.getTypePrefix }
(*
 Gets type register prefix; when ID is not valid, returns `i`.
*)
Function TCompiler.getTypePrefix(ID: TVType): Char;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit('i');

 Exit(TypeTable[ID].RegPrefix);
End;

{ TCompiler.getTypeFromExpr }
(*
 Gets type from an TMExpression; works only for constant (already folded) expressions (only a parent without children).
*)
Function TCompiler.getTypeFromExpr(Expr: TMExpression): TVType;
Begin
 if (Expr.Left <> nil) or (Expr.Right <> nil) Then
  CompileError(eInternalError, ['Folded expression was needed']);

 Case Expr.Typ of
  mtBool  : Exit(TYPE_BOOL);
  mtChar  : Exit(TYPE_CHAR);
  mtInt   : Exit(TYPE_INT);
  mtFloat : Exit(TYPE_FLOAT);
  mtString: Exit(TYPE_STRING);
  else CompileError(eInternalError, ['Invalid expression value']);
 End;
End;

{ TCompiler.CompareTypes }
(*
 Compares two types
*)
Function TCompiler.CompareTypes(T1, T2: TVType): Boolean;
Begin
 Result := True;

 if (T1 = TYPE_VOID) and (T2 <> TYPE_VOID) Then
  Exit(False);

 if (T1 = TYPE_ANY) or (T2 = TYPE_ANY) Then
  Exit(True);

 if (T1 = TYPE_FLOAT) and (T2 = TYPE_INT) Then
  Exit(True);

 if (T1 = TYPE_CHAR) and (T2 = TYPE_INT) Then
  Exit(True);

 if (T1 = TYPE_INT) and (T2 = TYPE_CHAR) Then
  Exit(True);

 if (T1 = TYPE_BOOL) and (T2 = TYPE_INT) Then
  Exit(True);

 //if (T1 = TYPE_STRING) and (T2 = TYPE_CHAR) Then
 // Exit(True);

 Exit(T1 = T2);
End;

{ TCompiler.isTypeVoid }
(*
 Returns `true`, when type passed in parameter is `void`-derived; in other case, returns `false`.
*)
Function TCompiler.isTypeVoid(ID: TVType): Boolean;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit(False);
 Exit(TypeTable[ID].InternalID = TYPE_VOID);
End;

{ TCompiler.isTypeString }
(*
 Returns `true`, when type passed in parameter is `string`-derived; in other case, returns `false`.
*)
Function TCompiler.isTypeString(ID: TVType): Boolean;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit(False);
 Exit(TypeTable[ID].InternalID = TYPE_STRING);
End;

{ TCompiler.isTypeNumerical }
(*
 Returns `true`, when type passed in parameter is a numerical type (int, float, char) (or numerical-derived); in other case, returns `false`.
*)
Function TCompiler.isTypeNumerical(ID: TVType): Boolean;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit(False);
 Exit(TypeTable[ID].InternalID in [TYPE_INT, TYPE_FLOAT, TYPE_CHAR]);
End;

{ TCompiler.isTypeBool }
(*
 Returns `true`, when type passed in parameter is `bool`-derived; in other case, returns `false`.
*)
Function TCompiler.isTypeBool(ID: TVType): Boolean;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit(False);
 Exit(TypeTable[ID].InternalID in [TYPE_BOOL, TYPE_INT]);
End;

{ TCompiler.isTypeInt }
(*
 Returns `true`, when type passed in parameter is `int`-derived; in other case, returns `false`.
*)
Function TCompiler.isTypeInt(ID: TVType): Boolean;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit(False);
 Exit(TypeTable[ID].InternalID = TYPE_INT);
End;

{ TCompiler.isTypeFloat }
(*
 Returns `true`, when type passed in parameter is `float`-derived; in other case, returns `false`.
 @Note: Also returns `false` when type is `int`!
*)
Function TCompiler.isTypeFloat(ID: TVType): Boolean;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit(False);
 Exit(TypeTable[ID].InternalID = TYPE_FLOAT);
End;

{ TCompiler.isTypeArray }
(*
 Returns `true`, when type passed in parameter is array; in other case, returns `false`.
*)
Function TCompiler.isTypeArray(ID: TVType): Boolean;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit(False);
 Exit(TypeTable[ID].InternalID = TYPE_STRING); // temporary (until I'll write code for array support)
End;

{ TCompiler.isTypeChar }
(*
 Returns `true`, when type passed in parameter is `char`-derived; in other case, returns `false`.
 @Note: Also returns `false` when type is `int`!
*)
Function TCompiler.isTypeChar(ID: TVType): Boolean;
Begin
 if (ID < 0) or (ID > High(TypeTable)) Then
  Exit(False);
 Exit(TypeTable[ID].InternalID in [TYPE_CHAR]);
End;

{ TCompiler.NewScope }
(*
 Makes a new scope typed `Typ`; when scope is a loop (supporting `continue` and `break`), there have to be
 passed `LoopBegin` and `LoopEnd`, which are labels' names (without preceding `:`)
*)
Procedure TCompiler.NewScope(const Typ: TMScopeType; LoopBegin: String=''; LoopEnd: String='');
Begin
 SetLength(Scope, Length(Scope)+1);
 Scope[High(Scope)].Typ       := Typ;
 Scope[High(Scope)].LoopBegin := LoopBegin;
 Scope[High(Scope)].LoopEnd   := LoopEnd;
End;

{ TCompiler.RemoveScope }
(*
 Removes the top (current) scope
*)
Procedure TCompiler.RemoveScope;
Begin
 if (Length(Scope) = 0) Then
  CompileError(eInternalError, ['Length(Scope) = 0']);
 SetLength(Scope, High(Scope));
End;

{ TCompiler.findFreeRegister }
(*
 Searches for any free register (third or fourth) of type `cRegChar`, based on variables.
 Should be called only for local (or temporary) variable allocation.
 When any free register is found, returns its ID; in other cases, returns -1.
*)
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

{ TCompiler.findVariable }
(*
 Finds a variable with specified name (`fName`) in specified scope (deep).
 When `fDeep` equals `-1`, searches for variable irrespectively of its scope.
*)
Function TCompiler.findVariable(fName: String; fDeep: Integer=-1): Integer;
Var I: Integer;
Begin
 Result := -1;

 if (fDeep = -1) Then
  fDeep := CurrentDeep;

 With getCurrentFunction do
  For I := Low(VariableList) To High(VariableList) Do
   if (VariableList[I].Deep <= fDeep) and (VariableList[I].Name = fName) Then
    Exit(I);
End;

{ TCompiler.getVariableType }
(*
 Gets specified variable's type
*)
Function TCompiler.getVariableType(ID: Integer): TVType;
Begin
 Result := getCurrentFunction.VariableList[ID].Typ;
End;

{ TCompiler.getVariableRegID }
(*
 Gets specified variable's register's ID (1..4)
*)
Function TCompiler.getVariableRegID(ID: Integer): Integer;
Begin
 Result := getCurrentFunction.VariableList[ID].RegID;
End;

{ TCompiler.getVariableRegChar }
(*
 Gets specified variable's register's char (b, c, i, f, s, r)
*)
Function TCompiler.getVariableRegChar(ID: Integer): Char;
Begin
 Result := getCurrentFunction.VariableList[ID].RegChar;
End;

{ TCompiler.isVariableConstant }
(*
 Returns `true` when a variable with specified ID is a constant.
*)
Function TCompiler.isVariableConstant(ID: Integer): Boolean;
Begin
 Result := getCurrentFunction.VariableList[ID].isConst;
End;

{ TCompiler.__variable_create }
(*
 Creates a variable in current function
*)
Procedure TCompiler.__variable_create(fName: String; fTyp: TVType; fRegID: Integer; fIsParam: Boolean);
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

{ TCompiler.__variable_setvalue_reg }
(*
 Sets a variable's value to the value stored in register identified by `RegChar`+`RegID`.

 When eg.a variable is located in `ei3` and it has ID 1, calling:
  __variable_setvalue_reg(1, 4, 'i');
 Will add opcode:
  mov(ei3, ei4)

 `PushedValues` is also important, because when a variable is allocated on the stack and something is pushed onto it, eg. pseudocode:
   push(ei3)
   __variable_setvalue_reg(...)
  We need to take care of that `push`, because we could overwrite not our variable, but some value on the stack.
*)
Procedure TCompiler.__variable_setvalue_reg(VarID: Integer; RegID: Byte; RegChar: Char; const PushedValues: Integer=0);
Var RegStr: String;
Begin
 RegStr := 'e'+RegChar+IntToStr(RegID); { get full register name (ei1, es3 etc.) }

 With getCurrentFunction.VariableList[VarID] do
 Begin
  if (isConst) Then
   CompileError(eInternalError, ['Cannot change a constant value']);

  if (RegID > 0) Then // variable is in the register
   PutOpcode(o_mov, ['e'+RegChar+IntToStr(RegID), RegStr]) Else
   PutOpcode(o_mov, ['['+IntToStr(RegID-PushedValues)+']', RegStr]); // variable is on the stack
 End;
End;

{ TCompiler.__variable_getvalue_reg }
(*
 Loads a variable's value onto the register identified by `RegChar`+`RegID`.
 See description above for info about `PushedValues`.

 Example:
 We have a string variable (with ID 2) loaded onto the `es4` and we want to load it's value into `es1`:
  __variable_getvalue_reg(2, 41 's');
 This will add one opcode:
  mov(es1, es4)
*)
Procedure TCompiler.__variable_getvalue_reg(VarID: Integer; RegID: Byte; RegChar: Char; const PushedValues: Integer=0);
Var RegStr: String;
Begin
 RegStr := 'e'+RegChar+IntToStr(RegID); { get full register name }

 With getCurrentFunction.VariableList[VarID] do
 Begin
  if (isConst) Then
  Begin
   PutOpcode(o_mov, [RegStr, getValueFromExpression(self, @Value)]);
  End Else
   if (RegID > 0) Then // variable is in the register
    PutOpcode(o_mov, [RegStr, 'e'+RegChar+IntToStr(RegID)]) Else
    PutOpcode(o_mov, [RegStr, '['+IntToStr(RegID-PushedValues)+']']); // variable is on the stack
 End;
End;

{ TCompiler.__variable_getvalue_stack }
(*
 Pushes variable's value onto the stack.
 See examples and descriptions above.
*)
Procedure TCompiler.__variable_getvalue_stack(VarID: Integer; const PushedValues: Integer=0);
Begin
 With getCurrentFunction.VariableList[VarID] do
 Begin
  if (isConst) Then
  Begin
   PutOpcode(o_push, [getValueFromExpression(self, @Value)]);
  End Else
   if (RegID > 0) Then // variable is in the register
    PutOpcode(o_push, ['e'+RegChar+IntToStr(RegID)]) Else
    PutOpcode(o_push, ['['+IntToStr(RegID-PushedValues)+']']); // variable is on the stack
 End;
End;

{ TCompiler.isConstValue }
(*
 Returns `true`, when a expression is a constant value.
 Passed expression must be already folded.
*)
Function TCompiler.isConstValue(Expr: TMExpression): Boolean;
Begin
 Result := (Expr.Left = nil) and (Expr.Right = nil) and (Expr.Typ in [mtBool, mtChar, mtInt, mtFloat, mtString]);
End;

{ TCompiler.findStringByName }
(*
 Searches for string named `Name` in the string table; returns its ID when found, or `-1` when not found.
*)
Function TCompiler.findStringByName(Name: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := Low(StringList) To High(StringList) Do
  if (StringList[I].Name = Name) Then
   Exit(I);
End;

{ TCompiler.findStringByContent }
(*
 Searches for string with content `Value` in the string table; returns its ID when found, or `-1` when not found.
 Used mainly for optimizations.
*)
Function TCompiler.findStringByContent(Value: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := Low(StringList) To High(StringList) Do
  if (StringList[I].Value = Value) Then
   Exit(I);
End;

{ TCompiler.getCurrentFunction }
(*
 Returns currently parsed function
*)
Function TCompiler.getCurrentFunction: TMFunction;
Begin
 Result := FunctionList[High(FunctionList)];
End;

{ TCompiler.getCurrentFunctionPnt }
(*
 Returns a pointer to currently parsed function
*)
Function TCompiler.getCurrentFunctionPnt: PMFunction;
Begin
 Result := @FunctionList[High(FunctionList)];
End;

{ TCompiler.findFunction }
(*
 Searches for function named `Name` and returns its ID (when found), or `-1` (when not found).
*)
Function TCompiler.findFunction(Name: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := Low(FunctionList) To High(FunctionList) Do
  if (FunctionList[I].Name = Name) Then
   Exit(I);
End;

{ TCompiler.findFunctionByLabel }
(*
 Searches for function with label-name `LabelName` and returns that function's ID (when found) or `-1` when not found.
*)
Function TCompiler.findFunctionByLabel(LabelName: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := Low(FunctionList) To High(FunctionList) Do
  if (FunctionList[I].MName = LabelName) Then
   Exit(I);
End;

{ TCompiler.inFunction }
(*
 Return `true` when parser is inside any function, or `false` when it's outside.
*)
Function TCompiler.inFunction: Boolean;
Begin
 Result := Length(Scope) > 0;
End;

{ TCompiler.findGlobalConstant }
(*
 Searches for global constant named `Name` and returns its ID (when found) or `-1` when constant doesn't exist
*)
Function TCompiler.findGlobalConstant(Name: String): Integer;
Var I: Integer;
Begin
 Result := -1;

 For I := Low(ConstantList) To High(ConstantList) Do
  if (ConstantList[I].Name = Name) Then
   Exit(I);
End;

{ TCompiler.CompileCode }
(*
 Compiles code:

Needed parameters:
 fInputFile  -> input *.ss file
 fOutputFile -> output compiled file
 fOptions    -> compiler options

Parameters set when compiling a module:
 isIncluded -> when `true`, no output code is saved into any file; instead, generated bytecode is sent into the parent compiler.
 fParent    -> parent compiler
*)
Procedure TCompiler.CompileCode(fInputFile, fOutputFile: String; fOptions: TCompileOptions; isIncluded: Boolean=False; fParent: TCompiler=nil);
Var Compiler2     : Compile2.TCompiler;
    MFunc         : TMFunction;
    VBytecode, Str: String;
    Item          : PMOpcode;
    I             : Integer;
Begin
 InputFile   := fInputFile;
 OutputFile  := fOutputFile;
 Options     := fOptions;
 SomeCounter := 0;
 Visibility  := mvPrivate;
 ModuleName  := '';
 AnyError    := False;
 Parent      := fParent;

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
 {$WARNING You can try to compile this compiler anyway (just comment this `if` above), but I'm not responsible for any damage...}
 {$FATAL :<}
 {$ENDIF}

 { parse `-includepath` }
 IncludePaths               := TStringList.Create;
 IncludePaths.Delimiter     := ';';
 IncludePaths.DelimitedText := getStringOption('includepath', '$file;$compiler');

 For I := 0 To IncludePaths.Count-1 Do
 Begin
  Str := IncludePaths[I];

  Str := StringReplace(Str, '$file', ExtractFilePath(InputFile), [rfReplaceAll]);
  Str := StringReplace(Str, '$main', ExtractFilePath(Parent.InputFile), [rfReplaceAll]);
  Str := StringReplace(Str, '$compiler', ExtractFilePath(ParamStr(0)), [rfReplaceAll]);

  IncludePaths[I] := Str;
 End;

 { 'generate verbal bytecode' output file and switch }
 VBytecode := getStringOption('s', '');

 { create classes }
 OpcodeList := TOpcodeList.Create;

 { allocate arrays }
 SetLength(ExportList, 0);
 SetLength(IncludeList, 0);
 SetLength(StringList, 0);
 SetLength(FunctionList, 0);
 SetLength(Scope, 0);
 SetLength(ConstantList, 0);

 Interpreter := ExpressionCompiler.TInterpreter.Create(self);

 { When a `-Cbcode` is specified: }
 if (_Cbcode in Options) Then
 Begin
  // @TODO: separate this into another procedure
  Log('-> Compiling as a bytecode');

  Preparse;
  CurrentDeep := 0;

  SetLength(FunctionList, 1);
  With FunctionList[0] do
  Begin
   Name  := 'main';
   MName := '__function_main';

   SetLength(ParamList, 0);
   SetLength(VariableList, 0);
   SetLength(ConstructionList, 0);
  End;

  Parse_CODE.Parse(self, True); // parse bytecode

  With FunctionList[0] do
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

  Compiler2 := Compile2.TCompiler.Create;
  Compiler2.Compile(self, getIntOption('stacksize', DEF_STACKSIZE), True);
  Compiler2.Free;

  Exit; // stop compiler
 End;

 if (not isIncluded) Then // are we the main file?
 Begin
  { compiling as a library }
  if (_Clib in Options) Then
  Begin
   Log('-> Compiling as a library');
   ModuleName := '_';
  End Else

  { compiling as a program }
  Begin
   // the beginning of the program must be an "init" and "main" function call

   if not (_NINIT in Options) Then
    PutOpcode(o_call, [':__init']);

   PutOpcode(o_call, [':__function_main_'+ModuleName+'_int_']);
   PutOpcode(o_stop); // and, if we back from main(), the program ends (virtual machine stops).
  End;
 End Else // if included (not the main file)
 Begin
  ModuleName := makeModuleName(fInputFile);
 End;

 { preparse code (parse tokens, remove comments etc.) }
 Preparse;

 { create primary type-table (don't change order!) }
 SetLength(TypeTable, 0);
 NewType('any', 'i', TYPE_ANY, 0);
 NewType('void', 'i', TYPE_VOID, 0);
 NewType('bool', 'b', TYPE_BOOL, 0);
 NewType('char', 'c', TYPE_CHAR, 0);
 NewType('int', 'i', TYPE_INT, 0);
 NewType('float', 'f', TYPE_FLOAT, 0);
 NewType('string', 's', TYPE_STRING, 0);

 { clear variables }
 CurrentDeep := 0;

 { parse code }
 Repeat
  ParseToken;
 Until (TokenList[getPosition].Token = noToken);

 { create an export list }
 For MFunc in FunctionList Do
  if (MFunc.ModuleName = ModuleName) and (MFunc.Visibility = mvPublic) Then // export only public functions from the compiled module (eg.if we included `somemodule.ss`, we don't want to export functions located in it)
  Begin
   SetLength(ExportList, Length(ExportList)+1);
   ExportList[High(ExportList)].Name := MFunc.MName;
   // @Note: ExportList[...].Pos will be set in Compile2
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

  if (_Op in OPTIONS) Then
  Begin
   Log('-> Optimizing bytecode...');
   Peephole.OptimizeBytecode(self);
  End;
 End;

 { if specified - save bytecode }
 if (VBytecode <> '') and (not isIncluded) Then
  SaveBytecode(VBytecode);

 { compile bytecode }
 if (not isIncluded) Then
 Begin
  Compiler2 := Compile2.TCompiler.Create;
  Compiler2.Compile(self, getIntOption('stacksize', DEF_STACKSIZE), _Clib in Options);
  Compiler2.Free;

  Str := getStringOption('h', '');

  { if specified - generate output header file }
  if (Str <> '') Then
  Begin
   Log('-> Generating output header file to: '+Str);
   GenerateHeaderFile(Str);
  End;
 End;
End;

(* TCompiler.CompileError *)
{
 Displays a error; when passed error is a `eInternalError` or belongs to `error_stop`, stops the compiler.
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
 CompileError(TokenList[TokenPos-1], Error, Args);
End;

(* TCompiler.CompileError *)
{
 See above
}
Procedure TCompiler.CompileError(Error: TCompileError);
Begin
 CompileError(Error, []);
End;

(* TCompiler.GenerateHeaderFile *)
{
 Generates a header file (based on current functions list) and saves it into file `fOutputFile.`
}
Procedure TCompiler.GenerateHeaderFile(const fOutputFile: String);
Var Output: TStringList;
    Func  : TMFunction;
    Cnst  : TMVariable;
    I     : Integer;
    Str   : String;
Begin
 Output := TStringList.Create;

 With Output do
 Begin
  Add('@visibility("public")');
  Add('');

  { constants }
  For Cnst in ConstantList Do
   With Cnst do
   Begin
    if (Visibility <> mvPublic) Then
     Continue;

    Add('const<'+getTypeName(Typ)+'> '+Name+' = '+getValueFromExpression(self, @Value, True)+';');
   End;

  { functions }
  For Func in FunctionList Do
   With Func do
   Begin
    if (ModuleName <> self.ModuleName) or (Visibility <> mvPublic) Then
     Continue;

    Str := 'function<'+getTypeName(Return)+'> '+Name+'(';

    For I := Low(ParamList) To High(ParamList) Do
    Begin
     Str += getTypeName(ParamList[I].Typ);

     if (I <> High(ParamList)) Then
      Str += ', ';
    End;

    Str += ') in "'+ExtractFileName(self.OutputFile)+'";';

    Add(Str);
   End;
 End;

 Output.SaveToFile(fOutputFile);
 Output.Free;
End;
End.
