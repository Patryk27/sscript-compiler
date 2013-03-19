(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Compile1;

 Interface
 Uses Classes, SysUtils, Variants, FGL,
      Parser, Tokens, CompilerUnit, Opcodes, Messages, MTypes, symdef,
      Parse_FUNCTION, Parse_VAR, Parse_CONST, Parse_RETURN, Parse_CODE, Parse_FOR, Parse_IF, Parse_WHILE, Parse_include, Parse_DELETE,
      Parse_NAMESPACE, Parse_TYPE, Parse_TRY_CATCH, Parse_THROW;

 { constants }
 Const Version  = '2.2'{$IFDEF NIGHTLY}+' nightly'{$ENDIF}; // version of the compiler
       iVersion = 2.2;

       bytecode_version_major = 0;
       bytecode_version_minor = 4;

 { types }
 // TMScope
 Type TMScopeType = (sFunction, sFOR, sIF, sWHILE, sTryCatch);

 Type TMScope = Record
                 Typ               : TMScopeType;
                 LoopBegin, LoopEnd: String;
                End;

 // TOpcodeVector
 Type TOpcodeList = specialize TFPGList<PMOpcode>;

 // TCompiler
 Type TCompiler = Class
                   Private
                    Procedure MakeImports;
                    Procedure CompileAsBytecode;
                    Procedure SaveBytecode(const FileName: String);

                   Public
                    Parser: TParser; // code parser

                    AnyError: Boolean;

                   Public
                    Parent      : TCompiler;
                    CompileMode : (cmApp, cmLibrary, cmBytecode); // compilation mode
                    InputFile   : String; // input file name
                    OutputFile  : String; // output file name
                    ModuleName  : String; // module name
                    Options     : TCompileOptions; // compile options
                    Interpreter : Pointer; // pointer to an expression's interpreter class (ExpressionCompiler.pas)
                    IncludePaths: TStringList; // list of include paths

                    CurrentNamespace  : Integer; // namespace in which we are (`namespace namespace_ame;`)
                    SelectedNamespaces: Array of Integer; // selected namespaces (`use namespace1, namespace2 (...);`)

                    OpcodeList: TOpcodeList; // output code opcode list

                    IncludeList: Array of TCompiler; // include
                    Scope      : Array of TMScope; // scope list

                    NamespaceList: TNamespaceList; // namespace list

                    SomeCounter: LongWord; // used in labels eg.`__while_<somecounter>_begin`, so they don't overwrite each other

               { -> methods }
                    Function getBoolOption(const Name: TCommandLineOption; oDefault: Boolean=False): Boolean; inline;
                    Function getStringOption(const Name: TCommandLineOption; oDefault: String=''): String; inline;
                    Function getIntOption(const Name: TCommandLineOption; oDefault: Integer): Integer; inline;

                    Function SearchFile(const FileName: String; out Found: Boolean): String;

                    Procedure AddConstruction(C: TMConstruction);

                   { parser }
                    Procedure ParseToken;
                    Procedure ParseCodeBlock(const AllowOneTokenOnly: Boolean=False);

                   { bytecode }
                    Function PutOpcode(fOpcode: TOpcode_E; fArgs: Array of Const; fToken: PToken_P=nil): PMOpcode;
                    Function PutOpcode(Opcode: TOpcode_E): PMOpcode;
                    Function PutOpcode(Opcode: String; Args: Array of Const; fToken: PToken_P=nil): PMOpcode;
                    Function PutLabel(fName: String; const asConstruction: Boolean=False): PMOpcode;
                    Procedure PutComment(fComment: String);

                    Function findLabel(Name: String): Integer;

                   { types }
                    Function NewTypeFromFunction(Func: TFunction): TType;

                    Function findGlobalType(TypeName: String): TType;
                    Function findType(TypeName: String): TType;

                    Function getTypeFromExpr(Expr: TMExpression): TType;

                   { scope }
                    Procedure NewScope(const Typ: TMScopeType; LoopBegin: String=''; LoopEnd: String='');
                    Procedure RemoveScope;

                   { variables }
                    Function findFreeRegister(cRegChar: Char): Integer;
                    Function findLocalVariable(fName: String; fDeep: Integer=-1): Integer;

                    Procedure __variable_create(fName: String; fTyp: TType; fMemPos: Integer; fAttributes: TVariableAttributes);
                    Function __allocate_var(const TryRegisters: Boolean; RegPrefix: Char=#0): Integer;

                    Function isConstantValue(Expr: TMExpression): Boolean;

                    Procedure findGlobalVariableCandidate(const VarName: String; Namespaces: TMIntegerArray; out VarID, NamespaceID: Integer; const Token: PToken_P=nil);

                   { functions }
                    Function getCurrentFunction: TFunction;

                    Function findFunction(FuncName: String; NamespaceID: Integer=-1): Integer;
                    Procedure findFunctionByLabel(const LabelName: String; out FuncID, NamespaceID: Integer);

                    Procedure findFunctionCallCandidate(const FuncName: String; Namespaces: TMIntegerArray; out FuncID, NamespaceID: Integer; const Token: PToken_P=nil);

                   { namespaces }
                    Function getCurrentNamespace: TNamespace;
                    Function getDefaultNamespace: TNamespace;

                    Function findNamespace(Name: String): Integer;

                   { global-things }
                    Function inFunction: Boolean;

                    Function findGlobalVariable(VarName: String; NamespaceID: Integer=-1): Integer;
                    Procedure findGlobalCandidate(const IdentName: String; Namespaces: TMIntegerArray; out IdentID, NamespaceID: Integer; const Token: PToken_P=nil);

                    Procedure RedeclarationCheck(Name: String; const SkipNamespaces: Boolean=False);

                   { compiling }
                    Procedure CompileCode(fInputFile, fOutputFile: String; fOptions: TCompileOptions; isIncluded: Boolean=False; fParent: TCompiler=nil);

                    Procedure CompileError(Token: TToken_P; Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Token: PToken_P; Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Error: TCompileError; Args: Array of Const);
                    Procedure CompileError(Error: TCompileError);

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
   For I := Low(SymbolList) To High(SymbolList) Do
    if (SymbolList[I].Typ = gsFunction) and (SymbolList[I].mFunction.LibraryFile <> '') Then // is it an imported function?
    Begin
     Can := True;

     For Q := Low(SymbolList) To I-1 Do // we don't want some file to be loaded eg.10 times instead of 1 time (it just skips multiple imports from same file)
      if (SymbolList[Q].Typ = gsFunction) Then
       if (SymbolList[Q].mFunction.LibraryFile = SymbolList[I].mFunction.LibraryFile) Then
        Can := False;

     if (not Can) Then
      Continue; // proceed to the next file

     FileName := SymbolList[I].mFunction.LibraryFile;
     Comp     := TCompiler(SymbolList[I].mFunction.mCompiler);

     FileName := SearchFile(FileName, Found);

     if (not Found) Then
     Begin
      Tmp := ExtractFilePath(Comp.InputFile)+FileName;
      if (FileExists(Tmp)) Then
       FileName := Tmp;
     End;

     SSM := TSSM.Create;
     if (not SSM.Load(FileName, Comp.ModuleName, self)) Then
      Comp.CompileError(SymbolList[I].mFunction.DeclToken, eCorruptedSSMFile, [SymbolList[I].mFunction.LibraryFile]);
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

 With getCurrentNamespace do
 Begin
  SetLength(SymbolList, 1); // at least one function have to be created (otherwise `AddConstruction` would fail)
  SymbolList[0] := TGlobalSymbol.Create;
  With SymbolList[0] do
  Begin
   Typ := gsFunction;

   mFunction := TFunction.Create;
   With mFunction do
   Begin
    Name := 'main';

    SetLength(ParamList, 0);
    SetLength(ConstructionList, 0);
   End;
  End;

  Parse_CODE.Parse(self, True); // parse bytecode

  With SymbolList[0].mFunction do
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
       ctInlineBytecode: PutOpcode(PChar(Values[0]), PVarRecArray(Values[1])^, Values[2]);

       else
        CompileError(eInternalError, ['Unexpected construction with type #'+IntToStr(ord(Typ))]);
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

(* TCompiler.AddConstruction *)
{
 Adds construction into the current function's construction list.
}
Procedure TCompiler.AddConstruction(C: TMConstruction);
Begin
 if (getCurrentFunction = nil) Then
  CompileError(eInternalError, ['No function found!']);

 C.Token := Parser.next_pnt(-1);

 With getCurrentFunction do // current function
 Begin
  SetLength(ConstructionList, Length(ConstructionList)+1);
  ConstructionList[High(ConstructionList)] := C;
 End;
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
Begin
 With Parser do
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
    _USE        : Parse_USE;
    _WHILE      : Parse_WHILE.Parse(self);
    _DO         : Parse_WHILE.Parse_DO_WHILE(self);
    _DELETE     : Parse_DELETE.Parse(self);
    _BREAK      : ParseBreak;
    _CONTINUE   : ParseContinue;
    _TYPE       : Parse_TYPE.Parse(self);
    _THROW      : Parse_THROW.Parse(self);
    _TRY        : Parse_TRY_CATCH.Parse(self);

    _ELSE, _NAMESPACE, _CATCH: CompileError(eNotAllowed, [Token.Display]);

    _SEMICOLON: // at pure semicolon, don't do anything

    else
    Begin
     Dec(TokenPos);
     AddConstruction(ExpressionCompiler.MakeConstruction(self)); // parse as expression
    End;
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
 With Parser do
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
End;

(* TCompiler.PutOpcode *)
{
 Creates an opcode basing on parameter list and adds it into the opcode list.
 When opcode is invalid, displays `eBytecode_InvalidOpcode`
}
Function TCompiler.PutOpcode(fOpcode: TOpcode_E; fArgs: Array of Const; fToken: PToken_P=nil): PMOpcode;
Var I, T: Integer;
    Str : String;
    iTmp: Integer;
    fTmp: Extended;
    Item: PMOpcode;

    DoCheck: Boolean;
Begin
 DoCheck := (fToken <> nil); // check only bytecode written by user

 if (fToken = nil) Then
  fToken := Parser.next_pnt(-1);

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
Function TCompiler.PutLabel(fName: String; const asConstruction: Boolean=False): PMOpcode;
Var Item: PMOpcode;
    C   : TMConstruction;
Begin
 if (asConstruction) Then
 Begin
  C.Typ := ctLabel;
  SetLength(C.Values, 2);
  C.Values[0] := CopyStringToPChar(fName);
  C.Values[1] := Parser.next_pnt(-1);
  AddConstruction(C);
  Exit(nil);
 End Else
 Begin
  New(Item);
  With Item^ do
  Begin
   Name      := fName;
   isLabel   := True;
   isComment := False;

   isFunctionBeginLabel := False;
  End;
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
 Result.DeclToken  := Func.DeclToken;
 Result.mCompiler  := Func.mCompiler;
 Result.Attributes := [taFunction];
End;

(* TCompiler.findType *)
{
 See TCompiler.findGlobalType
}
Function TCompiler.findType(TypeName: String): TType;
Begin
 Result := findGlobalType(TypeName); // @TODO: local types
End;

(* TCompiler.findGlobalType *)
{
 Searches the type table for type named `Name` in global and currently parsed namespace.
 If found, returns its ID; if not found, returns -1
}
Function TCompiler.findGlobalType(TypeName: String): TType;

 // Search
 Procedure Search(Namespace: TNamespace);
 Var I: Integer;
 Begin
  With Namespace do
   For I := Low(SymbolList) To High(SymbolList) Do
    if (SymbolList[I].Typ = gsType) and (SymbolList[I].mType.Name = TypeName) Then
    Begin
     Result := SymbolList[I].mType;
     Exit;
    End;
 End;

Begin
 Result := nil;

 Search(NamespaceList[0]);
 Search(getCurrentNamespace);
End;

(* TCompiler.getTypeFromExpr *)
{
 Gets a type from TMExpression; works only for constant (already folded) expressions (i.e. only a parent without any childrens).
}
Function TCompiler.getTypeFromExpr(Expr: TMExpression): TType;
Begin
 if (Expr.Left <> nil) or (Expr.Right <> nil) Then
  CompileError(eInternalError, ['Expected folded expression']);

 Case Expr.Typ of
  mtBool  : Exit(TYPE_BOOL);
  mtChar  : Exit(TYPE_CHAR);
  mtInt   : Exit(TYPE_INT);
  mtFloat : Exit(TYPE_FLOAT);
  mtString: Exit(TYPE_STRING);
  else CompileError(eInternalError, ['Invalid expression type']);
 End;
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
    if (Deep <= Parser.CurrentDeep) and (Typ.RegPrefix = cRegChar) and (MemPos > 0) Then // if register is already allocated, we exclude it from the `FreeRegs` list
     Exclude(FreeRegs, MemPos);

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
  fDeep := Parser.CurrentDeep;

 With getCurrentFunction do
 Begin
  For I := Low(VariableList) To High(VariableList) Do
   if (VariableList[I].Deep <= fDeep) and (VariableList[I].Name = fName) Then
    Exit(I);
 End;
End;

(* TCompiler.__variable_create *)
{
 Creates a variable in current function
}
Procedure TCompiler.__variable_create(fName: String; fTyp: TType; fMemPos: Integer; fAttributes: TVariableAttributes);
Begin
 With getCurrentFunction do
 Begin
  SetLength(VariableList, Length(VariableList)+1);
  VariableList[High(VariableList)] := TVariable.Create;
  With VariableList[High(VariableList)] do
  Begin
   Name       := fName;
   Typ        := fTyp;
   MemPos     := fMemPos;
   Attributes := fAttributes;
  End;
 End;
End;

(* TCompiler.__allocate_var *)
{
 Tries to allocate a variable; returns `TVariable.MemPos`
}
Function TCompiler.__allocate_var(const TryRegisters: Boolean; RegPrefix: Char=#0): Integer;
Var I: Integer;
Begin
 Result := -1;

 { allocate in register }
 if (TryRegisters) Then
  Result := findFreeRegister(RegPrefix);

 { allocate on the stack }
 if (Result = -1) Then
 Begin
  Result := 0;
  With getCurrentFunction do
   For I := Low(VariableList) To High(VariableList) Do
    if (VariableList[I].Deep <= Parser.CurrentDeep) and (VariableList[I].MemPos <= 0) and (not VariableList[I].DontAllocate) Then
     Dec(Result);
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
 This function searches also global constants.
}
Procedure TCompiler.findGlobalVariableCandidate(const VarName: String; Namespaces: TMIntegerArray; out VarID, NamespaceID: Integer; const Token: PToken_P=nil);
Type TCandidate = Record
                   VarID, Namespace: Integer;
                   Symbol          : TSymbol;
                  End;
Var List: Array of TCandidate;
    Tmp : Integer;
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
   List[High(List)].Symbol    := NamespaceList[NamespaceID].SymbolList[Tmp].mVariable;
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
  CompileError(Token, eAmbiguousVariable, [VarName]);
  CompileNote(Token, nCandidates, []);

  For Tmp := Low(List) To High(List) do
   With List[Tmp].Symbol do
    TCompiler(mCompiler).CompileNote(DeclToken, nCandidate, [asString(True)]);
 End;
End;

(* TCompiler.getCurrentFunction *)
{
 Returns currently parsed function
}
Function TCompiler.getCurrentFunction: TFunction;
Var I: Integer;
Begin
 Result := nil;

 With getCurrentNamespace do
  For I := High(SymbolList) Downto Low(SymbolList) Do
   if (SymbolList[I].Typ = gsFunction) Then
    Exit(SymbolList[I].mFunction);
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
  For I := Low(SymbolList) To High(SymbolList) Do
   With SymbolList[I] do
    if (Typ = gsFunction) and (mFunction.Name = FuncName) Then
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
   For Func := Low(SymbolList) To High(SymbolList) Do
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
 Searches for a function call candidate, basing on current namespaces and passed function's name.
 When a such candidate is not found, sets `FuncID` and `NamespaceID` to `-1`.

 Detects only 'ambiguous function call', so if function couldn't be found (`FuncID = -1`), you must display a
 `function not found` error by yourself.

 Also, when `Token == nil`, it's automatically set to current token.
}
Procedure TCompiler.findFunctionCallCandidate(const FuncName: String; Namespaces: TMIntegerArray; out FuncID, NamespaceID: Integer; const Token: PToken_P=nil);
Type TCandidate = Record
                   Func, Namespace: Integer;
                   Symbol         : TSymbol;
                  End;
Var List: Array of TCandidate;
    Tmp : Integer;
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
   List[High(List)].Symbol    := NamespaceList[NamespaceID].SymbolList[Tmp].mFunction;
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

  CompileError(Token, eAmbiguousCall, [FuncName]);
  CompileNote(Token, nCandidates, []);

  For Tmp := Low(List) To High(List) Do
   With List[Tmp].Symbol do
    TCompiler(mCompiler).CompileNote(DeclToken, nCandidate, [asString(True)]);
 End;
End;

(* TCompiler.getCurrentNamespace *)
{
 Returns current namespace (ie. - namespace which is currently parsed).
}
Function TCompiler.getCurrentNamespace: TNamespace;
Begin
 Result := NamespaceList[CurrentNamespace];
End;

(* TCompiler.getDefaultNamespace *)
{
 Returns default ("global") namespace.
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
  For I := Low(SymbolList) To High(SymbolList) Do
   With SymbolList[I] do
    if (Typ in [gsConstant, gsVariable]) and (mVariable.Name = VarName) Then
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
   For I := Low(SymbolList) To High(SymbolList) Do
    Case SymbolList[I].Typ of
     gsVariable, gsConstant:
      if (SymbolList[I].mVariable.Name = IdentName) Then
       Tmp := I;

     gsFunction:
      if (SymbolList[I].mFunction.Name = IdentName) Then
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
    Symbol    := NamespaceList[NamespaceID].SymbolList[Tmp].mFunction;

    Case NamespaceList[NamespaceID].SymbolList[Tmp].Typ of
     gsVariable, gsConstant: Typ := tVar;
     gsFunction            : Typ := tFunc;
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
   Tok := Parser.next_pnt(-1) Else
   Tok := Token;

  CompileError(Tok^, eAmbiguousIdentifier, [IdentName]);
  CompileNote(Tok^, nCandidates, []);

  For Tmp := Low(List) To High(List) Do
   With List[Tmp].Symbol do
    TCompiler(mCompiler).CompileNote(DeclToken, nCandidate, [asString(True)]);

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
  With getCurrentNamespace.SymbolList[ID].mFunction do
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
   With getCurrentNamespace.SymbolList[ID].mVariable do
    TCompiler(mCompiler).CompileError(DeclToken, ePrevDeclared, []);
   Exit;
  End;

  // global type
  Typ := findGlobalType(Name);
  if (Typ <> nil) Then
  Begin
   CompileError(eRedeclaration, [Name]);
   With Typ do
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
Var Compiler2     : Compile2.TCompiler;
    VBytecode, Str: String;

    I: Integer;

    // AddPrimaryType
    Procedure AddPrimaryType(Typ: TType);
    Begin
     With NamespaceList[0] do
     Begin
      SetLength(SymbolList, Length(SymbolList)+1);
      SymbolList[High(SymbolList)]            := TGlobalSymbol.Create;
      SymbolList[High(SymbolList)].Typ        := gsType;
      SymbolList[High(SymbolList)].mType      := Typ;
      SymbolList[High(SymbolList)].isInternal := True;
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

     With NamespaceList[0].SymbolList[FuncID].mFunction do
     Begin
      Result :=
      (Length(ParamList) = 0) and
      (Return.isInt) and
      (not Return.isObject);
     End;
    End;

Begin
 InputFile   := fInputFile;
 OutputFile  := fOutputFile;
 Options     := fOptions;
 SomeCounter := 0;
 ModuleName  := '';
 AnyError    := False;
 Parent      := fParent;

 Parser            := TParser.Create(self, InputFile);
 Parser.Visibility := mvPrivate;

 Try
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

  { quick compiler type-check }
  {$IF (sizeof(Byte) <> 1) or (sizeof(Char) <> 1) or (sizeof(Integer) <> 4) or (sizeof(LongWord) <> 4) or (sizeof(Extended) <> 10)}
  {$WARNING Invalid types sizes!}
  {$WARNING You can try to compile this compiler anyway (just remove this `$FATAL` below), but I'm not responsible for any damage...}
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
    Log('Unknown compile mode (-Cm): '+Str+'; set to `app`');
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
  SetLength(IncludeList, 0);

  SetLength(Scope, 0);

  SetLength(NamespaceList, 1);

  // init default namespace
  NamespaceList[0]            := TNamespace.Create;
  NamespaceList[0].Name       := '';
  NamespaceList[0].Visibility := mvPublic;
  SetLength(NamespaceList[0].SymbolList, 0);
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

  { create primary type-table }
  AddPrimaryType(TYPE_ANY);
  AddPrimaryType(TYPE_NULL);
  AddPrimaryType(TYPE_VOID);
  AddPrimaryType(TYPE_BOOL);
  AddPrimaryType(TYPE_CHAR);
  AddPrimaryType(TYPE_INT);
  AddPrimaryType(TYPE_FLOAT);
  AddPrimaryType(TYPE_STRING);

  { create global constants }
  With NamespaceList[0] do
  Begin
   Name := 'self';

   SetLength(SymbolList, Length(SymbolList)+1);
   SymbolList[High(SymbolList)] := TGlobalSymbol.Create;

   With SymbolList[High(SymbolList)] do
   Begin
    Typ       := gsConstant;
    mVariable := TVariable.Create;

    With mVariable do
    Begin
     Name       := 'null';
     Typ        := TYPE_NULL;
     Value      := MakeIntExpression(0);
     Visibility := mvPrivate;

     Include(Attributes, vaConst);
    End;

    isInternal := True;
   End;

   // @TODO: special constants
  End;

  { clear variables }
  Parser.CurrentDeep := 0;

  if (not isIncluded) Then
  Begin
   { add internal bytecode }
  End;

  { compile code }
  Repeat
   ParseToken;
  Until (Parser.TokenList[Parser.getPOsition].Token = noToken);

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
 CompileError(Parser.getToken(-1), Error, Args);
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
 CompileHint(Parser.getToken(-1), Hint, Args);
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
 CompileNote(Parser.getToken(-1), Note, Args);
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
    mFunc      : TFunction;
    mCnst      : TVariable;
    mType      : TType;
    Typ        : TGlobalSymbolType;
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

    For Item := Low(SymbolList) To High(SymbolList) Do
    Begin
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
       if (Visibility <> mvPublic) or (SymbolList[Item].isInternal) Then
        Continue;

       Str := 'type<'+mType.asString+'> '+Name+';';
      End;

     { global constant }
     if (Typ = gsConstant) Then
      With mCnst do
      Begin
       if (Visibility <> mvPublic) or (SymbolList[Item].isInternal) Then
        Continue;

       Str := 'const '+Name+' = '+getValueFromExpression(Value, True)+';';
      End;

     { function }
     if (Typ = gsFunction) Then
      With mFunc do
      Begin
       if (ModuleName <> self.ModuleName) or (Visibility <> mvPublic) {or (SymbolList[Item].isInternal)} Then
        Continue;

       Str := 'function<'+Return.asString+'> '+Name+'(';

       For I := Low(ParamList) To High(ParamList) Do
       Begin
        Str += ParamList[I].Typ.asString;

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
