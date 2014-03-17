(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit SSCompiler;

 Interface
 Uses Classes, SysUtils, Variants, FGL, Math,
      Logging, CommandLine, FlowGraph, Scanner, Tokens, Opcodes, Messages, Expression, symdef,
      Parse_FUNCTION, Parse_VAR, Parse_CONST, Parse_RETURN, Parse_CODE, Parse_FOR, Parse_IF, Parse_WHILE, Parse_include,
      Parse_NAMESPACE, Parse_TYPE, Parse_TRY_CATCH, Parse_THROW, Parse_FOREACH;

 { constants }
 Const Version = '2.2.4 nightly';
       vMajor  = 2.2;
       vMinor  = 4;

 { types }
 Type TCompiler = class;

 Type TOpcodeList = specialize TFPGList<PMOpcode>;
 Type TCompilePass = (_cp1, _cp2); // third pass is the actual function compiling; as it's performed at the end of each function, there's no need for an additional `_cp3` enum

 Type TCompilerArray = Array of TCompiler;
      PCompilerArray = ^TCompilerArray;

 Type TScopeType = (sctFunction, sctFor, sctForeach, sctIf, sctWhile, sctTryCatch);

 { TScope }
 Type TScope =
      Record
       Typ               : TScopeType;
       LoopBegin, LoopEnd: TCFGNode;
      End;

 { TCompiler }
 Type TCompiler =
      Class
       Private { methods }
        Procedure CompileAsBytecode;
        Procedure SaveBytecode(const FileName: String);

       Private { fields }
        Scanner: TScanner; // code scanner instance

       Public { fields }
        AnyError: Boolean; // 'true' if any error (`CompileError()`) was raised, 'false' otherwise.

        fCurrentRoot: TCFGNode; // when equal 'nil', nodes added by `CFGAddNode` will be added to current function's flow graph. Otherwise - to this.
        fCurrentNode: TCFGNode;

        PrevRootNodes: TCFGNodeList;

       Public { fields }
        Parent          : TCompiler;
        Supervisor      : TCompiler;
        PreviousInstance: TCompiler;
        CompileMode     : (cmApp, cmLibrary, cmBytecode); // compilation mode
        CompilePass     : TCompilePass;
        InputFile       : String; // input file name
        OutputFile      : String; // output file name
        ModuleName      : String; // module name
        IncludePaths    : TStringList; // list of include paths

        CurrentFunction : TFunction; // currently parsed (or compiled) function
        CurrentNamespace: TNamespace; // namespace in which we are (`namespace namespace_ame;`)

        NamespaceVisibilityList: TNamespaceVisibilityList; // range of selected namespaces, eg.`use xyz;`
        VisibilityStack        : Array[0..1024] of TVisibility;

        OpcodeList   : TOpcodeList; // output code opcode list
        IncludeList  : PCompilerArray; // global include list
        NamespaceList: TNamespaceList; // namespace list
        Scope        : Array of TScope; // current function scopes

        LabelCounter: uint32; // used in labels like eg.`__while_<somecounter>_begin`, so they don't overwrite each other

        DoNotStoreOpcodes: Boolean; // when equal `true`, any `PutOpcode` will not insert bytecode into the bytecode list. Affects also labels! Default: `false`.

        ParsingFORInitInstruction, ParsingForeachHeader: Boolean;

       Public { properties }
        Property getCurrentRoot: TCFGNode read fCurrentRoot;
     // Property getCurrentNode: TCFGNode read fCurrentNode;
        Function getCurrentNode: TCFGNode;

        Property getCurrentFunction: TFunction read CurrentFunction;

        Property getScanner: TScanner read Scanner;

       Public { methods }
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

       { scope }
        Procedure NewScope(const Typ: TScopeType; LoopBegin: TCFGNode=nil; LoopEnd: TCFGNode=nil);
        Procedure RemoveScope;
        Function inRange(Range: TRange; Position: Int64=-1): Boolean;

       { types }
        Function CreateFunctionType(Func: TFunction): TType;
        Function findTypeCandidate(const TypeName: String; const Namespace: TNamespace; const Token: TToken_P): TType;

       { variables }
        Procedure __variable_create_stackpos(fName: String; fTyp: TType; fStackPos: int8; fAttributes: TVariableAttributes);
        Function findVariableCandidate(const VarName: String; const Namespace: TNamespace; const Token: TToken_P): TVariable;

       { functions }
        Function findFunction(const FuncName: String; Namespace: TNamespace=nil): TFunction;
        Function findFunctionByLabel(const LabelName: String): TFunction;
        Function findFunctionCallCandidate(const FuncName: String; const Namespace: TNamespace; const Token: TToken_P): TFunction;

        Procedure CFGAddNode(Node: TCFGNode);

       { namespaces }
        Function getCurrentNamespace: TNamespace;
        Function getDefaultNamespace: TNamespace;

        Function findNamespace(const Name: String): TNamespace;

       { global things }
        Function inFunction: Boolean;

        Function findCandidate(const IdentName: String; const Namespace: TNamespace; const Token: TToken_P): TSymbol;
        Procedure RedeclarationCheck(Name: String; const SkipNamespaces: Boolean=False);

       { compiling }
        Procedure CompileCode(fInputFile, fOutputFile: String; isIncluded: Boolean=False; Pass1Only: Boolean=False; fParent: TCompiler=nil; fSupervisor: TCompiler=nil; fPreviousInstance: TCompiler=nil);

       { error reporting }
        Procedure CompileError(const Token: TToken_P; const Error: TCompileError; const Args: Array of Const);
        Procedure CompileError(const Token: PToken_P; const Error: TCompileError; const Args: Array of Const);
        Procedure CompileError(const Error: TCompileError; const Args: Array of Const);
        Procedure CompileError(const Error: TCompileError);

        Procedure CompileWarning(const Token: TToken_P; const Warning: TCompileWarning; const Args: Array of Const);
        Procedure CompileWarning(const Token: PToken_P; const Warning: TCompileWarning; const Args: Array of Const);
        Procedure CompileWarning(const Warning: TCompileWarning; const Args: Array of Const);
        Procedure CompileWarning(const Warning: TCompileWarning);

        Procedure CompileHint(const Token: TToken_P; const Hint: TCompileHint; const Args: Array of Const);
        Procedure CompileHint(const Token: PToken_P; const Hint: TCompileHint; const Args: Array of Const);
        Procedure CompileHint(const Hint: TCompileHint; const Args: Array of Const);
        Procedure CompileHint(const Hint: TCompileHint);

        Procedure CompileNote(const Token: TToken_P; const Note: TCompileNote; const Args: Array of Const);
        Procedure CompileNote(const Token: PToken_P; const Note: TCompileNote; const Args: Array of Const);
        Procedure CompileNote(const Note: TCompileNote; const Args: Array of Const);
        Procedure CompileNote(const Note: TCompileNote);
       End;

 Function ReplaceDirSep(const FileName: String): String;
 Function makeModuleName(const FileName: String): String;
 Function CopyStringToPChar(const Str: String): PChar;

 Implementation
Uses BCCompiler, ExpressionCompiler, ExpressionParser, PeepholeOptimizer;

(* ReplaceDirSep *)
{
 Replaces `/` and/or `\` to the appropriate directory separator
}
Function ReplaceDirSep(const FileName: String): String;
Const Sep = DirectorySeparator;
Begin
 Result := FileName;
 Result := StringReplace(Result, '/', Sep, [rfReplaceAll]);
 Result := StringReplace(Result, '\', Sep, [rfReplaceAll]);
 Result := StringReplace(Result, Sep+Sep, Sep, [rfReplaceAll]);
End;

(* makeModuleName *)
{
 Creates a module (bytecode label) name based on FileName in parameter.
}
Function makeModuleName(const FileName: String): String;
Var Ch: Char;
Begin
 Result := '';

 For Ch in FileName Do // create module name (used in bytecode labels)
 Begin
  if (Ch in ['a'..'z', 'A'..'Z', '0'..'9', '_']) Then
   Result += Ch Else
   Result += '_';
 End;
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

(* TCompiler.CompileAsBytecode *)
{
 Compiles input file as a bytecode
}
Procedure TCompiler.CompileAsBytecode;
Var Compiler2: BCCompiler.TCompiler;
Begin
 Log('-> Compiling as bytecode');

 With Scanner do
  if (next_t <> _BRACKET3_OP) Then
   CompileError(eExpected, ['{', next.Value]);

 Parse_CODE.Parse(self, True); // parse bytecode

 Compiler2 := BCCompiler.TCompiler.Create;
 Compiler2.Compile(self, False);
 Compiler2.Free;
End;

(* TCompiler.SaveBytecode *)
{
 Saves verbal (mnemonic) bytecode into the file specified in parameter.
}
Procedure TCompiler.SaveBytecode(const FileName: String);
Var OutputCode         : TStringList;
    Opcode             : PMOpcode;
    Arg                : TMOpcodeArg;
    Reg, Line, Str, Tmp: String;
    Int                : Integer;
Begin
 OutputCode := TStringList.Create;

 Log('-> Saving bytecode to file: '+FileName);

 // save bytecode
 OutputCode.Add('{');
 For Opcode in OpcodeList Do
  With Opcode^ do
  Begin
   { label }
   if (isLabel) Then
   Begin
    Case isPublic of
     True : OutputCode.Add(Name+': .public');
     False: OutputCode.Add(Name+':'); // labels are implicitly private so there's no need to add the `.private` modifier here.
    End;
    Continue; // proceed to the next opcode
   End;

   { comment }
   if (isComment) Then
   Begin
    OutputCode.Add('// '+Name);
    Continue;
   End;

   { opcode }
   Line := Opcodes.OpcodeList[ord(Opcode)].Name+'('; // fetch current opcode's name

   { opcode's parameters }
   For Arg in Args Do
   Begin
    Reg := '';

    Case Arg.Typ of
     ptBoolReg       : Reg := 'eb';
     ptCharReg       : Reg := 'ec';
     ptIntReg        : Reg := 'ei';
     ptFloatReg      : Reg := 'ef';
     ptStringReg     : Reg := 'es';
     ptReferenceReg  : Reg := 'er';
     ptStackVal      : Reg := '[';
     ptConstantMemRef: Reg := '&';
     ptSymbolMemRef  : Reg := '&$';
    End;

    Str := VarToStr(Arg.Value);

    if (Length(Str) > 2) and (Str[1] in [':', '@']) Then // check for function-label references
    Begin
     Tmp := Copy(Str, 2, Length(Str));
     if (Copy(Tmp, 1, 10) = '$function.') Then
     Begin
      Delete(Tmp, 1, 10);
      Int   := StrToInt(Tmp);
      Tmp   := TFunction(Int).LabelName;

      if (Length(Tmp) = 0) Then
       SSCompiler.TCompiler(Compiler).CompileError(eInternalError, ['Couldn''t fetch function''s label name; funcname = '+TSymbol(Int).mFunction.RefSymbol.Name]);

      Str := Str[1] + Tmp;
     End;
    End;

    Reg += Str;

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

    Line += Reg+',';
   End;

   if (Line[Length(Line)] = ',') Then
    Delete(Line, Length(Line), 1);

   Line += ')';

   OutputCode.Add(Line);
  End;

 OutputCode.Add('}');
 OutputCode.SaveToFile(FileName);
 OutputCode.Free;
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
 Parses current token (basing on current scanner scope)
}
Procedure TCompiler.ParseToken;

{$I parse_break_continue.pas}
{$I parse_use.pas}

// main block
Var TmpVisibility: TVisibility;
    Token        : TToken_P;
    Deep         : uint32;
    Node         : TCFGNode;
Begin
 With Scanner do
 Begin
  Token := read;

  if (not inFunction) Then // in global scope
  Begin
   Case Token.Token of
    { public/private }
    _PUBLIC, _PRIVATE:
    Begin
     TmpVisibility := Visibility;

     if (Token.Token = _PUBLIC) Then
      Visibility := mvPublic Else
      Visibility := mvPrivate;

     if (next_t = _SEMICOLON) Then // "public;" or "private;"
     Begin
      eat(_SEMICOLON);
      Exit;
     End;

     Deep := CurrentDeep;

     Repeat // parse declaration(s)
      ParseToken;
     Until (CurrentDeep = Deep);

     Visibility := TmpVisibility;
    End;

    { other }
    _BRACKET3_OP:
    Begin
     Inc(CurrentDeep);
     VisibilityStack[CurrentDeep] := Visibility;
    End;

    _BRACKET3_CL:
    Begin
     if (CurrentDeep = 0) Then
      CompileError(eExpectedDeclOrDef, [Token.Value]);

     Visibility := VisibilityStack[CurrentDeep];
     Dec(CurrentDeep);
    End;

    _NAMESPACE: Parse_NAMESPACE.Parse(self);
    _USE      : Parse_USE;
    _CONST    : Parse_CONST.Parse(self);
    _VAR      : Parse_VAR.Parse(self);
    _FUNCTION : Parse_Function.Parse(self);
    _AT       : Parse_include.Parse(self);
    _TYPE     : Parse_TYPE.Parse(self);

    else CompileError(eExpectedDeclOrDef, [Token.Value]);
   End;
  End Else // in function scope
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
    _BREAK      : ParseBreak;
    _CONTINUE   : ParseContinue;
    _TYPE       : Parse_TYPE.Parse(self);
    _THROW      : Parse_THROW.Parse(self);
    _TRY        : Parse_TRY_CATCH.Parse(self);
    _FOREACH    : Parse_FOREACH.Parse(self);

    _ELSE, _FUNCTION, _NAMESPACE, _CATCH: CompileError(eNotAllowed, [Token.Value]);

    _SEMICOLON: ; // at semicolon, don't do anything

    else
    Begin
     Dec(TokenPos);
     Node := getCurrentFunction.createNode(fCurrentNode, cetExpression, ExpressionCompiler.MakeExpression(self)); // parse as expression
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
 With Scanner do
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
Var Deep: uint32;
Begin
 With Scanner do
 Begin
  if (next_t <> _BRACKET3_OP) Then
   if (AllowOneTokenOnly) Then
   Begin
    if (next_t in [_VAR, _CONST]) Then
    Begin
     {
      eg.:
       if (true)
        var<int> a; else
        var<int> b;

      is an invalid construction.
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
Function TCompiler.PutOpcode(fOpcode: TOpcode_E; fArgs: Array of const; fToken: PToken_P): PMOpcode;
Var I, T: Integer;
    Str : String;
    iTmp: Int64;
    fTmp: Extended;
    Item: PMOpcode;

    DoCheck: Boolean;
Begin
 DoCheck := (fToken <> nil); // check only bytecode written by user

 if (fToken = nil) Then
  fToken := Scanner.next_pnt;

 New(Item);
 With Item^ do
 Begin
  isLabel    := False; // opcode cannot be a label
  isComment  := False; // nor a comment
  isFunction := False; // nor a function label
  Token      := fToken;

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
     if (Str[1] = '"') and (Str[Length(Str)] = '"') Then // string literal
     Begin
      Delete(Str, 1, 1); // remove quote chars
      Delete(Str, Length(Str), 1);
      Typ   := ptString;
      Value := Str;
     End Else

     if (Str[1] = '''') and (Str[Length(Str)] = '''') Then // char literal
     Begin
      Delete(Str, 1, 1);
      Delete(Str, Length(Str), 1);

      if (Length(Str) <> 1) Then
       CompileError(eInternalError, ['Char literals must have ''length'' = ''1''']);

      Typ   := ptChar;
      Value := ord(Str[1]);
     End Else

     if (Str[1] = '[') and (Str[Length(Str)] = ']') Then // stackval
     Begin
      Typ   := ptStackVal;
      Value := StrToInt(Copy(Str, 2, Length(Str)-2));
     End Else

     if (Str[1] = '#') Then // char
     Begin
      Delete(Str, 1, 1);
      Typ := ptChar;

      if (TryStrToInt64(Str, iTmp)) Then
       Value := iTmp Else
       CompileError(Token^, eBytecode_InvalidOpcode, []);
     End Else

     if (Str[1] = ':') Then // label
     Begin
      Value := Str;
      Typ   := ptInt;
     End Else

     if (Str[1] = '&') Then // constant memory/memory symbol reference
     Begin
      Delete(Str, 1, 1);

      if (Str[1] = '$') Then // memory symbol reference
      Begin
       Delete(Str, 1, 1);

       Typ   := ptSymbolMemRef;
       Value := Str;
      End Else // memory reference
      Begin
       Typ := ptConstantMemRef;

       if (TryStrToInt64(Str, iTmp)) Then
        Value := iTmp Else
        CompileError(Token^, eBytecode_InvalidOpcode, []);
      End;
     End Else

     Begin // something other, possibly a constant int/float value
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

 if (DoNotStoreOpcodes) Then
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
Function TCompiler.PutOpcode(Opcode: String; Args: Array of const; fToken: PToken_P): PMOpcode;
Begin
 if (GetOpcodeID(Opcode) = -1) Then
  CompileError(Scanner.next(-1), eBytecode_InvalidOpcode, []);

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
  Node                     := getCurrentFunction.createNode(fCurrentNode, cetBytecode, nil, Scanner.next_pnt(-1));
  Node.Bytecode.OpcodeName := '';
  Node.Bytecode.LabelName  := fName;

  CFGAddNode(Node);

  Exit(nil);
 End Else
 Begin
  New(Item);
  With Item^ do
  Begin
   Name       := fName;
   isLabel    := True;
   isComment  := False;
   isFunction := False;

   isPublic := False;
  End;

  if (DoNotStoreOpcodes) Then
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

(* TCompiler.CreateFunctionType *)
{
 Creates new function pointer type.

 Eg.: from function
   function<int> do_something(int[] tab)

 creates type:
   type<function<int>(int[])> you_have_to_name_it_by_yourself;
}
Function TCompiler.CreateFunctionType(Func: TFunction): TType;
Begin
 Result := TType.Create;

 Result.RegPrefix  := 'r';
 Result.InternalID := TYPE_INT_id;
 Result.FuncParams := Func.ParamList;
 Result.FuncReturn := Func.Return;
 Result.Attributes := [taFunction];
End;

(* TCompiler.findTypeCandidate *)
{
 See @TCompiler.findCandidate
}
Function TCompiler.findTypeCandidate(const TypeName: String; const Namespace: TNamespace; const Token: TToken_P): TType;
Var Candidate: TSymbol;
Begin
 Candidate := findCandidate(TypeName, Namespace, Token);

 if (Candidate <> nil) and (Candidate.Typ = stType) Then
  Result := Candidate.mType Else
  Result := nil;
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
  Position := Scanner.next(-1).Position;

 Exit(Math.inRange(Position, Range.PBegin.Position, Range.PEnd.Position));
End;

(* TCompiler.__variable_create_stackpos *)
{
 Creates a variable in current function
}
Procedure TCompiler.__variable_create_stackpos(fName: String; fTyp: TType; fStackPos: int8; fAttributes: TVariableAttributes);
Begin
 With getCurrentFunction do
 Begin
  SymbolList.Add(TSymbol.Create(stVariable));

  With SymbolList.Last do
  Begin
   Name := fName;

   With mVariable do
   Begin
    Typ           := fTyp;
    Attributes    := fAttributes;
    Range         := Scanner.getCurrentRange;
    DeclNamespace := getCurrentNamespace;
    DeclFunction  := getCurrentFunction;

    LocationData.Location      := vlStack;
    LocationData.StackPosition := fStackPos;
   End;
  End;
 End;
End;

(* TCompiler.findVariableCandidate *)
{
 See @TCompiler.findCandidate
 This function searches variables and constants.
}
Function TCompiler.findVariableCandidate(const VarName: String; const Namespace: TNamespace; const Token: TToken_P): TVariable;
Var Candidate: TSymbol;
Begin
 Candidate := findCandidate(VarName, Namespace, Token);

 if (Candidate <> nil) and (Candidate.Typ in [stVariable, stConstant]) Then
  Result := Candidate.mVariable Else
  Result := nil;
End;

(* TCompiler.findFunction *)
{
 Searches for a function named `Name` in namespace `Namespace` and returns it (when found), or return `nil` (when not found).
 When `NamespaceID` equals `-1`, it's set to currently parsed namespace.
}
Function TCompiler.findFunction(const FuncName: String; Namespace: TNamespace=nil): TFunction;
Begin
 if (Namespace = nil) Then
  Namespace := CurrentNamespace;

 Result := Namespace.findFunction(FuncName);
End;

(* TCompiler.findFunctionByLabel *)
{
 Searches for function with label name `LabelName` and returns that function (when found), or `nil` (when not found).
}
Function TCompiler.findFunctionByLabel(const LabelName: String): TFunction;
Var Namespace: TNamespace;
    Symbol   : TSymbol;
Begin
 Result := nil;

 For Namespace in NamespaceList Do
  For Symbol in Namespace.SymbolList Do
   if (Symbol.Typ = stFunction) and (Symbol.mFunction.LabelName = LabelName) Then
    Exit(Symbol.mFunction);
End;

(* TCompiler.findFunctionCallCandidate *)
{
 Searches for a function call candidate.
 When such candidate cannot be found, returns `nil`.

 Detects only 'ambiguous function call', so if such function couldn't have been found (`Result = nil`), you must display the `function not found` error by yourself.
}
Function TCompiler.findFunctionCallCandidate(const FuncName: String; const Namespace: TNamespace; const Token: TToken_P): TFunction;
Var Candidate: TSymbol;
Begin
 Candidate := findCandidate(FuncName, Namespace, Token);

 if (Candidate <> nil) and (Candidate.Typ = stFunction) Then
  Result := Candidate.mFunction Else
  Result := nil;
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
   fCurrentNode.Edges.Add(Node);
   fCurrentNode := Node;
  End;
End;

(* TCompiler.getCurrentNamespace *)
{
 Returns current namespace (ie. - namespace which currently being compiled).
}
Function TCompiler.getCurrentNamespace: TNamespace;
Begin
 Result := CurrentNamespace;
End;

(* TCompiler.getDefaultNamespace *)
{
 Returns the default ("self") namespace.
}
Function TCompiler.getDefaultNamespace: TNamespace;
Begin
 Result := NamespaceList[0]; // Result := findNamespace('self');
End;

(* TCompiler.findNamespace *)
{
 Searches for a namespace with specified name.
 Returns its ID when found, or `-1` when not found.
}
Function TCompiler.findNamespace(const Name: String): TNamespace;
Begin
 For Result in NamespaceList Do
  if (Result.RefSymbol.Name = Name) Then
   Exit;

 Exit(nil);
End;

(* TCompiler.inFunction *)
{
 Return `true` when scanner is inside any function, or `false` when it's outside.
}
Function TCompiler.inFunction: Boolean;
Begin
 Result := (Length(Scope) > 0);
End;

(* TCompiler.findCandidate *)
{
 This function searches for local and global variables, constants, functions and types.
}
Function TCompiler.findCandidate(const IdentName: String; const Namespace: TNamespace; const Token: TToken_P): TSymbol;
Var Namespaces         : TNamespaceList;
    NamespaceVisibility: PNamespaceVisibility;
    TmpNamespace       : TNamespace;
    List               : TSymbolList;
    Symbol, Tmp        : TSymbol;
Begin
 Result := nil;

 // step 1: fetch namespaces reachable at given token
 Namespaces := TNamespaceList.Create;
 List       := TSymbolList.Create;

 Try
  Namespaces.Add(getDefaultNamespace);

  if (Namespaces.IndexOf(getCurrentNamespace) = -1) Then
   Namespaces.Add(getCurrentNamespace);

  if (Namespace <> nil) Then
   if (Namespaces.IndexOf(Namespace) = -1) Then
    Namespaces.Add(Namespace);

  For NamespaceVisibility in NamespaceVisibilityList Do
   if (Token in NamespaceVisibility^.Range) Then
    if (Namespaces.IndexOf(NamespaceVisibility^.Namespace) = -1) Then
     Namespaces.Add(NamespaceVisibility^.Namespace);

  // step 2: search for specified identifier in current function
  if (inFunction) Then
  Begin
   // @Note: we cannot use `TFunction.findSymbol`, as there can be multiple variables with the same name inside one function, so `findSymbol` would return only the first one, but not the rest.
   For Tmp in getCurrentFunction.SymbolList Do
    if (Tmp.Name = IdentName) and (Token in Tmp.Range) Then
     List.Add(Tmp);
  End;

  // step 3: search for specified identifier in namespaces
  For TmpNamespace in Namespaces Do
  Begin
   Symbol := TmpNamespace.findSymbol(IdentName);

   // found?
   if (Symbol <> nil) Then
    if (Token in Symbol.Range) Then
     List.Add(Symbol);
  End;

  { symbol not found }
  if (List.Count = 0) Then
  Begin
   Result := nil;
  End Else

  { exactly one symbol found }
  if (List.Count = 1) Then
  Begin
   Result := List[0];
  End Else

  { possibly ambiguous reference (multiple symbols with the same names) }
  Begin
   Result := nil;

   if (inFunction) Then
   Begin
    (*
     @Note: there can happen a situation where (at least) two different identifiers with the same name are visible from the same scope and it's not an ambiguous reference, eg:

     function<void> foo()
     {
     }

     function<void> do_something()
     {
      var<int> foo;

      foo = 10; // this is an "ambiguous" reference, which really should be resolved to the local variable "foo", not error (because 'local scope' > 'global scope'). That's what this code below does.
     }
    *)

    For Symbol in List Do
     if (Symbol.DeclFunction = getCurrentFunction) Then
      Exit(Symbol);
   End;

   CompileError(Token, eAmbiguousIdentifier, [IdentName]);
   CompileNote(Token, nCandidates, []);

   For Symbol in List Do
    if (Symbol.mCompiler <> nil) and (Symbol.DeclToken <> nil) Then
     TCompiler(Symbol.mCompiler).CompileNote(Symbol.DeclToken, nCandidate, [Symbol.DeclNamespace.RefSymbol.Name+'::'+Symbol.Name]);
  End;
 Finally
  List.Free;
  Namespaces.Free;
 End;
End;

(* TCompiler.RedeclarationCheck *)
{
 Returns `true` if specified identifier is redeclared **in current scope**.
}
Procedure TCompiler.RedeclarationCheck(Name: String; const SkipNamespaces: Boolean=False);
Var Symbol: TSymbol;
Begin
 if (inFunction) Then
  Symbol := getCurrentFunction.findSymbol(Name, Scanner.next(-1)) Else
  Symbol := getCurrentNamespace.findSymbol(Name, Scanner.next(-1));

 if (Symbol <> nil) Then // symbol found?
 Begin
  if (SkipNamespaces) and (Symbol.Typ = stNamespace) Then
   Exit;

  CompileError(eRedeclaration, [Name]);
  With Symbol do
   if (mCompiler <> nil) and (DeclToken <> nil) Then
    TCompiler(mCompiler).CompileError(DeclToken, ePrevDeclared, []); // @TODO: CompileHint/CompileNote?
  Exit;
 End;
End;

(* TCompiler.CompileCode *)
{
 Compiles code.

 Required parameters:
   fInputFile  -> input *.ss file
   fOutputFile -> output compiled file

 Parameters automatically set during the compilation (leave them alone):
   isIncluded        -> when `true`, no output code is saved into any file
   Pass1Only         -> pretty self-explanatory
   fParent           -> parent compiler
   fSupervisor       -> see @Parse_include
   fPreviousInstance -> see @Parse_include and @Parse_FUNCTION
}
Procedure TCompiler.CompileCode(fInputFile, fOutputFile: String; isIncluded: Boolean; Pass1Only: Boolean; fParent: TCompiler; fSupervisor: TCompiler; fPreviousInstance: TCompiler);
Var Compiler2: BCCompiler.TCompiler;

    VBytecode        : String = '';
    UnfinishedComment: Boolean = False;

  { AddPrimaryType }
  Procedure AddPrimaryType(const Typ: TType);
  Begin
   Typ.RefSymbol.isInternal := True;
   NamespaceList[0].SymbolList.Add(TSymbol.Create(stType, Typ));
  End;

  { CheckMain }
  Function CheckMain: Boolean;
  Var Func: TFunction;
  Begin
   if (CompileMode <> cmApp) Then // library or bytecode compile mode do not require the 'main' function to exist.
    Exit(True);

   Func := findFunction('main', findNamespace('self')); // search for `main` function in the `self` namespace

   if (Func = nil) Then // not found!
    Exit(False);

   With Func do
    Result := (Length(ParamList) = 0) and (type_equal(Return, TYPE_INT));
  End;

  { ParseCommandLine }
  Procedure ParseCommandLine;
  Var I  : Integer;
      Str: String;
  Begin
   { parse `-Cm` }
   Str := CmdLine.getStringSwitch(opt__compile_mode, 'app');
   Case Str of
    'app'     : CompileMode := cmApp;
    'lib'     : CompileMode := cmLibrary;
    'bytecode': CompileMode := cmBytecode;

    else
     raise ECommandLineException.CreateFmt('Unknown compile mode: %s', [Str]);
   End;

   { parse `-includepath` }
   IncludePaths               := TStringList.Create;
   IncludePaths.Delimiter     := ';';
   IncludePaths.DelimitedText := CmdLine.getStringSwitch(opt_includepath, '$file;$compiler');

   For I := 0 To IncludePaths.Count-1 Do
   Begin
    Str := IncludePaths[I];

    Str := StringReplace(Str, '$file', ExtractFilePath(InputFile), [rfReplaceAll]);
    Str := StringReplace(Str, '$main', ExtractFilePath(Parent.InputFile), [rfReplaceAll]);
    Str := StringReplace(Str, '$compiler', ExtractFilePath(ParamStr(0)), [rfReplaceAll]);

    IncludePaths[I] := Str;
   End;

   { parse `-bytecode` }
   VBytecode := CmdLine.getStringSwitch(opt_bytecode);
  End;

  { CreateGlobalConstant }
  Procedure CreateGlobalConstant(const cName: String; const cType: TType; const cExpr: PExpressionNode);
  Begin
   With NamespaceList.Last do
   Begin
    SymbolList.Add(TSymbol.Create(stConstant));
    With SymbolList.Last do
    Begin
     Name       := cName;
     isInternal := True;
     Visibility := mvPrivate;

     With mVariable do
     Begin
      Typ   := cType;
      Value := cExpr;

      Include(Attributes, vaConst);
     End;
    End;
   End;
  End;

  { CreateSymbols }
  Procedure CreateSymbols;
  Begin
   { init default namespace }
   NamespaceList := TNamespaceList.Create;
   NamespaceList.Add(TNamespace.Create);

   With NamespaceList.Last do
   Begin
    RefSymbol.Name       := 'self';
    RefSymbol.Visibility := mvPublic;

    SymbolList := TSymbolList.Create;
   End;

   CurrentNamespace := getDefaultNamespace;
   CurrentFunction  := nil;

   NamespaceVisibilityList := TNamespaceVisibilityList.Create;

   { create primary type table }
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
    CreateGlobalConstant('null', TYPE_NULL, MakeIntExpression(0));

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

  { ResetScanner }
  Procedure ResetScanner;
  Begin
   With Scanner do
   Begin
    TokenPos   := 0;
    Visibility := mvPrivate;
   End;
  End;

  { Pass1 }
  Procedure Pass1;
  Begin
   Log('Compilation pass 1');
   ResetScanner;
   CompilePass := _cp1;

   With Scanner do
    While (Can) Do
     ParseToken;
  End;

  { Pass2 }
  Procedure Pass2;
  Begin
   Log('Compilation pass 2');
   ResetScanner;
   CompilePass := _cp2;

   With Scanner do
    While (Can) Do
     ParseToken;
  End;

Begin
 fInputFile  := ReplaceDirSep(fInputFile);
 fOutputFile := ReplaceDirSep(fOutputFile);

 InputFile        := fInputFile;
 OutputFile       := fOutputFile;
 LabelCounter     := 0;
 ModuleName       := '';
 AnyError         := False;
 Parent           := fParent;
 Supervisor       := fSupervisor;
 PreviousInstance := fPreviousInstance;

 PrevRootNodes := TCFGNodeList.Create;

 DoNotStoreOpcodes         := False;
 ParsingFORInitInstruction := False;

 if (isIncluded) Then
  Log('Module: '+InputFile) Else
  Log('Main file: '+InputFile+' => '+OutputFile);

 Scanner := TScanner.Create(self, InputFile, UnfinishedComment);
 ResetScanner;

 if (UnfinishedComment) Then
 Begin
  CompileError(Scanner.getLastToken, eUnfinishedComment, []);
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
    DevLog(dvFatal, 'TCompiler.CompileCode', 'no `parent` specified and compiling as unit!');
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
  Begin
   IncludeList := Parent.IncludeList;
  End;

  SetLength(Scope, 0);

  { When `-Cm=bytecode` is specified: }
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
   End;
  End Else // if included (not the main file)
  Begin
   ModuleName := makeModuleName(fInputFile);
  End;

  { create symbol list }
  CreateSymbols;

  { clear scanner variables }
  Scanner.CurrentDeep := 0;

  { compile code }
  Pass1;

  if (not AnyError) and (not Pass1Only) Then
   Pass2;

  { go next? }
  if (not isIncluded) and (AnyError) Then
  Begin
   Log('-> There were errors compiling this program; stopped.');
   Exit;
  End;

  if (not isIncluded) Then
  Begin
   Log('-> Bytecode generated.');

   if (CmdLine.getBoolSwitch(opt__bytecode_optimize)) Then
   Begin
    Log('-> Optimizing bytecode.');
    With TPeepholeOptimizer.Create(self, nil) Do
    Begin
     Execute;
     Free;
    End;
   End;
  End;

  { check for valid "main" function existence and add prolog code }
  if (CompileMode = cmApp) and (not isIncluded) Then
  Begin
   if (not CheckMain) Then
   Begin
    CompileError(eNoValidMainFunctionFound);
    Exit;
   End;

   // the beginning of an application code have to be "main" function call and "stop" opcode.
   DoNotStoreOpcodes := True;
   OpcodeList.Insert(0, PutOpcode(o_call, [':'+findFunction('main', findNamespace('self')).LabelName]));
   OpcodeList.Insert(1, PutOpcode(o_stop)); // and, if we back from main(), the program ends (virtual machine stops).
   DoNotStoreOpcodes := False;
  End;

  { if specified - save bytecode }
  if (VBytecode <> '') and (not isIncluded) Then
   SaveBytecode(VBytecode);

  { compile bytecode }
  if (not isIncluded) Then
  Begin
   Compiler2 := BCCompiler.TCompiler.Create;
   Compiler2.Compile(self, CompileMode = cmLibrary);
   Compiler2.Free;
  End;
 Finally
 End;
End;

(* TCompiler.CompileError *)
{
 Displays an error; when passed error is a `eInternalError` or belongs to `error_stop`, function also stops the compiler.
}
Procedure TCompiler.CompileError(const Token: TToken_P; const Error: TCompileError; const Args: Array of const);
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
Procedure TCompiler.CompileError(const Token: PToken_P; const Error: TCompileError; const Args: Array of const);
Begin
 if (Token = nil) Then
  CompileError(Error, Args) Else
  CompileError(Token^, Error, Args);
End;

(* TCompiler.CompileError *)
{
 See above
}
Procedure TCompiler.CompileError(const Error: TCompileError; const Args: Array of const);
Begin
 CompileError(Scanner.next(0), Error, Args);
End;

(* TCompiler.CompileError *)
{
 See above
}
Procedure TCompiler.CompileError(const Error: TCompileError);
Begin
 CompileError(Error, []);
End;

(* TCompiler.CompileWarning *)
{
 Displays a warning
}
Procedure TCompiler.CompileWarning(const Token: TToken_P; const Warning: TCompileWarning; const Args: Array of const);
Begin
 Writeln(InputFile+'('+IntToStr(Token.Line)+','+IntToStr(Token.Char)+') Warning: '+Format(CompileWarning_fmt[Warning], Args));
End;

(* TCompiler.CompileWarning *)
{
 See above
}
Procedure TCompiler.CompileWarning(const Token: PToken_P; const Warning: TCompileWarning; const Args: Array of const);
Begin
 if (Token = nil) Then
  CompileWarning(Warning, Args) Else
  CompileWarning(Token^, Warning, Args);
End;

(* TCompiler.CompileWarning *)
{
 See above
}
Procedure TCompiler.CompileWarning(const Warning: TCompileWarning; const Args: Array of const);
Begin
 CompileWarning(Scanner.next(0), Warning, Args);
End;

(* TCompiler.CompileWarning *)
{
 See above
}
Procedure TCompiler.CompileWarning(const Warning: TCompileWarning);
Begin
 CompileWarning(Warning, []);
End;

(* TCompiler.CompileHint *)
{
 Displays a hint
}
Procedure TCompiler.CompileHint(const Token: TToken_P; const Hint: TCompileHint; const Args: Array of const);
Begin
 Writeln(InputFile+'('+IntToStr(Token.Line)+','+IntToStr(Token.Char)+') Hint: '+Format(CompileHint_fmt[Hint], Args));
End;

(* TCompiler.CompileHint *)
{
 See above
}
Procedure TCompiler.CompileHint(const Token: PToken_P; const Hint: TCompileHint; const Args: Array of const);
Begin
 if (Token = nil) Then
  CompileHint(Hint, Args) Else
  CompileHint(Token^, Hint, Args);
End;

(* TCompiler.CompileHint *)
{
 See above
}
Procedure TCompiler.CompileHint(const Hint: TCompileHint; const Args: Array of const);
Begin
 CompileHint(Scanner.next(0), Hint, Args);
End;

(* TCompiler.CompileHint *)
{
 See above
}
Procedure TCompiler.CompileHint(const Hint: TCompileHint);
Begin
 CompileHint(Hint, []);
End;

(* TCompiler.CompileNote *)
{
 Displays a note
}
Procedure TCompiler.CompileNote(const Token: TToken_P; const Note: TCompileNote; const Args: Array of const);
Begin
 Writeln(InputFile+'('+IntToStr(Token.Line)+','+IntToStr(Token.Char)+') Note: '+Format(CompileNote_fmt[Note], Args));
End;

(* TCompiler.CompileNote *)
{
 See above
}
Procedure TCompiler.CompileNote(const Token: PToken_P; const Note: TCompileNote; const Args: Array of const);
Begin
 if (Token = nil) Then
  CompileNote(Note, Args) Else
  CompileNote(Token^, Note, Args);
End;

(* TCompiler.CompileNote *)
{
 See above
}
Procedure TCompiler.CompileNote(const Note: TCompileNote; const Args: Array of const);
Begin
 CompileNote(Scanner.next(0), Note, Args);
End;

(* TCompiler.CompileNote *)
Procedure TCompiler.CompileNote(const Note: TCompileNote);
Begin
 CompileNote(Note, []);
End;
End.
