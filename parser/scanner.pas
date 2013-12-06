(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Scanner;

 Interface
 Uses Classes, FGL,
      symdef, Lexer, Tokens, Expression;

 Const DefaultSeparators = [_SEMICOLON, _COMMA, _BRACKET1_CL, _BRACKET2_CL, _BRACKET3_CL];

 Type TTokenList = specialize TFPGList<PToken_P>;

 { TScanner }
 Type TScanner =
      Class
       Private { fields }
        Compiler : TObject;
        TokenList: TTokenList; // list of tokens (with stripped comments)

        DontFailOnEOF: Boolean;

       Public { fields }
        TokenPos: Int64; // current token ID (counting from 0)

        CurrentDeep: Integer; // current brackets' deep (`{` = +1, `}` = -1)
        Visibility : TVisibility; // current visibility

        Property getPosition: Int64 read TokenPos; // current token position
        Property getVisibility: TVisibility read Visibility; // current visibility state

       Public { methods }
        Constructor Create(const CompilerObject: TObject; InputFile: String; out inLongComment: Boolean);
        Destructor Destroy; override;

        Function getToken(const Index: uint32): TToken_P;
        Function getTokenPnt(const Index: uint32): PToken_P;
        Function getLastToken: TToken_P;
        Function getCurrentRange(Deep: int16=1): TRange;

        Function read: TToken_P;
        Function read_t: TToken;
        Function next(const I: Integer=0): TToken_P;
        Function next_pnt(const I: Integer=0): PToken_P;
        Function next_t(const I: Integer=0): TToken;
        Function read_ident: String;
        Function read_string: String;
        Function read_int: Integer;
        Function read_type(const AllowArrays: Boolean=True): TType;
        Function read_constant_expr(const Sep: TTokenSet=DefaultSeparators): PExpressionNode;
        Function read_constant_expr_int(const Sep: TTokenSet=DefaultSeparators): Int64;
        Procedure eat(Token: TToken);
        Procedure semicolon;

        Procedure skip_parenthesis;
        Procedure read_until(const Token: TToken);

        Function Can: Boolean;
       End;

 Implementation
Uses CompilerUnit, SSCompiler, ExpressionCompiler, Messages, SysUtils;

(* TScanner.Create *)
{
 Loads code from file and preparses it (removes comments etc.)
}
Constructor TScanner.Create(const CompilerObject: TObject; InputFile: String; out inLongComment: Boolean);
Var Lexer: TLexer; // lexer
    Code : TStringList; // TScanner needs a TStringList to parse code

    Token           : TToken_P; // current token
    PToken          : PToken_P;
    ShortCommentLine: LongWord=0; // short comment (`//`) line
Begin
 Compiler      := CompilerObject as TCompiler;
 inLongComment := False;

 if (Compiler = nil) Then
  raise Exception.Create('TScanner.Create() -> expected a TCompiler instance as first parameter!');

 TokenPos    := 0;
 CurrentDeep := 0;

 { load code from file }
 Code := TStringList.Create;
 Code.LoadFromFile(InputFile); // `InputFile` is already set in the `CompileCode`

 { parse it }
 TokenList := TTokenList.Create;

 Lexer := TLexer.Create(Code);
 if (not Lexer.Can) Then // an empty file
 Begin
  New(PToken);
  With PToken^ do
  Begin
   Char      := 1;
   Line      := 1;
   Position  := 0;
   Value     := 'end-of-file';
   Token     := _EOF;
   TokenName := 'EOF';
  End;

  TokenList.Add(PToken);
  Exit;
 End;

 While (Lexer.Can) do
 Begin
  Token := Lexer.getToken_P;

  if (Token.Token = noToken) Then // skip `noToken`-s
   Continue;

  if (Token.Token = _EOF) Then
  Begin
   DevLog(dvInfo, 'TScanner.Create', 'reached `EOF` - finishing code parsing...');
   Break;
  End;

  Case Token.Token of
   _DOUBLE_SLASH { // }:
    if (not inLongComment) Then
     ShortCommentLine := Token.Line;

   else
    if (Token.Line <> ShortCommentLine) Then // not in short (one-line) comment
    Begin
     if (Token.Token = _LONGCMT_OPEN { /* }) Then
      inLongComment := True Else
     if (Token.Token = _LONGCMT_CLOSE { */ }) Then
      inLongComment := False Else

     if (not inLongComment) Then
     Begin
      New(PToken);
      PToken^          := Token;
      PToken^.Position := TokenList.Count;
      TokenList.Add(PToken);
     End;
    End;
  End;
 End;

 TokenPos      := 0;
 CurrentDeep   := 0;
 DontFailOnEOF := False;

 { destroy objects }
 Lexer.Free;
 Code.Free;
End;

(* TScanner.Destroy *)
Destructor TScanner.Destroy;
Var Token: PToken_P;
Begin
 For Token in TokenList Do
  Dispose(Token);
 TokenList.Free;
End;

(* TScanner.getToken *)
{
 Returns a token with specified index.
}
Function TScanner.getToken(const Index: uint32): TToken_P;
Begin
 Result := TokenList[Index]^;
End;

(* TScanner.getTokenPnt *)
{
 Returns a pointer to token with specified index.
}
Function TScanner.getTokenPnt(const Index: uint32): PToken_P;
Begin
 Result := TokenList[Index];
End;

(* TScanner.getLastToken *)
{
 Returns last non-`noToken` token
}
Function TScanner.getLastToken: TToken_P;
Begin
 Exit(TokenList.Last^);
End;

(* TScanner.getCurrentRange *)
{
 Returns current scope's range.
}
Function TScanner.getCurrentRange(Deep: int16=1): TRange;

  { SkipBlock }
  Procedure SkipBlock;
  Var Deep: int16 = 0;
  Begin
   if (next_t = _BRACKET3_OP) Then
   Begin
    While (true) Do
    Begin
     Case read.Token of
      _BRACKET3_OP:
       Inc(Deep);

      _BRACKET3_CL:
       Dec(Deep);
     End;

     if (Deep = 0) Then
      Break;
    End;
   End Else
   Begin
    read_until(_SEMICOLON);
   End;
  End;

Var TPos: Int64;
Label Parse_IF;
Begin
 Try
  DontFailOnEOF := True; // don't fail when brackets are unclosed (it would fail with error `unexpected eof`) beacuse this error will be detected and raised later (when parsing the actual construction)

  TPos          := TokenPos;
  Result.PBegin := TokenList[TokenPos]^;

  With TCompiler(Compiler) do
  Begin
   if (ParsingFORInitInstruction or ParsingForeachHeader) Then // super special cases: parsing a "for" init instruction or parsing a "foreach" header
   Begin
    read_until(_BRACKET1_CL);

    Case next_t of
     (* { *)
     _BRACKET3_OP:
     Begin
      SkipBlock;
     End;

     (* if *)
     _IF:
     Begin
     Parse_IF:
      eat(_IF);
      eat(_BRACKET1_OP);
      read_until(_BRACKET1_CL);

      SkipBlock;

      if (next_t = _ELSE) Then
      Begin
       eat(_ELSE);
       SkipBlock;

       if (next_t = _IF) Then
        goto Parse_IF;
      End;
     End;

     else
      read_until(_SEMICOLON);
    End;

    While (TokenPos >= TokenList.Count) Do
     Dec(TokenPos);

    Result.PEnd := TokenList[TokenPos]^;
    TokenPos    := TPos;
    Exit;
   End;
  End;

  While (true) Do
  Begin
   if (TokenPos >= TokenList.Count) Then // ending `}` not found, so symbol must have global reachability
   Begin
    Dec(TokenPos);
    Break;
   End;

   Case TokenList[TokenPos]^.Token of
    _BRACKET3_OP: Inc(Deep);
    _BRACKET3_CL: Dec(Deep);
   End;

   Inc(TokenPos);

   if (Deep = 0) Then
    Break;
  End;

  While (TokenPos >= TokenList.Count) Do
   Dec(TokenPos);

  Result.PEnd := TokenList[TokenPos]^;

  TokenPos := TPos;
 Finally
  DontFailOnEOF := False;
 End;
End;

(* TScanner.read *)
{
 Reads a token
}
Function TScanner.read: TToken_P;
Begin
 if (TokenPos >= TokenList.Count) Then
  TCompiler(Compiler).CompileError(eEOF);

 Result := TokenList[TokenPos]^;
 Inc(TokenPos);

 With TCompiler(Compiler) do
  Case Result.Token of
   _INVALID_INT: CompileError(Result, eInvalidIntegerValue, [Result.Value]);
   _INVALID_FLOAT: CompileError(Result, eInvalidFloatValue, [Result.Value]);
   _INVALID_STRING: CompileError(Result, eStringExceedsLine, []);
  End;
End;

(* TScanner.read_t *)
{
 Reads a token's kind; see @TScanner.read
}
Function TScanner.read_t: TToken;
Begin
 Result := read.Token;
End;

(* TScanner.next *)
{
 Returns a next - or previous (when `I` is negative) - token.
}
Function TScanner.next(const I: Integer=0): TToken_P;
Begin
 Result := next_pnt(I)^;
End;

(* TScanner.next_pnt *)
{
 Returns a next - or previous (when `I` is negative) - token's pointer.
}
Function TScanner.next_pnt(const I: Integer=0): PToken_P;
Begin
 if (TokenPos+I >= TokenList.Count) Then
  Result := TokenList.Last Else
  Result := TokenList[TokenPos+I];
End;

(* TScanner.next_t *)
{
 Works just as TScanner.next, but gets only a token's kind.
}
Function TScanner.next_t(const I: Integer=0): TToken;
Begin
 Result := next(I).Token;
End;

(* TScanner.read_ident *)
{
 Reads an identifier; displays error `eExpectedIdentifier` when read token isn't an identifier.
}
Function TScanner.read_ident: String;
Begin
 if (next_t <> _IDENTIFIER) Then
  TCompiler(Compiler).CompileError(next, eExpectedIdentifier, [next.Value]);
 Result := read.Value;
End;

(* TScanner.read_string *)
{
 Reads a string; displays error `eExpectedString` when read token isn't a string.
}
Function TScanner.read_string: String;
Begin
 if (next_t <> _STRING) Then
  TCompiler(Compiler).CompileError(next, eExpectedString, [next.Value]);
 Result := read.Value;
End;

(* TScanner.read_int *)
{
 Reads an integer value; displays error `eExpectedInt` when read token isn't a string.
}
Function TScanner.read_int: Integer;
Begin
 if (next_t <> _INT) Then
  TCompiler(Compiler).CompileError(next, eExpectedInt, [next.Value]);
 Result := StrToInt(read.Value);
End;

(* TScanner.read_type *)
{
 Reads a type name or a full type (based on current token) and returns its ID.
}
Function TScanner.read_type(const AllowArrays: Boolean=True): TType;
Var Base, Typ, TmpType: TType;

    Token: TToken_P;

    I         : Integer;
    FuncReturn: TType;
    FuncParams: TParamList;
    FuncParam : PParam;

    isArray, isStringBased, isFunction: Boolean;
    RequireDefaultValue               : Boolean = False;

    NamespaceName: String;
    Namespace    : TNamespace;
Begin
 With TCompiler(Compiler) do
 Begin
  Result := nil;
  Base   := nil;
  Token  := read;

  isArray       := False;
  isStringBased := False;
  isFunction    := False;

  Typ := TType.Create;

  { read current token }
  Case Token.Token of
   _IDENTIFIER:
   Begin
    if (next_t = _DOUBLE_COLON) Then // `namespace name::type name`
    Begin
     eat(_DOUBLE_COLON);

     NamespaceName := Token.Value;
     Namespace     := findNamespace(NamespaceName);

     if (Namespace = nil) Then // namespace not found
     Begin
      CompileError(next(-2), eUnknownNamespace, [NamespaceName]);
      read_ident;
      Exit;
     End;

     Token := next;
     Base  := findTypeCandidate(read_ident, Namespace, Token);
    End Else // `type name`
    Begin
     Base := findTypeCandidate(Token.Value, getCurrentNamespace, Token);

     if (Base = nil) Then // type not found
     Begin
      CompileError(next(-1), eUnknownType, [Token.Value]);
      Exit;
     End;
    End;
   End;

   { function-type declaration }
   _FUNCTION:
    isFunction := True;

   else
    CompileError(next, eExpectedIdentifier, [Token.Value]);
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
     FuncParam := @FuncParams[High(FuncParams)];

     if (Token.Token = _CONST) Then // const-param
     Begin
      Token := read;

      FuncParam^.Attributes += [vaConst];
      FuncParam^.isConst := True;
     End Else

     if (Token.Token = _VAR) Then // var-param
     Begin
      Token            := read;
      FuncParam^.isVar := True;
     End;

     FuncParam^.Typ := read_type(); // [param type]

     if (FuncParam^.Typ.isVoid) Then // error: void-typed param
      CompileError(eVoidNoNameParam);

     if (next_t = _IDENTIFIER) Then // optional identifier indicating parameter's name
     Begin
      FuncParam^.Name := read_ident;

      For I := 0 To High(FuncParams)-1 Do
       if (FuncParams[I].Name = FuncParam^.Name) Then // redeclaration
        CompileError(eRedeclaration, [FuncParam^.Name]);
     End;

     if (next_t = _EQUAL) Then // optional default parameter's value
     Begin
      eat(_EQUAL);
      FuncParam^.DefaultValue := read_constant_expr;
      TmpType                 := getTypeFromExpression(FuncParam^.DefaultValue);
      Dec(TokenPos);

      if (not TmpType.CanBeAssignedTo(FuncParam^.Typ)) Then
       CompileError(eWrongType, [TmpType.asString, FuncParam^.Typ.asString]);

      RequireDefaultValue := True;
     End Else
      if (RequireDefaultValue) Then
       CompileError(next, eDefaultParamValueRequired, [FuncParam^.Name]) Else
       FuncParam^.DefaultValue := nil;

     if (next_t = _BRACKET1_CL) Then // end of parameter list?
      Break;

     eat(_COMMA); // 'eat' comma (parameter list separator)
    End;
    eat(_BRACKET1_CL);

    Typ.RegPrefix  := 'r';
    Typ.InternalID := TYPE_INT_id;
    Typ.FuncReturn := FuncReturn;
    Typ.FuncParams := FuncParams;

    Include(Typ.Attributes, taFunction);
   End Else
   Begin
   { unspecialized function }
    Typ.RegPrefix  := 'r';
    Typ.InternalID := TYPE_INT_id;
    Typ.FuncReturn := TYPE_ANY;

    Typ.Attributes += [taFunction, taUnspecialized];
   End;

   if ((next_t = _BRACKET2_OP) and (AllowArrays)) Then // is it an array declaration?
   Begin
    Base := Typ.Clone;
   End Else
    Exit(Typ);
  End;

  { check for primary type existence }
  if (Base = nil) Then
  Begin
   CompileError(next, eUnknownType, [Token.Value]);
   Exit;
  End;

  Typ := Base.Clone;

  if (next_t = _BRACKET2_OP) Then
  Begin
   if (Base.isVoid) Then // `void` array cannot be created (it would destroy our universe)...
   Begin
    CompileError(next, eVoidArray, []);
    Exit;
   End;

   if (Base.InternalID = TYPE_ANY_id) Then // ... as well, as `any`-typed array
   Begin
    CompileError(next, eInternalError, ['Cannot create an ''any''-typed array!']);
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

  isArray       := Typ.ArrayDimCount > 0;
  isStringBased := type_equal(Typ, TYPE_STRING); // @TODO: memleak (as `TYPE_STRING` creates a new instance of `string` type)

  if (isArray) Then
  Begin
   Typ.RegPrefix := 'r';
   Typ.ArrayBase := Base;

   if (isStringBased) Then
   Begin
    Typ.RegPrefix := 's';
    Typ.ArrayBase := Typ.ArrayBase.ArrayBase;
   End;
  End;

  { set result }
  Result := Typ;
 End;
End;

(* TScanner.read_constant_expr *)
{
 Reads and evaluates a constant expression.
}
Function TScanner.read_constant_expr(const Sep: TTokenSet=DefaultSeparators): PExpressionNode;
Begin
 Result := MakeExpression(Compiler, Sep, []);
 OptimizeExpression(TCompiler(Compiler), Result, [oInsertConstants, oConstantFolding, oDisplayParseErrors]);
End;

(* TScanner.read_constant_expr_int *)
Function TScanner.read_constant_expr_int(const Sep: TTokenSet=DefaultSeparators): Int64;
Var Expr: PExpressionNode;
Begin
 Expr := read_constant_expr(Sep);

 if (Expr^.Typ <> mtInt) Then
  TCompiler(Compiler).CompileError(eWrongType, [getExpressionTypeName(Expr), 'int']);

 if (Expr^.Value = null) Then
 Begin
  DevLog(dvError, 'TScanner.read_constant_expr_int', 'Error: TScanner.read_constant_expr_int() -> Expr^.Value = null; returned `0`');
  Exit(0);
 End;

 Exit(Expr^.Value);
End;

(* TScanner.eat *)
{
 'eats' a specified token.
 (ie. if current token isn't token passed in the parameter, displays a syntax error).
}
Procedure TScanner.eat(Token: TToken);
Begin
 if (read_t <> Token) Then
  TCompiler(Compiler).CompileError(eExpected, [getTokenDisplay(Token), next(-1).Value]);
End;

(* TScanner.semicolon *)
{
 Eats a semicolon (`_SEMICOLON` token)
}
Procedure TScanner.semicolon;
Begin
 eat(_SEMICOLON);
End;

(* TScanner.skip_parenthesis *)
{
 Skips parenthesises
}
Procedure TScanner.skip_parenthesis;
Var Deep: Integer = 0;
Begin
 Repeat
  if ((TokenPos >= TokenList.Count) and (DontFailOnEOF)) Then
   Exit;

  Case read_t of
   _BRACKET1_OP, _BRACKET2_OP, _BRACKET3_OP, _LOWER: Inc(Deep);
   _BRACKET1_CL, _BRACKET2_CL, _BRACKET3_CL, _GREATER: Dec(Deep);
  End;
 Until (Deep = 0);
End;

(* TScanner.read_until *)
Procedure TScanner.read_until(const Token: TToken);
Var Deep: Integer = 0;
    Tok : TToken;
Begin
 While (true) do
 Begin
  if ((TokenPos >= TokenList.Count) and (DontFailOnEOF)) Then
   Exit;

  Tok := read_t;

  if (Tok = Token) and (Deep = 0) Then
   Break;

  Case Tok of
   _BRACKET1_OP, _BRACKET2_OP, _BRACKET3_OP: Inc(Deep);
   _BRACKET1_CL, _BRACKET2_CL, _BRACKET3_CL: Dec(Deep);
  End;
 End;
End;

(* TScanner.Can *)
{
 Returns 'true', if at least one token can be read.
}
Function TScanner.Can: Boolean;
Begin
 Result := (TokenPos < TokenList.Count);
End;
End.
