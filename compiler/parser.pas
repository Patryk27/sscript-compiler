Unit Parser;

 Interface
 Uses Classes, symdef,
      Scanner, Tokens, MTypes;

 { TParser }
 Type TParser = Class
                 Private
                // private fields
                  Compiler: Pointer;

                  DontFailOnEOF: Boolean;

                 Public
                // public fields
                  TokenList: Array of TToken_P; // list of tokens (with stripped comments)
                  TokenPos : Int64; // current token ID (counting from 0)

                  CurrentDeep: Integer; // current brackets' deep (`{` = +1, `}` = -1)
                  Visibility : TVisibility; // current visibility

                  Property getPosition: Int64 read TokenPos; // current token position
                  Property getVisibility: TVisibility read Visibility; // current visibility state


                // public methods
                  Constructor Create(const CompilerPnt: Pointer; InputFile: String; out inLongComment: Boolean);

                  Function getLastToken: TToken_P;
                  Function getCurrentRange(Deep: Integer=1): TRange;

                  Function read: TToken_P;
                  Function read_t: TToken;
                  Function next(const I: Integer=0): TToken_P;
                  Function next_pnt(const I: Integer=0): PToken_P;
                  Function next_t(const I: Integer=0): TToken;
                  Function read_ident: String;
                  Function read_string: String;
                  Function read_int: Integer;
                  Function read_type(const AllowArrays: Boolean=True): TType;
                  Function read_constant_expr(const Sep: TTokenSet=[_SEMICOLON, _COMMA]): PMExpression;
                  Function read_constant_expr_int: Int64;
                  Procedure eat(Token: TToken);
                  Procedure semicolon;

                  Procedure skip_parenthesis;
                  Procedure read_until(const Token: TToken);
                 End;

 Implementation
Uses CompilerUnit, Compile1, ExpressionCompiler, Messages, SysUtils;

(* TParser.Create *)
{
 Loads code from file and preparses it (removes comments etc.)
}
Constructor TParser.Create(const CompilerPnt: Pointer; InputFile: String; out inLongComment: Boolean);
Var Scanner: TScanner; // token scanner
    Code   : TStringList; // TScanner needs a TStringList to parse code

    Token           : TToken_P; // current token
    ShortCommentLine: LongWord=0; // short comment (`//`) line

    I: Integer; // temporary variable
Begin
 Compiler      := CompilerPnt;
 inLongComment := False;

 TokenPos    := 0;
 CurrentDeep := 0;

 { load code from file }
 Code := TStringList.Create;
 Code.LoadFromFile(InputFile); // `InputFile` is already set in the `CompileCode`

 { parse it }
 SetLength(TokenList, 0);

 Scanner := TScanner.Create(Code);
 if (not Scanner.Can) Then // an empty file
 Begin
  SetLength(TokenList, 1);
  TokenList[0].Token := _EOF;
 End;

 While (Scanner.Can) do
 Begin
  if (TokenPos > High(TokenList)) Then // we ran out of the array, so we need to expand it
  Begin
   SetLength(TokenList, Length(TokenList)+100);
   // @TODO: use a generic list instead of an array
   For I := High(TokenList)-99 To High(TokenList) Do
    if (I >= 0) Then
     TokenList[I].Token := noToken;
  End;

  Token := Scanner.getToken_P;

  if (Token.Token = noToken) Then // skip `noToken`-s
   Continue;

  if (Token.Token = _EOF) Then
  Begin
   DevLog('Info: reached `EOF` - finishing code parsing.');
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
      TokenList[TokenPos] := Token;
      Inc(TokenPos);
     End;
    End;
  End;
 End;

 TokenPos      := 0;
 CurrentDeep   := 0;
 DontFailOnEOF := False;

 { destroy objects }
 Scanner.Free;
 Code.Free;
End;

(* TParser.getLastToken *)
{
 Returns last non-`noToken` token
}
Function TParser.getLastToken: TToken_P;
Var I: LongWord;
Begin
 For I := High(TokenList) Downto Low(TokenList) Do
  if (TokenList[I].Token <> noToken) Then
   Exit(TokenList[I]);
End;

(* TParser.getCurrentRange *)
{
 Returns current scope's range.
}
Function TParser.getCurrentRange(Deep: Integer=1): TRange;
Var TPos: Int64;
Begin
 Try
  DontFailOnEOF := True; // don't fail on case when brackets are unclosed (it would fail with error `unexpected eof`), as this error will be detected and raised later (eg.when parsing a construction)

  TPos          := TokenPos;
  Result.PBegin := TokenPos;

  With TCompiler(Compiler) do
   if (ParsingFORInitInstruction) Then
   Begin
    read_until(_BRACKET1_CL);
    Deep := 0;

    if (next_t <> _BRACKET3_OP) Then
    Begin
     if (next_t in [_FOR, _WHILE]) Then
     Begin
      read;
      read;
      read_until(_BRACKET1_CL);
     End;

     read_until(_SEMICOLON);
     if (next_t = _ELSE) Then
     Begin
      read;
      read_until(_SEMICOLON);
     End;

     Result.PEnd := TokenPos;
     TokenPos    := TPos;
     Exit;
    End;
   End;

  Result.PEnd := TokenPos;

  While (true) Do
  Begin
   if (Result.PEnd >= High(TokenList)) Then // ending `}` not found
   Begin
    DevLog('Syntax error: ending `}` not found');
    Exit;
   End;

   Case TokenList[Result.PEnd].Token of
    _BRACKET3_OP: Inc(Deep);
    _BRACKET3_CL: Dec(Deep);
   End;

   Inc(Result.PEnd);

   if (Deep = 0) Then
    Break;
  End;

  TokenPos := TPos;
 Finally
  DontFailOnEOF := False;
 End;
End;

(* TParser.read *)
{
 Reads a token
}
Function TParser.read: TToken_P;
Begin
 if (TokenPos > High(TokenList)) Then
  TCompiler(Compiler).CompileError(eEOF);

 Result := TokenList[TokenPos];
 Inc(TokenPos);

 With TCompiler(Compiler) do
  Case Result.Token of
   _INVALID_INT: CompileError(Result, eInvalidIntegerValue, [Result.Value]);
   _INVALID_FLOAT: CompileError(Result, eInvalidFloatValue, [Result.Value]);
   _INVALID_STRING: CompileError(Result, eStringExceedsLine, []);
  End;
End;

(* TParser.read_t *)
{
 Reads a token's kind; see @TParser.read
}
Function TParser.read_t: TToken;
Begin
 Result := read.Token;
End;

(* TParser.next *)
{
 Returns a next - or previous (when `I` is negative) - token.
}
Function TParser.next(const I: Integer=0): TToken_P;
Begin
 Result := TokenList[TokenPos+I];
End;

(* TParser.next_pnt *)
{
 Returns a next - or previous (when `I` is negative) - token's pointer.
}
Function TParser.next_pnt(const I: Integer=0): PToken_P;
Begin
 Result := @TokenList[TokenPos+I];
End;

(* TParser.next_t *)
{
 Works just as TParser.next, but gets only a token's kind.
}
Function TParser.next_t(const I: Integer=0): TToken;
Begin
 Result := next(I).Token;
End;

(* TParser.read_ident *)
{
 Reads an identifier; displays error `eExpectedIdentifier` when read token isn't an identifier.
}
Function TParser.read_ident: String;
Begin
 if (next_t <> _IDENTIFIER) Then
  TCompiler(Compiler).CompileError(next, eExpectedIdentifier, [next.Value]);
 Result := read.Value;
End;

(* TParser.read_string *)
{
 Reads a string; displays error `eExpectedString` when read token isn't a string.
}
Function TParser.read_string: String;
Begin
 if (next_t <> _STRING) Then
  TCompiler(Compiler).CompileError(next, eExpectedString, [next.Value]);
 Result := read.Value;
End;

(* TParser.read_int *)
{
 Reads an integer value; displays error `eExpectedInt` when read token isn't a string.
}
Function TParser.read_int: Integer;
Begin
 if (next_t <> _INT) Then
  TCompiler(Compiler).CompileError(next, eExpectedInt, [next.Value]);
 Result := StrToInt(read.Value);
End;

(* TParser.read_type *)
{
 Reads a type name or a full type (based on current token) and returns its ID.
}
Function TParser.read_type(const AllowArrays: Boolean=True): TType;
Var Base, Typ: TType;

    Token: TToken_P;

    FuncReturn: TType;
    FuncParams: TParamList;

    isArray, isStringBased, isFunction: Boolean;

    NamespaceName: String;
    NamespaceID  : Integer;
    TypeID       : Integer;
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
     NamespaceID   := findNamespace(NamespaceName);

     if (NamespaceID = -1) Then // namespace not found
     Begin
      CompileError(next(-2), eUnknownNamespace, [NamespaceName]);
      read_ident;
      Exit;
     End;

     Token := next;
     Base  := findGlobalType(read_ident, NamespaceID);
    End Else // `type name`
    Begin
     if (inFunction) Then
      TypeID := findLocalType(Token.Value) Else
      TypeID := -1;

     if (TypeID = -1) Then // not a local type
     Begin
      findTypeCandidate(Token.Value, SelectedNamespaces, TypeID, NamespaceID);

      if (TypeID = -1) Then // type not found
      Begin
       CompileError(next(-1), eUnknownType, [Token.Value]);
       Exit;
      End;

      Base := NamespaceList[NamespaceID].SymbolList[TypeID].mType;
     End Else // local type
      Base := getCurrentFunction.SymbolList[TypeID].mType;
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
     FuncParams[High(FuncParams)].Typ := read_type();

     if (FuncParams[High(FuncParams)].Typ.isVoid) Then
      CompileError(eVoidNoNameParam);

     if (next_t = _BRACKET1_CL) Then
      Break;

     eat(_COMMA);
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

  isArray       := Typ.ArrayDimCount > 0;
  isStringBased := type_equal(Typ, TYPE_STRING); // @TODO: memleak (as `TYPE_STRING` creates a new instance of `string` type)

  if (isArray) Then
  Begin
   Typ.RegPrefix := 'r';
   Typ.ArrayBase := Base;

   With Typ do
    While (ArrayBase.isArray(False)) Do // :>
     ArrayBase := ArrayBase.ArrayBase;

   if (isStringBased) Then
    Typ.RegPrefix := 's';
  End;

  { set result }
  Result := Typ;
 End;
End;

(* TParser.read_constant_expr *)
{
 Reads and evaluates a constant expression.
}
Function TParser.read_constant_expr(const Sep: TTokenSet=[_SEMICOLON, _COMMA]): PMExpression;
Begin
 Result := PMExpression(ExpressionCompiler.MakeConstruction(Compiler, Sep, [oInsertConstants, oConstantFolding, oDisplayParseErrors]).Values[0]);
End;

(* TParser.read_constant_expr_int *)
Function TParser.read_constant_expr_int: Int64;
Var Expr: PMExpression;
Begin
 Expr := read_constant_expr;

 if (Expr^.Typ <> mtInt) Then
  TCompiler(Compiler).CompileError(eWrongType, [getExpressionTypeName(Expr), 'int']);

 if (Expr^.Value = null) Then
 Begin
  DevLog('Error: TParser.read_constant_expr_int() -> Expr^.Value = null; returned `0`');
  Exit(0);
 End;

 Exit(Expr^.Value);
End;

(* TParser.eat *)
{
 'eats' a specified token.
 (ie. if current token isn't token passed in the parameter, displays a syntax error).
}
Procedure TParser.eat(Token: TToken);
Begin
 if (read_t <> Token) Then
  TCompiler(Compiler).CompileError(eExpected, [getTokenDisplay(Token), next(-1).Value]);
End;

(* TParser.semicolon *)
{
 Eats a semicolon (`_SEMICOLON` token)
}
Procedure TParser.semicolon;
Begin
 eat(_SEMICOLON);
End;

(* TParser.skip_parenthesis *)
{
 Skips parenthesises
}
Procedure TParser.skip_parenthesis;
Var Deep: Integer = 0;
Begin
 Repeat
  if ((TokenPos >= High(TokenList)) and (DontFailOnEOF)) Then
   Exit;

  Case read_t of
   _BRACKET1_OP, _BRACKET2_OP, _BRACKET3_OP, _LOWER: Inc(Deep);
   _BRACKET1_CL, _BRACKET2_CL, _BRACKET3_CL, _GREATER: Dec(Deep);
  End;
 Until (Deep = 0);
End;

(* TParser.read_until *)
Procedure TParser.read_until(const Token: TToken);
Var Deep: Integer = 0;
    Tok : TToken;
Begin
 While (true) do
 Begin
  if ((TokenPos >= High(TokenList)) and (DontFailOnEOF)) Then
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
End.
