Unit Parser;

 Interface
 Uses Classes,
      Scanner, Tokens, MTypes, symdef;

 { TParser }
 Type TParser = Class
                 Private
               // private fields
                  Compiler: Pointer;

                 Public
                // public fields
                  TokenList: Array of TToken_P; // list of tokens (with stripped comments)
                  TokenPos : LongWord; // current token ID (counting from 0)

                  CurrentDeep: Integer; // current brackets' deep (`{` = +1, `}` = -1)
                  Visibility : TVisibility; // current visibility

                  Property getPosition: LongWord read TokenPos; // current token position
                  Property getVisibility: TVisibility read Visibility; // current visibility state

                // public methods
                  Constructor Create(const CompilerPnt: Pointer; InputFile: String);

                  Function read: TToken_P;
                  Function read_t: TToken;
                  Function next(const I: Integer=0): TToken_P;
                  Function next_pnt(const I: Integer=0): PToken_P;
                  Function next_t(const I: Integer=0): TToken;
                  Function read_ident: String;
                  Function read_string: String;
                  Function read_type(const AllowArrays: Boolean=True): TType;
                  Procedure eat(Token: TToken);
                  Procedure semicolon;
                  Function getToken(const I: Integer=0): TToken_P;
                 End;

 Implementation
Uses Compile1, Messages;

(* TParser.Create *)
{
 Loads code from file and preparses it (removes comments etc.)
}
Constructor TParser.Create(const CompilerPnt: Pointer; InputFile: String);
Var Scanner: TScanner; // token scanner
    Code   : TStringList; // TScanner needs a TStringList to parse code

    Token           : TToken_P; // current token
    isLongComment   : Boolean=False; // are we in long comment: `/* ... */` (we need to know it, because all tokens in comments are skipped)?
    ShortCommentLine: LongWord=0; // are we in short comment: `//` (it holds this type comment's line)

    I: Integer; // temporary variable
Begin
 Compiler := CompilerPnt;

 CurrentDeep := 0;

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

(* TParser.read *)
{
 Reads a token; skips any `noTokens` and shows an error on unfinished strings.
}
Function TParser.read: TToken_P;
Begin
 if (TokenPos > High(TokenList)) Then
  TCompiler(Compiler).CompileError(eEOF);

 Result := TokenList[TokenPos];
 Inc(TokenPos);

 if (Result.Token = noToken) Then // skip `noToken`-s
  Result := read;

 if (Result.Token = _INVALID_STRING) Then
  TCompiler(Compiler).CompileError(eStringExceedsLine, []);
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
 Returns a next - or previous (when `I` is negative) - pointer to a token.
}
Function TParser.next_pnt(const I: Integer=0): PToken_P;
Begin
 Result := @TokenList[TokenPos+I];
End;

(* TParser.next_t *)
{
 Works just as TParser.next, but gets only a token kind.
}
Function TParser.next_t(const I: Integer=0): TToken;
Begin
 Result := next(I).Token;
End;

(* TParser.read_ident *)
{
 Reads an identifier; displays error `eExpectedIdentifier` when current token isn't an identifier.
}
Function TParser.read_ident: String;
Begin
 if (next_t <> _IDENTIFIER) Then
  TCompiler(Compiler).CompileError(next, eExpectedIdentifier, [next.Display]);
 Result := read.Display;
End;

(* TParser.read_string *)
{
 Reads a string; displays error `eExpectedString` when current token isn't a string.
}
Function TParser.read_string: String;
Begin
 if (next_t <> _STRING) Then
  TCompiler(Compiler).CompileError(next, eExpectedString, [next.Display]);
 Result := read.Display;
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
   CompileError(next, eExpectedType, [Token.Display]);
   Exit;
  End;

  Typ           := Base.Clone;
  isStringBased := type_equal(Typ, TYPE_STRING);

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

  isArray := Typ.ArrayDimCount > 0;

  if (isArray) Then
  Begin
   Typ.RegPrefix := 'r';
   Typ.ArrayBase := Base;

   if (isStringBased) Then
    Typ.RegPrefix := 's';
   // Typ.ArrayBase := TYPE_STRING;
  End;

  { set result }
  Result := Typ;
 End;
End;

(* TParser.eat *)
{
 'eats' a specified token.
 (i.e. if current token isn't token passed in the parameter, displays a syntax error).
}
Procedure TParser.eat(Token: TToken);
Begin
 if (read_t <> Token) Then
  TCompiler(Compiler).CompileError(eExpected, [getTokenDisplay(Token), next(-1).Display]);
End;

(* TParser.semicolon *)
{
 Eats a semicolon (`_SEMICOLON` token)
}
Procedure TParser.semicolon;
Begin
 eat(_SEMICOLON);
End;

(* TParser.getToken *)
{
 Works the same as TParser.next
}
Function TParser.getToken(const I: Integer=0): TToken_P;
Begin
 Result := TokenList[TokenPos+I];
End;
End.
