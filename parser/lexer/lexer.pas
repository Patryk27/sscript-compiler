(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Lexer;

 Interface
 Uses Tokens, SysUtils, Classes;

 { TLexer }
 Type TLexer =
      Class
       Private
        FileName: String;

        Input   : String;
        Position: uint32;

       Private
        Function __readChar(out NewLine, Escaped: Boolean; const AllowEscaping: Boolean=False): Char;
        Function __readCharN(out NewLine: Boolean; const AllowEscaping: Boolean=False): Char;
        Function __readString(out OK: Boolean; const Ch: Char): String;
        Function __readIdentifier: String;
        Function __readNumber(out OK, isFloat: Boolean; out Str: String): Extended;
        Function __readHexNumber(out OK: Boolean; out Str: String): Int64;

        Function getToken: TToken;

       Public
        Constructor Create(const fFileName: String; const Lines: TStringList);

        Function getToken_P: TToken_P;
        Function Can: Boolean;
       End;

 Implementation
Uses Variants;
Const NewlineChar = #13; // this can be pretty any char, except that printable ones - it doesn't depend on any system settings

(* TLexer.__readChar *)
{
 Reads a single char which may be - when 'AllowEscaping = true' - escaped, eg.: `\0xA` or `\\`
}
Function TLexer.__readChar(out NewLine, Escaped: Boolean; const AllowEscaping: Boolean=False): Char;
Var Ch : Char;
    Tmp: String;
    OK : Boolean;
Begin
 NewLine := False;
 Escaped := False;

 Result := Input[Position];
 Inc(Position);

 if (Result = NewlineChar) Then // if encountered the newline character
 Begin
  NewLine := True;
  Exit;
 End;

 if (Result = '\') and (AllowEscaping) Then // read an escaped char, if allowed
 Begin
  Ch      := __readCharN(NewLine);
  Escaped := True;

  Case Ch of // possible char escapes: \n, \r, \t, \v, \f
   'n': Exit(#$0A);
   'r': Exit(#$0D);
   't': Exit(#$09);
   'v': Exit(#$0B);
   'f': Exit(#$0C);
  End;

  if (Ch in ['0'..'9']) Then // read an escaped decimal number-char, like: \40
  Begin
   Tmp := Ch;

   Ch := __readCharN(NewLine);
   if (NewLine) Then
    Exit(#0);

   if (Ch = 'x') Then // hexadecimal number: \0xnumber
   Begin
    Exit(chr(__readHexNumber(OK, Tmp)));
   End Else

   Begin // decimal number: \number
//    Dec(Position); ?

    if (Ch in ['0'..'9']) Then
     Tmp += Ch Else
     Dec(Position);

    While (true) Do
    Begin
     Ch := __readCharN(NewLine);

     if (NewLine) Then
      Break;

     if (Ch in ['0'..'9']) Then
      Tmp += Ch Else
      Break;
    End;

    Dec(Position);
    Exit(chr(StrToInt(Trim(Tmp))));
   End;
  End Else
  Begin
   Exit(Ch);
  End;
 End;
End;

(* TLexer.__readCharN *)
{
 Works like `__readChar`, but do not require the "Escaped" parameter (variable) to be given.
}
Function TLexer.__readCharN(out NewLine: Boolean; const AllowEscaping: Boolean=False): Char;
Var Escaped: Boolean;
Begin
 Result := __readChar(NewLine, Escaped, AllowEscaping);
End;

(* TLexer.__readString *)
{
 Reads a string literal.
 'Ch' should be `"` or `'`, depending on literal to be read.
}
Function TLexer.__readString(out OK: Boolean; const Ch: Char): String;
Var C          : Char;
    NL, Escaped: Boolean;
Begin
 Result := '';
 OK     := True;

 While (true) Do
 Begin
  C := __readChar(NL, Escaped, True);

  if (NL) or ((C = Ch) and (not Escaped)) Then
   Break;

  Result += C;
 End;

 if (NL) Then // string hasn't been finished (terminator `"` not found)
  OK := False;
End;

(* TLexer.__readIdentifier *)
{
 Reads an identifier.
}
Function TLexer.__readIdentifier: String;
Var Ch     : Char;
    Newline: Boolean;
Begin
 Dec(Position);
 Result := '';

 While (true) Do
 Begin
  Ch := __readCharN(Newline);

  if (not (Ch in IdentAllowed)) or (Newline) Then // if encountered non-identifier char or newline, stop
   Break;

  Result += Ch;
 End;

 Dec(Position);
End;

(* TLexer.__readNumber *)
{
 Reads a decimal integer or float number.
}
Function TLexer.__readNumber(out OK, isFloat: Boolean; out Str: String): Extended;
Var Ch             : Char;
    Newline, Dot, E: Boolean;
Begin
 Dec(Position);

 Result  := 0;
 Str     := '';
 OK      := True;
 isFloat := False;

 Dot := False;
 E   := False;

 While (true) Do
 Begin
  Ch := __readCharN(Newline);

  if (not (Ch in ['0'..'9', '.', 'e', '+', '-'])) or (Newline) Then // if encountered non-number char or newline, stop
   Break;

  if (Ch = '.') Then
  Begin
   if (Dot) Then // more than one `.`
    Break;

   Dot     := True;
   isFloat := True;
  End;

  if (Ch in ['e', 'E']) Then
  Begin
   if (E) Then // more than one `E`
    Break;

   E       := True;
   isFloat := True;
  End;

  if (Ch in ['+', '-']) Then // `+` and `-` are allowed only after `e`/`E`
  Begin
   if (Length(Str) = 0) Then // invalid use of `+` or `-`
    Break;

   if (not (Str[Length(Str)] in ['e', 'E'])) Then // invalid use of `+` or `-`
    Break;
  End;

  Str += Ch;
 End;

 OK := TryStrToFloat(Str, Result);
 Dec(Position);
End;

(* TLexer.__readHexNumber *)
{
 Reads a hexadecimal integer number.
}
Function TLexer.__readHexNumber(out OK: Boolean; out Str: String): Int64;
Var Ch     : Char;
    Newline: Boolean;
    ValCode: Integer;
Begin
 Result := 0;
 Str    := '$';
 OK     := True;

 While (true) Do
 Begin
  Ch := __readCharN(Newline);

  if (not (Ch in ['0'..'9', 'a'..'f', 'A'..'F'])) or (Newline) Then // if encountered non-hex-number char or newline, stop
   Break;

  Str += Ch;
 End;

 Val(Str, Result, ValCode);
 OK := (ValCode = 0);

 Dec(Position);
End;

(* TLexer.getToken *)
{
 Reads a token.
}
Function TLexer.getToken: TToken;
Var C1, C2, C3: Char;
    NL        : Boolean;

  { Match }
  Function Match(const Str: String): Boolean;
  Begin
   Case Length(Str) of
    1: Result := (Str[1] = C1);
    2: Result := (Str[1] = C1) and (Str[2] = C2);
    3: Result := (Str[1] = C1) and (Str[2] = C2) and (Str[3] = C3);

    else
     raise Exception.CreateFmt('TLexer.getToken::Match() called with invalid argument (str = ''%s'')', [Str]);
   End;
  End;

Begin
 if (not Can) Then // encountered end of file
 Begin
  Result := _EOF;
  Exit;
 End;

 Result := noToken;

 C1 := __readCharN(NL); // first token char

 if (not NL) Then // second token char (if possible)
  C2 := Input[Position] Else
  C2 := #0;

 if (not NL) Then // third token char (if possible)
  C3 := Input[Position+1] Else
  C3 := #0;

 if (C1 in [' ', NewlineChar]) Then // skip spaces and newlines
  Result := getToken(); // @TODO: 'Exit()'?

 if (C1 = '0') and (C2 = 'x') Then
 Begin
  Inc(Position);
  Exit(_HEX_INTEGER);
 End;

 Case C1 of
  '+': Result := _PLUS;
  '-': Result := _MINUS;
  '*': Result := _STAR;
  '/': Result := _SLASH;
  '=': Result := _EQUAL;
  '>': Result := _GREATER;
  '<': Result := _LOWER;
  '!': Result := _EXCLM_MARK;
  '.': Result := _POINT;
  '(': Result := _BRACKET1_OP;
  ')': Result := _BRACKET1_CL;
  '[': Result := _BRACKET2_OP;
  ']': Result := _BRACKET2_CL;
  '{': Result := _BRACKET3_OP;
  '}': Result := _BRACKET3_CL;
  ',': Result := _COMMA;
  ':': Result := _COLON;
  ';': Result := _SEMICOLON;
  '"': Result := _QUOTE;
  '''': Result := _APOSTR;
  '&': Result := _AMPERSAND;
  '@': Result := _AT;
  '_': Result := _UNDERSCORE;
  '|': Result := _PIPE;
  '^': Result := _CARON;
  '%': Result := _PERCENT;
  '~': Result := _TILDE;
  '#': Result := _HASH;

  '0'..'9'          : Result := _NUMBER;
  'a'..'z', 'A'..'Z': Result := _CHAR;
 End;

 if (Match('+=')) Then { += }
 Begin
  Result := _PLUS_EQUAL;
  Inc(Position);
 End Else

 if (Match('-=')) Then { -= }
 Begin
  Result := _MINUS_EQUAL;
  Inc(Position);
 End Else

 if (Match('*=')) Then { *= }
 Begin
  Result := _STAR_EQUAL;
  Inc(Position);
 End Else

 if (Match('/=')) Then { /= }
 Begin
  Result := _SLASH_EQUAL;
  Inc(Position);
 End Else

 if (Match('%=')) Then { %= }
 Begin
  Result := _PERCENT_EQUAL;
  Inc(Position);
 End Else

 if (Match('<<=')) Then { <<= }
 Begin
  Result := _DBL_LOWER_EQUAL;
  Inc(Position, 2);
 End Else

 if (Match('>>=')) Then { >>= }
 Begin
  Result := _DBL_GREATER_EQUAL;
  Inc(Position, 2);
 End Else

 if (Match('|=')) Then { |= }
 Begin
  Result := _PIPE_EQUAL;
  Inc(Position);
 End Else

 if (Match('&=')) Then { &= }
 Begin
  Result := _AMPERSAND_EQUAL;
  Inc(Position);
 End Else

 if (Match('^=')) Then { ^= }
 Begin
  Result := _CARON_EQUAL;
  Inc(Position);
 End Else

 if (C1 in ['>', '<', '=', '!']) and (C2 = '=') Then { >=  <=  ==  != }
 Begin
  if (Result in [_GREATER, _LOWER, _EQUAL, _EXCLM_MARK]) Then
   Inc(Position);

  Case Result of
   _GREATER   : Result := _GREATER_EQUAL;
   _LOWER     : Result := _LOWER_EQUAL;
   _EQUAL     : Result := _EQUAL_EQUAL;
   _EXCLM_MARK: Result := _DIFFERENT;
  End;
 End Else

 if (Match('...')) Then { ... }
 Begin
  Result := _ELLIPSIS;
  Inc(Position, 2);
 End Else

 if (Match('/*')) Then { /* }
 Begin            
  Result := _LONGCMT_OPEN;
  Inc(Position);
 End Else

 if (Match('*/')) Then { */ }
 Begin
  Result := _LONGCMT_CLOSE;
  Inc(Position);
 End Else

 if (Match('//')) Then { // }
 Begin
  Result := _DOUBLE_SLASH;
  Inc(Position);
 End Else
  
 if (Match('::')) Then { :: }
 Begin
  Result := _DOUBLE_COLON;
  Inc(Position);
 End Else

 if (Match('++')) Then { ++ }
 Begin
  Result := _DOUBLE_PLUS;
  Inc(Position);
 End Else

 if (Match('--')) Then { -- }
 Begin
  Result := _DOUBLE_MINUS;
  Inc(Position);
 End Else

 if (Match('**')) Then { ** }
 Begin
  Result := _DOUBLE_STAR;
  Inc(Position);
 End Else

 if (Match('||')) Then { || }
 Begin
  Result := _DOUBLE_PIPE;
  Inc(Position);
 End Else

 if (Match('&&')) Then { && }
 Begin
  Result := _DOUBLE_AMPERSAND;
  Inc(Position);
 End Else

 if (Match('<<')) Then { << }
 Begin
  Result := _DOUBLE_LOWER;
  Inc(Position);
 End Else

 if (Match('>>')) Then { >> }
 Begin
  Result := _DOUBLE_GREATER;
  Inc(Position);
 End;
End;

// -------------------------------------------------------------------------- //
(* TLexer.Create *)
{
 Tokenizes code given in `TStringList`.
}
Constructor TLexer.Create(const fFileName: String; const Lines: TStringList);
Var I: Integer;
Begin
 // save file name for future use
 FileName := fFileName;

 // concatenate lines
 Input := '';

 For I := 0 To Lines.Count-1 Do
  Input += Lines[I] + NewlineChar;

 Position := 1; // `string` is iterated from `1`
End;

(* TLexer.getToken_P *)
{
 Reads a token.
}
Function TLexer.getToken_P: TToken_P;

  { getLine }
  Function getLine: LongWord;
  Var I: LongWord;
  Begin
   Result := 1;

   For I := 1 To Position-1 Do
    if (Input[I] = NewlineChar) Then
     Inc(Result);
  End;

  { getChar }
  Function getChar: LongWord;
  Var I: LongWord;
  Begin
   Result := 0;

   For I := Position-1 Downto 1 Do
    if (Input[I] = NewlineChar) Then
     Exit Else
     Inc(Result);
  End;

// ----- //
Var Token: TToken;
    Ch   : Char;

    OK     : Boolean;
    isFloat: Boolean;

    Flt: Extended;
    Int: Int64;
    Str: String;
Begin
 Token           := getToken;
 Result.Token    := Token;
 Result.Value    := null;
 Result.Position := Position;

 Result.Line     := getLine;
 Result.Char     := getChar;
 Result.FileName := FileName;

 if (Result.Char > 0) Then
  Dec(Result.Char);

 Case Token of
  { strings }
  _QUOTE, _APOSTR:
  Begin
   if (Token = _QUOTE) Then
    Ch := '"' Else
    Ch := '''';

   Result.Token := _STRING;
   Result.Value := __readString(OK, Ch);

   if (Token = _APOSTR) Then
    Result.Token := _CHAR;

   if (not OK) Then
    Result.Token := _INVALID_STRING;
  End;

  { identifiers }
  _CHAR, _UNDERSCORE:
  Begin
   Result.Token := _IDENTIFIER;
   Result.Value := __readIdentifier;

   if (isKeyword(Result.Value)) Then // is it a keyword?
    Result.Token := KeywordToToken(Result.Value);
  End;

  { numbers }
  _NUMBER, _HEX_INTEGER:
  Begin
   isFloat := False;

   Case Token of
    { decimal int or float }
    _NUMBER: Result.Value := __readNumber(OK, isFloat, Str);

    { hexadecimal int }
    _HEX_INTEGER: Result.Value := __readHexNumber(OK, Str);
   End;

   if (isFloat) Then // is float?
   Begin
    Result.Token := _FLOAT;
   End Else // is integer?
   Begin
    Result.Token := _INT;
   End;

   if (not OK) Then // not a valid number?
   Begin
    if (isFloat) Then
     Result.Token := _INVALID_FLOAT Else
     Result.Token := _INVALID_INT;
   End;

   if (OK) Then
   Begin
    // check for underflow and overflow
    Case isFloat of
     // floats
     True:
      if (not TryStrToFloat(Result.Value, Flt)) Then
       Result.Token := _INVALID_FLOAT;

     // integers
     False:
      if (not TryStrToInt64(Result.Value, Int)) Then
      Begin
       if (TryStrToFloat(Result.Value, Flt)) Then
       Begin
        Result.Token := _FLOAT;
       End Else
       Begin
        Result.Token := _INVALID_INT;
       End;
      End;
    End;
   End;
  End;
 End;

 Result.TokenName := getTokenName(Result.Token);

 if (Result.Value = null) Then
 Begin
  Result.Value := getTokenDisplay(Result.Token);

  if (Result.Value = '') Then
   Result.Value := Input[Result.Position];
 End;

 if (not ((Result.Token in [_IDENTIFIER, _STRING, _INVALID_STRING]) or (isKeyword(Result.Value)))) Then
  Result.Char += 1;
End;

(* TLexer.Can *)
{
 Returns 'true', if there's at least one token to be read.
}
Function TLexer.Can: Boolean;
Begin
 Result := (Position < uint32(Length(Input)));
End;
End.
