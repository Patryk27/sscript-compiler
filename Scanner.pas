(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Scanner;

 Interface
 Uses Tokens, SysUtils, Classes;

 { TScanner }
 Type TScanner = Class
                  Private
                   Code    : String;
                   Position: Int64;

                   Function __readChar(out NewLine, Escaped: Boolean; const AllowFormatting: Boolean=False): Char;
                   Function __readCharN(out NewLine: Boolean; const AllowFormatting: Boolean=False): Char;
                   Function __readString(out OK: Boolean; const Ch: Char): String;
                   Function __readIdentifier: String;
                   Function __readNumber(out OK: Boolean; out isFloat: Boolean; out Str: String): Extended;
                   Function __readHexNumber(out OK: Boolean; out Str: String): Int64;

                   Function getToken: TToken;

                  Public
                   Constructor Create(Lines: TStringList);

                   Function getToken_P: TToken_P;

                   Function Can: Boolean;
                 End;

 Implementation
Const NewlineChar = #13;

{ TScanner.Create }
Constructor TScanner.Create(Lines: TStringList);
Var I: Integer;
Begin
 Code := '';

 For I := 0 To Lines.Count-1 Do
  Code += Lines[I] + #13;

 Position := 1; // `string` is iterated from `1`
End;

{ TScanner.__readChar }
Function TScanner.__readChar(out NewLine, Escaped: Boolean; const AllowFormatting: Boolean=False): Char;
Var C  : Char;
    Tmp: String;
Label Now1, Now2;
Begin
 NewLine := False;
 Escaped := False;

 Result := Code[Position];

 Inc(Position);

 if (Result = NewlineChar) Then // newline
 Begin
  NewLine := True;
  Exit;
 End;

 //if (Result = [#13]) Then // skip newlines
 // Result := __readCharN(NewLine);

 if (Result = '\') and (AllowFormatting) Then // char escape
 Begin
  C       := __readCharN(NewLine);
  Escaped := True;

  Case C of
   'n': Exit(#$0A);
   'r': Exit(#$0D);
   't': Exit(#$09);
   'v': Exit(#$0B);
   'f': Exit(#$0C);
  End;

  if (C in ['0'..'9']) Then // read a number
  Begin
   Tmp := C;

   C := __readCharN(NewLine);
   if (NewLine) Then
    Exit(#0);

   { read a hexadecimal number }
   if (C = 'x') Then { 0x ... }
   Begin
    While (true) Do
    Begin
     C := __readCharN(NewLine);

     if (NewLine) Then
      goto Now1;

     if not (C in ['0'..'9', 'a'..'f', 'A'..'F']) Then
      Break Else
      Tmp += C;
    End;

   Now1:
    Dec(Position);
    Exit(chr(StrToInt('$'+Trim(Tmp))));
   End Else

   { read a decimal number }
   Begin
    if (C in ['0'..'9']) Then
     Tmp += C Else
     Dec(Position);

    While (true) Do
    Begin
     C := __readCharN(NewLine);

     if (NewLine) Then
      goto Now2;

     if not (C in ['0'..'9']) Then
      Break Else
      Tmp += C;
    End;

   Now2:
    Dec(Position);
    Exit(chr(StrToInt(Trim(Tmp))));
   End;
  End Else
   Exit(C);
 End;
End;

{ TScanner.__readChar }
Function TScanner.__readCharN(out NewLine: Boolean; const AllowFormatting: Boolean=False): Char;
Var Escaped: Boolean;
Begin
 Result := __readChar(NewLine, Escaped, AllowFormatting);
End;

{ TScanner.__readString }
Function TScanner.__readString(out OK: Boolean; const Ch: Char): String;
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

{ TScanner.__readIdentifier }
Function TScanner.__readIdentifier: String;
Var Ch     : Char;
    Newline: Boolean;
Begin
 Dec(Position);
 Result := '';

 While (true) Do
 Begin
  Ch := __readCharN(Newline);

  if (not (Ch in IdentAllowed)) or (Newline) Then
   Break;

  Result += Ch;
 End;

 Dec(Position);
End;

{ TScanner.__readNumber }
Function TScanner.__readNumber(out OK: Boolean; out isFloat: Boolean; out Str: String): Extended;
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

  if (not (Ch in ['0'..'9', '.', 'e', '+', '-'])) or (Newline) Then
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
   if (Length(Str) = 0) Then
    Break;

   if not (Str[Length(Str)] in ['e', 'E']) Then
    Break;
  End;

  Str += Ch;
 End;

 OK := TryStrToFloat(Str, Result);
 Dec(Position);
End;

{ TScanner.__readHexNumber }
Function TScanner.__readHexNumber(out OK: Boolean; out Str: String): Int64;
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

  if (not (Ch in ['0'..'9', 'a'..'f', 'A'..'F'])) or (Newline) Then
   Break;

  Str += Ch;
 End;

 Val(Str, Result, ValCode);
 OK := (ValCode = 0);

 Dec(Position);
End;

{ TScanner.getToken }
Function TScanner.getToken: TToken;
Var C1, C2, C3: Char;
    NL        : Boolean;

  // Double
  Function Double(X: Char): Boolean;
  Begin
   Exit((C1 = X) and (C2 = X));
  End;

Begin
 if (not Can) Then // end of file
 Begin
  Result := _EOF;
  Exit;
 End;

 Result := noToken;

 C1 := __readCharN(NL); // first char

 if (not NL) Then // second char (if possible)
  C2 := Code[Position] Else
  C2 := #0;

 if (not NL) Then // third char (if possible)
  C3 := Code[Position+1] Else
  C3 := #0;

 if (C1 in [' ', NewlineChar]) Then // skip spaces and newlines
  Result := getToken();

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

  '0'..'9': Result := _NUMBER;
  'a'..'z': Result := _CHAR;
  'A'..'Z': Result := _CHAR;
 End;

 if (C1 = '+') and (C2 = '=') Then { += }
 Begin
  Result := _PLUS_EQUAL;
  Inc(Position);
 End Else

 if (C1 = '-') and (C2 = '=') Then { -= }
 Begin
  Result := _MINUS_EQUAL;
  Inc(Position);
 End Else

 if (C1 = '*') and (C2 = '=') Then { *= }
 Begin
  Result := _STAR_EQUAL;
  Inc(Position);
 End Else

 if (C1 = '/') and (C2 = '=') Then { /= }
 Begin
  Result := _SLASH_EQUAL;
  Inc(Position);
 End Else

 if (C1 = '%') and (C2 = '=') Then { %= }
 Begin
  Result := _PERCENT_EQUAL;
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

 if (C1 = '.') and (C2 = '.') and (C3 = '.') Then { ... }
 Begin
  Result := _ELLIPSIS;
  Inc(Position, 2);
 End Else

 if (C1 = '/') and (C2 = '*') Then { /* }
 Begin            
  Result := _LONGCMT_OPEN;
  Inc(Position);
 End Else

 if (C1 = '*') and (C2 = '/') Then { */ }
 Begin
  Result := _LONGCMT_CLOSE;
  Inc(Position);
 End Else

 if (Double('/')) Then { // }
 Begin
  Result := _DOUBLE_SLASH;
  Inc(Position);
 End Else
  
 if (Double(':')) Then { :: }
 Begin
  Result := _DOUBLE_COLON;
  Inc(Position);
 End Else

 if (Double('+')) Then { ++ }
 Begin
  Result := _DOUBLE_PLUS;
  Inc(Position);
 End Else

 if (Double('-')) Then { -- }
 Begin
  Result := _DOUBLE_MINUS;
  Inc(Position);
 End Else

 if (Double('*')) Then { ** }
 Begin
  Result := _DOUBLE_STAR;
  Inc(Position);
 End Else

 if (Double('|')) Then { || }
 Begin
  Result := _DOUBLE_PIPE;
  Inc(Position);
 End Else

 if (Double('&')) Then { && }
 Begin
  Result := _DOUBLE_AMPERSAND;
  Inc(Position);
 End Else

 if (Double('<')) Then { << }
 Begin
  Result := _DOUBLE_LOWER;
  Inc(Position);
 End Else

 if (Double('>')) Then { >> }
 Begin
  Result := _DOUBLE_GREATER;
  Inc(Position);
 End;
End;

{ TScanner.getToken_P }
Function TScanner.getToken_P: TToken_P;

  // getLine
  Function getLine: LongWord;
  Var I: LongWord;
  Begin
   Result := 1;

   For I := 1 To Position-1 Do
    if (Code[I] = NewlineChar) Then
     Inc(Result);
  End;

  // getChar
  Function getChar: LongWord;
  Var I: LongWord;
  Begin
   Result := 0;

   For I := Position-1 Downto 1 Do
    if (Code[I] = NewlineChar) Then
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
Begin
 Token           := getToken;
 Result.Token    := Token;
 Result.Value    := '';
 Result.Display  := '';
 Result.Position := Position;

 Result.Line := getLine;
 Result.Char := getChar;

 if (Result.Char > 0) Then
  Dec(Result.Char);

 Case Token of
  { strings }
  _QUOTE, _APOSTR:
  Begin
   if (Token = _QUOTE) Then
    Ch := '"' Else
    Ch := '''';

   Result.Token   := _STRING;
   Result.Value   := __readString(OK, Ch);
   Result.Display := Result.Value;

   if (Token = _APOSTR) Then
    Result.Token := _CHAR;

   if (not OK) Then
    Result.Token := _INVALID_STRING;
  End;

  { identifiers }
  _CHAR, _UNDERSCORE:
  Begin
   Result.Token   := _IDENTIFIER;
   Result.Value   := __readIdentifier;
   Result.Display := Result.Value;

   if (isKeyword(Result.Value)) Then // is keyword?
    Result.Token := KeywordToToken(Result.Value);
  End;

  { numbers }
  _NUMBER, _HEX_INTEGER:
  Begin
   isFloat := False;

   Case Token of
    { decimal int or float }
    _NUMBER: Result.Value := FloatToStr(__readNumber(OK, isFloat, Result.Display));

   { hexadecimal int }
    _HEX_INTEGER: Result.Value := IntToStr(__readHexNumber(OK, Result.Display));
   End;

   if (isFloat) Then // is float?
   Begin
    Result.Token := _FLOAT;

    if (Pos('.', Result.Value) = 0) Then
     Result.Value := Result.Value+'.0';
   End Else // is integer?
    Result.Token := _INT;

   if (not OK) Then // not a valid number?
    if (isFloat) Then
     Result.Token := _INVALID_FLOAT Else
     Result.Token := _INVALID_INT;

   Case isFloat of
    True:
     if (not TryStrToFloat(Result.Display, Flt)) Then
      Result.Token := _INVALID_FLOAT;

    False:
     if (not TryStrToInt64(Result.Display, Int)) Then
      Result.Token := _INVALID_INT;
   End;
  End;
 End;

 Result.TokenName := getTokenName(Result.Token);

 if (Result.Display = '') Then
 Begin
  Result.Display := getTokenDisplay(Result.Token);

  if (Result.Display = '') Then
   Result.Display := Code[Result.Position];
 End;

 if (not ((Result.Token in [_IDENTIFIER, _STRING, _INVALID_STRING]) or (isKeyword(Result.Value)))) Then
  Result.Char += 1;
End;

{ TScanner.Can }
Function TScanner.Can: Boolean;
Begin
 Result := (Position < Length(Code));
End;
End.
