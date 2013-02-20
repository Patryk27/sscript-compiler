(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Scanner;

 Interface
 Uses Tokens, SysUtils, Classes;

 Type TScanner = Class
                  Private
                   Code         : String;
                   Position     : Integer;
                   Line         : Integer;
                   NLPosition   : Integer;
                   CommentOpened: Boolean;

                   Function __readChar(out NewLine, Escaped: Boolean; const AllowFormatting: Boolean=False): Char;
                   Function __readCharN(out NewLine: Boolean; const AllowFormatting: Boolean=False): Char;
                   Function __readString(out OK: Boolean; const Ch: Char): String;
                   Function __readIdentifier: String;
                   Function __readNumber(out OK: Boolean; out Dot: Boolean): Extended;

                   Procedure IncPosition;
                   Procedure DecPosition;
                  Public
                   Constructor Create(Code_: TStringList);
                   Destructor Destroy; override;

                   Function getNextToken: TToken;
                   Function getNextToken_P(const EnableKeywords: Boolean=True; const EnableComments: Boolean=True): TToken_P;

                   Function getLine: Integer;
                   Function getChar: Integer;

                   Function Can: Boolean;
                 End;

 Implementation

{ TScanner.Create }
Constructor TScanner.Create(Code_: TStringList);
Var I: Integer;
Begin
 For I := 0 To Code_.Count-1 Do
  Code_[I] := Code_[I]+#0;

 Code := Code_.Text;

 I := 1;
 if (Length(Code) > 0) Then
  Repeat
   if (Code[I] = #9) Then
    Delete(Code, I, 1) Else
    Inc(I);
  Until (I >= Length(Code));

 Position      := 1;
 NLPosition    := 0;
 Line          := 0;
 CommentOpened := False;
End;

{ TScanner.Destroy }
Destructor TScanner.Destroy;
Begin
 inherited;
End;

{ TScanner.IncPosition }
Procedure TScanner.IncPosition;
Begin
 Inc(Position);
 Inc(NLPosition);

 if (Code[Position-1] = #0) Then
 Begin
  NLPosition := -2;
  Inc(Line);
 End;
End;

{ TScanner.DecPosition }
Procedure TScanner.DecPosition;
Begin
 Dec(Position);
 Dec(NLPosition);
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

 IncPosition;

 if (Result = #0) Then
 Begin
  NewLine := True;
  Exit;
 End;

 if (Result in [#13, #10]) Then // skip newlines
  Result := __readCharN(NewLine);

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
    Exit(chr(StrToInt('$'+Tmp)));
   End Else

   { read a decimal number }
   Begin
    Tmp += C;

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
    Exit(chr(StrToInt(Tmp)));
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
Label InvalidString;
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
 Begin
  InvalidString:
  OK := False;
 End;
End;

{ TScanner.__readIdentifier }
Function TScanner.__readIdentifier: String;
Var C : Char;
    NL: Boolean;
Begin
 Result := '';

 DecPosition;

 Repeat
  C      := __readCharN(NL);
  Result += C;
 Until (not (C in identAllowed)) or (NL);

 DecPosition;

 if (not (Result[Length(Result)] in identAllowed)) Then
  Delete(Result, Length(Result), 1);

 Result := Trim(Result);

 if (NL) Then
  Dec(Line); 
End;

{ TScanner.__readNumber }
Function TScanner.__readNumber(out OK: Boolean; out Dot: Boolean): Extended;
Var Str  : String;
    C    : Char;
    NL   : Boolean;
    CodeI: Integer;
Begin
 Result := 0;
 Str    := '';
 OK     := True;
 Dot    := False;

 DecPosition;

 Repeat
  C := __readCharN(NL);

  if (C = '.') Then
   if (Dot) Then
   Begin
    OK := False;
    Exit;
   End Else
    Dot := True;

  Str += C;
 Until (not (C in ['0'..'9','.'])) or (NL);

 DecPosition;

 if (not (Str[Length(Str)] in ['0'..'9'])) Then
  Delete(Str, Length(Str), 1);

 Str := Trim(Str);

 Val(Str, Result, CodeI);
 if (CodeI <> 0) Then
  OK := False;

 Dot := (Pos('.', Str) > 0);
End;

{ TScanner.getNextToken }
Function TScanner.getNextToken: TToken;
Var C1, C2, C3: Char;
    NL        : Boolean;

Function Double(X: Char): Boolean;
Begin
 Result := (C1 = X) and (C2 = X);
End;

Begin
 if (not Can) Then
 Begin
  Result := _EOF;
  Exit;
 End;

 Result := noToken;

 C1 := __readCharN(NL);

 if (not NL) Then
  C2 := Code[Position] Else
  C2 := #0;

 if (not NL) Then
  C3 := Code[Position+1] Else
  C3 := #0;

 if (C1 in [' ', #0]) Then                              
  Result := getNextToken;

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

  '0'..'9': Result := _INTEGER;
  'a'..'z': Result := _CHAR;
  'A'..'Z': Result := _CHAR;
 End;

 if (C1 = '+') and (C2 = '=') Then
 Begin
  Result := _PLUS_EQUAL;
  IncPosition;
 End Else

 if (C1 = '-') and (C2 = '=') Then
 Begin
  Result := _MINUS_EQUAL;
  IncPosition;
 End Else

 if (C1 = '*') and (C2 = '=') Then
 Begin
  Result := _STAR_EQUAL;
  IncPosition;
 End Else

 if (C1 = '/') and (C2 = '=') Then
 Begin
  Result := _SLASH_EQUAL;
  IncPosition;
 End Else

 if (C1 = '%') and (C2 = '=') Then
 Begin
  Result := _PERCENT_EQUAL;
  IncPosition;
 End Else

 if (C2 = '=') and (C1 <> ' ') Then
 Begin
  if (Result in [_GREATER, _LOWER, _EQUAL, _EXCLM_MARK]) Then
   IncPosition;
  Case Result of
   _GREATER   : Result := _GREATER_EQUAL;
   _LOWER     : Result := _LOWER_EQUAL;
   _EQUAL     : Result := _EQUAL_EQUAL;
   _EXCLM_MARK: Result := _DIFFERENT;
  End;
 End Else

 if (C1 = '.') and (C2 = '.') and (C3 = '.') Then
 Begin
  Result := _ELLIPSIS;
  IncPosition;
  IncPosition;
 End Else

 if (C1 = '/') and (C2 = '*') Then
 Begin            
  Result := _LONGCMT_OPEN;
  IncPosition;
 End Else

 if (C1 = '*') and (C2 = '/') Then
 Begin
  Result := _LONGCMT_CLOSE;
  IncPosition;
 End Else

 if (Double('/')) Then
 Begin
  Result := _DOUBLE_SLASH;
  IncPosition;
 End Else
  
 if (Double(':')) Then
 Begin
  Result := _DOUBLE_COLON;
  IncPosition;
 End Else

 if (Double('+')) Then
 Begin
  Result := _DOUBLE_PLUS;
  IncPosition;
 End Else

 if (Double('-')) Then
 Begin
  Result := _DOUBLE_MINUS;
  IncPosition;
 End Else

 if (Double('*')) Then
 Begin
  Result := _DOUBLE_STAR;
  IncPosition;
 End Else

 if (Double('|')) Then
 Begin
  Result := _DOUBLE_PIPE;
  IncPosition;
 End Else

 if (Double('&')) Then               
 Begin
  Result := _DOUBLE_AMPERSAND;
  IncPosition;
 End Else

 if (Double('<')) Then
 Begin
  Result := _DOUBLE_LOWER;
  IncPosition;
 End Else

 if (Double('>')) Then
 Begin
  Result := _DOUBLE_GREATER;
  IncPosition;
 End;
End;

{ TScanner.getNextToken_P }
Function TScanner.getNextToken_P(const EnableKeywords: Boolean=True; const EnableComments: Boolean=True): TToken_P;
Var Token: TToken;
    OK   : Boolean;
    Dot  : Boolean;
    Ch   : Char;
Begin
 Token        := getNextToken;
 Result.Token := Token;
 Result.Value := '';
 Result.Posi  := Position;
 Result.Line  := Line;
 Result.Char  := NLPosition;

 if (EnableComments) Then
  if (Result.Token = _LONGCMT_OPEN) Then
   CommentOpened := True Else
  if (Result.Token = _LONGCMT_CLOSE) Then
   CommentOpened := False Else
   if (CommentOpened) Then
   Begin
    Result.Token := noToken;
    Exit;
   End;

 Case Token of
  _QUOTE, _APOSTR: Begin // strings
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
  _CHAR, _UNDERSCORE: Begin // identifiers
                       Result.Value := __readIdentifier;
                       Result.Token := _IDENTIFIER;

                       if (EnableKeywords) Then
                        if (isKeyword(Result.Value)) Then
                         Result.Token := KeywordToToken(Result.Value);
                      End;
  _INTEGER: Begin // numbers
             Result.Token := _INTEGER;
             Result.Value := FloatToStr(__readNumber(OK, Dot));

             if (Dot) Then
             Begin
              Result.Token := _FLOAT;
              if (Pos('.', Result.Value) = 0) Then
               Result.Value := Result.Value+'.0';
             End;

             if (not OK) Then
              Result.Token := noToken;
             End;
 End;

 Result.TokenName := getTokenName(Result.Token);
 Result.Display   := getTokenDisplay(Result.Token);
 if (Result.Display = '') Then
  if (Result.Token in [_STRING, _IDENTIFIER, _INTEGER, _FLOAT{, noToken}]) Then
   Result.Display := Result.Value Else
   Result.Display := Code[Result.Posi];
End;

{ TScanner.getLine }
Function TScanner.getLine: Integer;
Begin
 Result := Line;
End;

{ TScanner.getChar }
Function TScanner.getChar: Integer;
Begin
 Result := NLPosition;
End; 

{ TScanner.Can }
Function TScanner.Can: Boolean;
Begin
 Result := (Position < Length(Code)-1);
End;
End.
