(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Tokens;

 Interface
 Uses TypInfo;                                                    

 Const IdentAllowed = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
 Const Keywords     : Array[0..24] of String = ('function', 'var', 'const', 'return', 'naked', 'for', 'if', 'else', 'while', 'break', 'continue',
                                                'in', 'do', 'public', 'private', 'type', 'new', 'delete', 'namespace', 'use', 'cast', 'strict',
                                                'try', 'catch', 'throw');

 // IMPORTANT: do not change tokens order and do not try to remove any token, unless you change order in "Token_Display" (see below)
 Type TToken =
 (
  noToken,        //
  _PLUS,          // +
  _DOUBLE_PLUS,   // ++
  _PLUS_EQUAL,    // +=
  _MINUS,         // -                                                 
  _DOUBLE_MINUS,  // --
  _MINUS_EQUAL,   // -=
  _STAR,          // *
  _DOUBLE_STAR,   // **
  _STAR_EQUAL,    // *=
  _SLASH,         // /
  _DOUBLE_SLASH,  // //
  _SLASH_EQUAL,   // /=
  _EQUAL,         // =
  _EQUAL_EQUAL,   // == 
  _GREATER,       // >
  _DOUBLE_GREATER,// >>
  _LOWER,         // <
  _DOUBLE_LOWER,  // <<
  _GREATER_EQUAL, // >=
  _LOWER_EQUAL,   // <=
  _EXCLM_MARK,    // !
  _DIFFERENT,     // !=
  _POINT,         // .                     
  _BRACKET1_OP,   // (
  _BRACKET1_CL,   // )
  _BRACKET2_OP,   // [
  _BRACKET2_CL,   // ]
  _BRACKET3_OP,   // {
  _BRACKET3_CL,   // }
  _COMMA,         // ,
  _COLON,         // :
  _DOUBLE_COLON,  // ::
  _SEMICOLON,     // ;
  _QUOTE,         // "
  _APOSTR,        // '
  _AT,            // @
  _UNDERSCORE,    // _
  _PIPE,          // |
  _DOUBLE_PIPE,   // ||
  _CARON,         // ^
  _PERCENT,       // %
  _PERCENT_EQUAL, // %=
  _EOF,           //
  _NUMBER,        // int or float
  _HEX_INTEGER,   // hexadecimal number
  _INT,           // integer value
  _FLOAT,         // float value
  _STRING,        // string value
  _INVALID_STRING,//
  _INVALID_INT,   //
  _INVALID_FLOAT, //
  _CHAR,          //                               
  _MCHAR,         //
  _AMPERSAND,     // &
  _DOUBLE_AMPERSAND, // &&
  _HASH,          // #
  _GRAVE_ACCENT,  // `
  _ELLIPSIS,      // ...
  _TILDE,         // ~
  _SPACE,         //
  _LONGCMT_OPEN,  // /*
  _LONGCMT_CLOSE, // */
  _IDENTIFIER,    //

  _TT,            // [keyword list]
  _FUNCTION,      // function
  _VAR,           // var
  _CONST,         // const
  _RETURN,        // return
  _NAKED,         // naked
  _FOR,           // for
  _IF,            // if
  _ELSE,          // else
  _WHILE,         // while
  _BREAK,         // break
  _CONTINUE,      // continue
  _IN,            // in
  _DO,            // do
  _PUBLIC,        // public
  _PRIVATE,       // private
  _TYPE,          // type
  _NEW,           // new
  _DELETE,        // delete
  _NAMESPACE,     // namespace
  _USE,           // use
  _CAST,          // cast
  _STRICT,        // strict
  _TRY,           // try
  _CATCH,         // catch
  _THROW,         // throw

  _NEWLINE
 );

 Type TTokenSet = Set of TToken;

 Const Token_Display: Array[TToken] of String =
 (
  '',
  '+',
  '++',
  '+=',
  '-',
  '--',
  '-=',
  '*',
  '**',
  '*=',
  '/',
  '//',
  '%=',
  '=',
  '==',
  '>',
  '>>',
  '<',
  '<<',
  '>=',
  '<=',
  '!',
  '!=',
  '.',
  '(',
  ')',
  '[',
  ']',
  '{',
  '}',
  ',',
  ':',
  '::',
  ';',
  '"',
  '''',
  '@',
  '_',
  '|',
  '||',
  '^',
  '%',
  '%=',
  '',
  '',    
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '',
  '&',
  '&&',
  '#',
  '`',
  '...',
  '~',
  ' ',
  '/*',
  '*/',
  '',
  '',
  'function',
  'var',
  'const',
  'return',
  'naked',
  'for',
  'if',
  'else',
  'while',
  'break',
  'continue',
  'in',
  'do',
  'public',
  'private',
  'type',
  'new',
  'delete',
  'namespace',
  'use',
  'cast',
  'strict',
  'try',
  'catch',
  'throw',
  ''
 );

 Type PToken_P = ^TToken_P;
      TToken_P = Record
                  Token                    : TToken;
                  TokenName, Value, Display: String;
                  Position, Line, Char     : LongWord;
                 End;

 Function getTokenName(T: TToken): String;
 Function getTokenDisplay(T: TToken): String;
 Function isKeyword(S: String): Boolean;
 Function KeywordToToken(S: String): TToken;
 
 Implementation

(* getTokenName *)
{
 Gets token name.
 Eg. for token `>` returns '_GREATER'
}
Function getTokenName(T: TToken): String;
Begin                                         
 Result := getEnumName(TypeInfo(TToken), ord(T));
End;

(* getTokenDisplay *)
{
 Returns how a token looks in code.
 Eg.for token `_GREATER` returns '>'
}
Function getTokenDisplay(T: TToken): String;
Begin
 Result := Token_Display[T];
End;

(* isKeyword *)
{
 Returns `true` when passed string is a keyword.
}
Function isKeyword(S: String): Boolean;
Var I: Integer;
Begin
 Result := False;

 For I := Low(Keywords) To High(Keywords) Do
  if (Keywords[I] = S) Then
  Begin
   Result := True;
   Exit;                               
  End;
End;

(* KeywordToToken *)
{
 Converts a keyword into token; when no such token can be found, returns `noToken`.
}
Function KeywordToToken(S: String): TToken;
Var I, Q: Integer;
Begin
 Result := noToken;
 Q      := -1;
 
 For I := Low(Keywords) To High(Keywords) Do
  if (Keywords[I] = S) Then
   Q := I;

 if (Q <> -1) Then
  Result := TToken(ord(_TT)+Q+1);
End;
End.
