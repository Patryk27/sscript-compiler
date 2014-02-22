(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Tokens;

 Interface
 Uses TypInfo;                                                    

 Const IdentAllowed = ['a'..'z', 'A'..'Z', '0'..'9', '_']; // chars allowed in identifiers; don't change it if you don't know what you're doing.
 Const Keywords     : Array[0..25] of String = ('function', 'var', 'const', 'return', 'naked', 'for', 'if', 'else', 'while', 'break', 'continue',
                                                'in', 'do', 'public', 'private', 'type', 'new', 'namespace', 'use', 'cast', 'strict', 'try',
                                                'catch', 'throw', 'enum', 'foreach');

 { TToken }
 Type TToken =
 (
  noToken,            //
  _PLUS,              // +
  _DOUBLE_PLUS,       // ++
  _PLUS_EQUAL,        // +=
  _MINUS,             // -
  _DOUBLE_MINUS,      // --
  _MINUS_EQUAL,       // -=
  _STAR,              // *
  _DOUBLE_STAR,       // **
  _STAR_EQUAL,        // *=
  _SLASH,             // /
  _DOUBLE_SLASH,      // //
  _SLASH_EQUAL,       // /=
  _EQUAL,             // =
  _EQUAL_EQUAL,       // ==
  _GREATER,           // >
  _DOUBLE_GREATER,    // >>
  _DBL_GREATER_EQUAL, // >>=
  _LOWER,             // <
  _DOUBLE_LOWER,      // <<
  _DBL_LOWER_EQUAL,   // <<=
  _GREATER_EQUAL,     // >=
  _LOWER_EQUAL,       // <=
  _EXCLM_MARK,        // !
  _DIFFERENT,         // !=
  _POINT,             // .
  _BRACKET1_OP,       // (
  _BRACKET1_CL,       // )
  _BRACKET2_OP,       // [
  _BRACKET2_CL,       // ]
  _BRACKET3_OP,       // {
  _BRACKET3_CL,       // }
  _COMMA,             // ,
  _COLON,             // :
  _DOUBLE_COLON,      // ::
  _SEMICOLON,         // ;
  _QUOTE,             // "
  _APOSTR,            // '
  _AT,                // @
  _UNDERSCORE,        // _
  _PIPE,              // |
  _PIPE_EQUAL,        // |=
  _DOUBLE_PIPE,       // ||
  _CARON,             // ^
  _CARON_EQUAL,       // ^=
  _PERCENT,           // %
  _PERCENT_EQUAL,     // %=
  _EOF,               //
  _NUMBER,            // int or float
  _HEX_INTEGER,       // hexadecimal number
  _INT,               // integer value
  _FLOAT,             // float value
  _STRING,            // string value
  _INVALID_STRING,    //
  _INVALID_INT,       //
  _INVALID_FLOAT,     //
  _CHAR,              //
  _MCHAR,             //
  _AMPERSAND,         // &
  _AMPERSAND_EQUAL,   // &=
  _DOUBLE_AMPERSAND,  // &&
  _HASH,              // #
  _GRAVE_ACCENT,      // `
  _ELLIPSIS,          // ...
  _TILDE,             // ~
  _SPACE,             //
  _LONGCMT_OPEN,      // /*
  _LONGCMT_CLOSE,     // */
  _IDENTIFIER,        //

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
  _NAMESPACE,     // namespace
  _USE,           // use
  _CAST,          // cast
  _STRICT,        // strict
  _TRY,           // try
  _CATCH,         // catch
  _THROW,         // throw
  _ENUM,          // enum
  _FOREACH,       // foreach

  _NEWLINE
 );

 { TTokenSet }
 Type TTokenSet = Set of TToken;

 Const TokenDisplay: Array[TToken] of String =
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
  '>>=',
  '<',
  '<<',
  '<<=',
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
  '|=',
  '||',
  '^',
  '^=',
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
  '&=',
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
  'namespace',
  'use',
  'cast',
  'strict',
  'try',
  'catch',
  'throw',
  'enum',
  'foreach',
  ''
 );

 { TToken_P }
 Type PToken_P = ^TToken_P;
      TToken_P =
      Record
       Token    : TToken;
       TokenName: String;

       Value: Variant;

       Position, Line, Char: uint32;
      End;

 Function getTokenName(const T: TToken): String;
 Function getTokenDisplay(const T: TToken): String;
 Function isKeyword(const S: String): Boolean;
 Function KeywordToToken(const S: String): TToken;
 
 Implementation

(* getTokenName *)
{
 Returns token name.
 Eg. for token `>` returns '_GREATER'
}
Function getTokenName(const T: TToken): String;
Begin                                         
 Result := getEnumName(TypeInfo(TToken), ord(T));
End;

(* getTokenDisplay *)
{
 Returns ASCII representation of token.
 Eg.for token `_GREATER` returns '>'
}
Function getTokenDisplay(const T: TToken): String;
Begin
 Result := TokenDisplay[T];
End;

(* isKeyword *)
{
 Returns `true` when passed string is a keyword.
}
Function isKeyword(const S: String): Boolean;
Var Str: String;
Begin
 Result := False;

 For Str in Keywords Do
  if (Str = S) Then
   Exit(True);
End;

(* KeywordToToken *)
{
 Converts a keyword into token; when no such token can be found returns `noToken`.
}
Function KeywordToToken(const S: String): TToken;
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
