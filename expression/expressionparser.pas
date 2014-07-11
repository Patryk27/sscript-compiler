(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)

{.$DEFINE DISPLAY_TREE} // debug only
{$MODESWITCH ADVANCEDRECORDS}
{$MACRO ON}

Unit ExpressionParser;

 Interface
 Uses SysUtils, HLCompiler, Expression, Scanner, symdef, Tokens, Variants, TypInfo;

 { EExpressionParserException }
 Type EExpressionParserException = Class(Exception);

 { TExpressionParser }
 Type TExpressionParser =
      Class
       Private
        Compiler: TCompiler;

       Public
        Constructor Create(const fCompiler: TCompiler);

        Function Parse(const EndTokens: TTokenSet): TExpressionNode;
       End;

 Implementation
Uses Messages;

(* TExpressionParser.Create *)
Constructor TExpressionParser.Create(const fCompiler: TCompiler);
Begin
 Compiler := fCompiler;
End;

(* TExpressionParser.Parse *)
Function TExpressionParser.Parse(const EndTokens: TTokenSet): TExpressionNode;
Const Order1: TTokenSet = [_EQUAL, _PLUS_EQUAL, _MINUS_EQUAL, _SLASH_EQUAL, _STAR_EQUAL, _PERCENT_EQUAL, _DBL_LOWER_EQUAL, _DBL_GREATER_EQUAL, _PIPE_EQUAL, _AMPERSAND_EQUAL, _CARON_EQUAL];
      Order2: TTokenSet = [_DOUBLE_PIPE, _DOUBLE_AMPERSAND];
      Order3: TTokenSet = [_LOWER, _LOWER_EQUAL, _EQUAL_EQUAL, _GREATER, _GREATER_EQUAL, _DIFFERENT];
      Order4: TTokenSet = [_DOUBLE_LOWER, _DOUBLE_GREATER];
      Order5: TTokenSet = [_PLUS, _MINUS];
      Order6: TTokenSet = [_STAR, _SLASH, _PERCENT];
      Order8: TTokenSet = [_PIPE, _CARON, _AMPERSAND];

Var Scanner  : TScanner;
    LoopToken: TToken_P;

  { WhileCondition }
  Function WhileCondition(const TokenSet: TTokenSet): Boolean;
  Begin
   LoopToken := Scanner.next;

   Result := (LoopToken.Token in TokenSet); // and (not (Token.Token in EndTokens));

   if (Result) Then
    Scanner.read;
  End;

  { ReadSymbol }
  Function ReadSymbol(out NamespaceName, IdentifierName: String; out Namespace: TNamespace): TSymbol;
  Var Token: TToken_P;
  Begin
   Token := Scanner.next(-1);

   if (Scanner.next_t = _DOUBLE_COLON) Then // namespace resolution operator (namespace::ident)
   Begin
    Scanner.read; // skip `::`

    NamespaceName  := Token.Value;
    IdentifierName := Scanner.read_ident;

    Namespace := Compiler.findNamespace(NamespaceName);

    if (Namespace = nil) Then // error: namespace not found
    Begin
     Compiler.CompileError(Token, eUnknownNamespace, [NamespaceName]);
     Exit(nil);
    End;

    Exit(Namespace.findSymbol(IdentifierName)); // assume no multiple symbols with the same name exist in one namespace
   End Else
   Begin
    IdentifierName := Token.Value;
    Exit(Compiler.findCandidate(Token.Value, nil, Token));
   End;
  End;

  { ParseOrder9 }
  Function ParseOrder9: TExpressionNode; //  const-expr   group-expr   array-expr   function-call   cast-expr
  Var Token, First                 : TToken_P;
      Symbol                       : TSymbol;
      NamespaceName, IdentifierName: String;
      mNamespace                   : TNamespace;
      mType                        : TType;
  Label ParseModifiers;
  Begin
   Result := nil;

   Token := Scanner.read;
   First := Token;

   Case Token.Token of
    { a char constant }
    _CHAR:
    Begin
     Result := TCharExpressionNode.Create(Compiler.getExpressionCompiler, Token, Token.Value);
    End;

    { an integer constant }
    _INT:
    Begin
     Result := TIntegerExpressionNode.Create(Compiler.getExpressionCompiler, Token, Token.Value);
    End;

    { a float constant }
    _FLOAT:
    Begin
     Result := TFloatExpressionNode.Create(Compiler.getExpressionCompiler, Token, Token.Value);
    End;

    { a string constant }
    _STRING:
    Begin
     Result := TStringExpressionNode.Create(Compiler.getExpressionCompiler, Token, Token.Value);
    End;

    { a variable or a function/method call }
    _IDENTIFIER:
    Begin
     if (Scanner.next_t <> _DOUBLE_COLON) Then
     Begin
      // built-in constants
      Case VarToStr(Token.Value) of
       'true' : Exit(TBooleanExpressionNode.Create(Compiler.getExpressionCompiler, Token, True));
       'false': Exit(TBooleanExpressionNode.Create(Compiler.getExpressionCompiler, Token, False));
      End;
     End;

     // read symbol
     Symbol := ReadSymbol(NamespaceName, IdentifierName, mNamespace);

     // check if it's valid
     if (Symbol = nil) Then
     Begin
      Compiler.CompileError(Token, eUnknownIdentifier, [IdentifierName]);
     End Else
     Begin
      if (not (Symbol.Typ in [stVariable, stConstant, stFunction])) Then
       Compiler.CompileError(Token, eExpectedValue, [Symbol.Name]);
     End;

     // create appropriate node
     Result := TIdentifierExpressionNode.Create(Compiler.getExpressionCompiler, Token, IdentifierName, Symbol);
    End;

    { cast }
    _CAST: // cast<symbol-name or type-decl>(expression)
    Begin
     Scanner.eat(_LOWER);
     mType := Scanner.read_type;
     Scanner.eat(_GREATER);

     Scanner.eat(_BRACKET1_OP);
     Result := TCastExpressionNode.Create(Compiler.getExpressionCompiler, Token, Parse([_BRACKET1_CL]), mType);
    End;

    { opening parenthesis }
    _BRACKET1_OP:
    Begin
     Result := Parse([_BRACKET1_CL]);
    End;

    { unexpected symbol }
    else
     Compiler.CompileError(eExpectedValue, [Token.Value]);
   End;

   // parse optional 'modifiers'
   ParseModifiers:

   // function call
   if (Scanner.next_t = _BRACKET1_OP) Then
   Begin
    Scanner.read;

    Result := TCallExpressionNode.Create(Compiler.getExpressionCompiler, Scanner.next(-2), Result);

    // empty parameter list
    if (Scanner.next_t = _BRACKET1_CL) Then
    Begin
     Scanner.read;
    End Else

    // read parameter list
    Begin
     While (Scanner.next_t <> _BRACKET1_CL) Do
     Begin
      // add argument
      TCallExpressionNode(Result).AddArgument(Parse([_COMMA, _BRACKET1_CL]));
      Dec(Scanner.TokenPos);

      // check
      if (Scanner.next_t = _BRACKET1_CL) Then
      Begin
       Scanner.eat(_BRACKET1_CL); // `)` ends function call
       Break;
      End Else
      Begin
       Scanner.eat(_COMMA); // arguments are separated by comma
      End;
     End;
    End;
   End;

   // field fetch
   While (Scanner.next_t = _POINT) Do
   Begin
    // eat '.'
    Token := Scanner.read;

    // field name
    IdentifierName := Scanner.read_ident;

    // do a field fetch
    Result := TFieldFetchExpressionNode.Create(Compiler.getExpressionCompiler, Token, Result, IdentifierName);
   End;

   // method call
   While (Scanner.next_t = _BRACKET1_OP) Do
   Begin
    // do call
    Result := TCallExpressionNode.Create(Compiler.getExpressionCompiler, Token, Result);

    // eat '('
    Scanner.eat(_BRACKET1_OP);

    // empty argument list
    if (Scanner.next_t = _BRACKET1_CL) Then
    Begin
     Scanner.eat(_BRACKET1_CL);
    End Else

    // parse argument list
    Begin
     While (true) Do
     Begin
      TCallExpressionNode(Result).AddArgument(Parse([_COMMA, _BRACKET1_CL]));

      if (Scanner.next_t(-1) = _BRACKET1_CL) Then
       Break;
     End;
    End;
   End;

   // array element fetch
   While (Scanner.next_t = _BRACKET2_OP) Do
   Begin
    Scanner.read; // `[`
    Result := TArrayElementExpressionNode.Create(Compiler.getExpressionCompiler, Scanner.next(-1), Result, Parse([_BRACKET2_CL]));
   End;

   if (Scanner.next_t in [_BRACKET1_OP, _POINT, _BRACKET2_OP]) Then
   Begin
    First.Token := Scanner.next_t(-1);
    Token       := First;

    goto ParseModifiers;
   End;
  End;

  {$DEFINE PARAMETER_LIST1 := Compiler.getExpressionCompiler, LoopToken}
  {$DEFINE PARAMETER_LIST2 := Compiler.getExpressionCompiler, LoopToken, Result}

  { ParseOrder8 }
  Function ParseOrder8: TExpressionNode; //  unary '-'   |   ^   &   !   new
  Var Token: TToken_P;
      next : TToken;

      Op: TBitwiseMathExpressionNodeKind;

      UnspecifiedDimensions: uint8 = 0;
      mType                : TType;
      NewNode              : TNewExpressionNode;
      TmpNode, PrevNode    : TBinaryExpressionNode;
  Begin
   next := Scanner.next_t;

   TmpNode  := nil;
   PrevNode := nil;

   // "new" operator
   if (next = _NEW) Then
   Begin
    LoopToken := Scanner.read; // `new`
    mType     := Scanner.read_type(False); // read type

    // create node
    NewNode := TNewExpressionNode.Create(PARAMETER_LIST1, nil, mType);
    Result  := NewNode;

    // read dimension sizes
    While (true) Do
    Begin
     // eat '['
     Token := Scanner.eat(_BRACKET2_OP);

     // there can exist no more than one unspecified dimension
     if (UnspecifiedDimensions > 0) Then
      Compiler.CompileError(Token, eUnexpected, [Token.Value]);

     // "[]"
     if (Scanner.next_t = _BRACKET2_CL) Then
     Begin
      LoopToken := Scanner.read;

      // "new type[]" is wrong, and so is "new type[10][][]" (doubly unspecified)
      if (NewNode.getChild = nil) or
         (UnspecifiedDimensions > 0) Then
      Begin
       Compiler.CompileError(LoopToken, eExpectedValue, [']']);
      End;

      // create node
      PrevNode := TmpNode;
      TmpNode  := TArrayElementExpressionNode.Create(PARAMETER_LIST1, nil, nil);

      if (NewNode.getChild = nil) Then
       NewNode.setChild(TmpNode) Else
       PrevNode.setLeft(TmpNode);

      // increase counter
      Inc(UnspecifiedDimensions);
     End Else

     // "[size]"
     Begin
      PrevNode := TmpNode;
      TmpNode  := TArrayElementExpressionNode.Create(PARAMETER_LIST1, nil, Parse([_BRACKET2_CL]));

      if (NewNode.getChild = nil) Then
       NewNode.setChild(TmpNode) Else
       PrevNode.setLeft(TmpNode);
     End;

     // move further
     if (Scanner.next_t = _BRACKET2_OP) Then
      Continue Else
      Break;
    End;

    Exit;
   End;

   // unary '-'
   if (next = _MINUS) Then
   Begin
    Token  := Scanner.read;
    Result := TNegExpressionNode.Create(Compiler.getExpressionCompiler, Token, ParseOrder8());
    Exit;
   End;

   // unary '!'
   if (next = _EXCLM_MARK) Then
   Begin
    Token  := Scanner.read;
    Result := TLogicalNotExpressionNode.Create(Compiler.getExpressionCompiler, Token, ParseOrder8());
    Exit;
   End;

   // unary '~'
   if (next = _TILDE) Then
   Begin
    Token  := Scanner.read;
    Result := TBitwiseNotExpressionNode.Create(Compiler.getExpressionCompiler, Token, ParseOrder8());
    Exit;
   End;

   // other operators
   Result := ParseOrder9;

   While (WhileCondition(Order8)) Do
   Begin
    Case LoopToken.Token of
     _PIPE     : Op := beOr;
     _CARON    : Op := beXor;
     _AMPERSAND: Op := beAnd;
    End;

    Result := TBitwiseMathExpressionNode.Create(PARAMETER_LIST2, ParseOrder9, Op);
   End;
  End;

  { ParseOrder7 }
  Function ParseOrder7: TExpressionNode; //  ++   --
  Var Op: TPrePostOpExpressionNodeKind;
  Begin
   // pre operator (++x)
   if (Scanner.next_t in [_DOUBLE_PLUS, _DOUBLE_MINUS]) Then
   Begin
    // read token
    LoopToken := Scanner.read;

    // fetch operator
    Case LoopToken.Token of
     _DOUBLE_PLUS : Op := pePreInc;
     _DOUBLE_MINUS: Op := pePreDec;
    End;

    // create node
    Result := TPrePostOpExpressionNode.Create(PARAMETER_LIST1, ParseOrder9, Op);
    Exit;
   End;

   // either some other expression or a post operator, we'll find out
   Result := ParseOrder8();

   // a post operator
   if (Scanner.next_t in [_DOUBLE_PLUS, _DOUBLE_MINUS]) Then
   Begin
    // read token
    LoopToken := Scanner.read;

    // fetch operator
    Case LoopToken.Token of
     _DOUBLE_PLUS : Op := pePostInc;
     _DOUBLE_MINUS: Op := pePostDec;
    End;

    // create node
    Result := TPrePostOpExpressionNode.Create(PARAMETER_LIST1, Result, Op);
   End;
  End;

  { ParseOrder6 }
  Function ParseOrder6: TExpressionNode; //  *   /   %
  Var Op: TMathExpressionNodeKind;
  Begin
   Result := ParseOrder7;

   While (WhileCondition(Order6)) Do
   Begin
    Case LoopToken.Token of
     _STAR   : Op := meMul;
     _SLASH  : Op := meDiv;
     _PERCENT: Op := meMod;
    End;

    Result := TMathExpressionNode.Create(PARAMETER_LIST2, ParseOrder7, Op);
   End;
  End;

  { ParseOrder5 }
  Function ParseOrder5: TExpressionNode; //  +   -
  Var Op: TMathExpressionNodeKind;
  Begin
   Result := ParseOrder6;

   While (WhileCondition(Order5)) Do
   Begin
    Case LoopToken.Token of
     _PLUS : Op := meAdd;
     _MINUS: Op := meSub;
    End;

    Result := TMathExpressionNode.Create(PARAMETER_LIST2, ParseOrder6, Op);
   End;
  End;

  { ParseOrder4 }
  Function ParseOrder4: TExpressionNode; // <<   >>
  Var Op: TBitwiseMathExpressionNodeKind;
  Begin
   Result := ParseOrder5();

   While (WhileCondition(Order4)) Do
   Begin
    Case LoopToken.Token of
     _DOUBLE_LOWER  : Op := beShl;
     _DOUBLE_GREATER: Op := beShr;
    End;

    Result := TBitwiseMathExpressionNode.Create(PARAMETER_LIST2, ParseOrder5, Op);
   End;
  End;

  { ParseOrder3 }
  Function ParseOrder3: TExpressionNode; //  <   <=   ==   >=   >   !=
  Var Op: TComprasionExpressionNodeKind;
  Begin
   Result := ParseOrder4();

   While (WhileCondition(Order3)) Do
   Begin
    Case LoopToken.Token of
     _LOWER        : Op := ceLower;
     _LOWER_EQUAL  : Op := ceLowerEqual;
     _EQUAL_EQUAL  : Op := ceEqual;
     _GREATER_EQUAL: Op := ceGreaterEqual;
     _GREATER      : Op := ceGreater;
     _DIFFERENT    : Op := ceDifferent;
    End;

    Result := TComprasionExpressionNode.Create(PARAMETER_LIST2, ParseOrder4, Op);
   End;
  End;

  { ParseOrder2 }
  Function ParseOrder2: TExpressionNode; //  ||   &&
  Var Op: TLogicalMathExpressionNodeKind;
  Begin
   Result := ParseOrder3();

   While (WhileCondition(Order2)) Do
   Begin
    Case LoopToken.Token of
     _DOUBLE_PIPE     : Op := leOr;
     _DOUBLE_AMPERSAND: Op := leAnd;
    End;

    Result := TLogicalMathExpressionNode.Create(PARAMETER_LIST2, ParseOrder3, Op);
   End;
  End;

  { ParseOrder1 }
  Function ParseOrder1: TExpressionNode; //  =   +=   -=   /=   *=   %=   <<=   >>=   |=   &=   ^=
  Var Node: TExpressionNode;
      Op  : TAssignExpressionNodeKind;
  Begin
   Result := ParseOrder2();

   While (WhileCondition(Order1)) Do
   Begin
    Case LoopToken.Token of
     _EQUAL            : Op := aeAssign;
     _PLUS_EQUAL       : Op := aeAddAssign;
     _MINUS_EQUAL      : Op := aeSubAssign;
     _SLASH_EQUAL      : Op := aeDivAssign;
     _STAR_EQUAL       : Op := aeMulAssign;
     _PERCENT_EQUAL    : Op := aeModAssign;
     _DBL_LOWER_EQUAL  : Op := aeShlAssign;
     _DBL_GREATER_EQUAL: Op := aeShrAssign;
     _PIPE_EQUAL       : Op := aeOrAssign;
     _AMPERSAND_EQUAL  : Op := aeAndAssign;
     _CARON_EQUAL      : Op := aeXorAssign;
    End;

    if (Op = aeAssign) Then // we're doing this so that "x = y = z;" gets parsed as "x = (y = z);", not "(x = y) = z";
     Node := ParseOrder1() Else
     Node := ParseOrder2();

    Result := TAssignExpressionNode.Create(PARAMETER_LIST2, Node, Op);
   End;
  End;

 {$UNDEF PARAMETER_LIST1}
 {$UNDEF PARAMETER_LIST2}

Begin
 Scanner := Compiler.getScanner;
 Result  := ParseOrder1();

 if (not (Scanner.next_t in EndTokens)) Then // if finished parsing too early
 Begin
  Compiler.CompileError(Scanner.next, eExpectedValue, [Scanner.next.Value]);
 End;

 Scanner.read;
End;
End.
