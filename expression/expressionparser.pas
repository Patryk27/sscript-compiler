(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)

{.$DEFINE DISPLAY_TREE} // debug only
{$MODESWITCH ADVANCEDRECORDS}
{$MACRO ON}

Unit ExpressionParser;

 Interface
 Uses SysUtils, SSCompiler, Expression, Scanner, symdef, Tokens, Variants, TypInfo;

 { TExpressionParser }
 Type TExpressionParser =
      Class
       Private
        Compiler: TCompiler;

        Function CreateNode(const Left, Right: PExpressionNode; const Typ: TExpressionNodeType; const Value: Variant; const Token: TToken_P): PExpressionNode;

       Public
        Constructor Create(const fCompiler: TCompiler);

        Function Parse(const EndTokens: TTokenSet): PExpressionNode;
       End;

 Function EmptyExpression(const Token: PToken_P=nil): PExpressionNode;

 Function MakeBoolExpression(const Value: Boolean; const Token: PToken_P=nil): PExpressionNode;
 Function MakeIntExpression(const Value: Int64; const Token: PToken_P=nil): PExpressionNode;
 Function MakeIntExpression(const Value: String; const Token: PToken_P=nil): PExpressionNode;
 Function MakeFloatExpression(const Value: Extended; const Token: PToken_P=nil): PExpressionNode;
 Function MakeStringExpression(const Value: String; const Token: PToken_P=nil): PExpressionNode;

 Function getValueFromExpression(const Expr: PExpressionNode; const Beautify: Boolean=False): String;
 Function getTypeFromExpression(const Expr: PExpressionNode): TType;
 Function getExpressionTypeName(const Expr: PExpressionNode): String;
 Function ExpressionToString(const Expr: PExpressionNode): String;

 Implementation
Uses Messages;

(* EmptyExpression *)
Function EmptyExpression(const Token: PToken_P=nil): PExpressionNode;
Begin
 New(Result);

 With Result^ do
 Begin
  Left  := nil;
  Right := nil;

  Typ   := mtNothing;
  Value := 0;

  ResultOnStack := False;

  SetLength(SSA.Values, 0);
  SetLength(PostSSA.Values, 0);
 End;

 if (Token <> nil) Then
  Result^.Token := Token^;
End;

(* MakeBoolExpression *)
Function MakeBoolExpression(const Value: Boolean; const Token: PToken_P=nil): PExpressionNode;
Begin
 Result := EmptyExpression(Token);

 Result^.Typ   := mtBool;
 Result^.Value := Value;
End;

(* MakeIntExpression *)
Function MakeIntExpression(const Value: Int64; const Token: PToken_P=nil): PExpressionNode;
Begin
 Result := EmptyExpression(Token);

 Result^.Typ   := mtInt;
 Result^.Value := Value;
End;

(* MakeIntExpression *)
Function MakeIntExpression(const Value: String; const Token: PToken_P=nil): PExpressionNode;
Begin
 Result := EmptyExpression(Token);

 Result^.Typ   := mtInt;
 Result^.Value := Value;
End;

(* MakeFloatExpression *)
Function MakeFloatExpression(const Value: Extended; const Token: PToken_P=nil): PExpressionNode;
Begin
 Result := EmptyExpression(Token);

 Result^.Typ   := mtFloat;
 Result^.Value := Value;
End;

(* MakeStringExpression *)
Function MakeStringExpression(const Value: String; const Token: PToken_P=nil): PExpressionNode;
Begin
 Result := EmptyExpression(Token);

 Result^.Typ   := mtString;
 Result^.Value := Value;
End;

(* getValueFromExpression *)
Function getValueFromExpression(const Expr: PExpressionNode; const Beautify: Boolean=False): String;
Var Value: String;
Begin
 Value := VarToStr(Expr^.Value);

 if (Beautify) Then
 Begin
  Case Expr^.Typ of
   mtChar  : Exit('''\0x'+IntToHex(ord(Value[1]), 2)+'''');
   mtString: Exit('"'+Value+'"');
   mtBool  : if (Expr^.Value) Then
              Exit('true') Else
              Exit('false');
  End;
 End;

 Case Expr^.Typ of
  mtChar  : Exit('#'+IntToStr(ord(Value[1])));
  mtString: Exit('"'+Value+'"');
  mtBool  : if (Expr^.Value) Then
             Exit('true') Else
             Exit('false');
 End;

 Exit(Value);
End;

(* getTypeFromExpression *)
Function getTypeFromExpression(const Expr: PExpressionNode): TType;
Begin
 Case Expr^.Typ of
  mtBool  : Result := TYPE_BOOL;
  mtChar  : Result := TYPE_CHAR;
  mtInt   : Result := TYPE_INT;
  mtFloat : Result := TYPE_FLOAT;
  mtString: Result := TYPE_STRING;

  else
   Result := TYPE_ANY;
 End;
End;

(* getExpressionTypeName *)
Function getExpressionTypeName(const Expr: PExpressionNode): String;
Begin
 Case Expr^.Typ of
  mtBool  : Result := 'bool';
  mtChar  : Result := 'char';
  mtInt   : Result := 'int';
  mtFloat : Result := 'float';
  mtString: Result := 'string';
  else
   Result := 'erroneous type';
 End;
End;

(* ExpressionToString *)
Function ExpressionToString(const Expr: PExpressionNode): String;
Var I   : int32;
    L, R: String;
Begin
 if (Expr = nil) Then
  Exit('');

 if (Expr^.Typ = mtIdentifier) Then
 Begin
  Result := Expr^.IdentName;

  if (Length(Expr^.PostSSA.Values) > 0) Then
  Begin
   Result += '.(';

   For I := 0 To High(Expr^.PostSSA.Values) Do
    Result += IntToStr(Expr^.PostSSA.Values[I])+', ';

   Delete(Result, Length(Result)-1, 2);
   Result += ')';
  End;

  Result += '$';
  if (Length(Expr^.SSA.Values) = 0) Then
  Begin
   Result += 'unknown';
  End Else
  Begin
   Result += '(';
   For I := 0 To High(Expr^.SSA.Values) Do
    Result += IntToStr(Expr^.SSA.Values[I])+', ';

   Delete(Result, Length(Result)-1, 2);
   Result += ')';
  End;

  Exit;
 End;

 if (Expr^.Typ in [mtBool, mtChar, mtInt, mtFloat]) Then
  Exit(Expr^.Value);

 if (Expr^.Typ = mtString) Then
  Exit('"'+Expr^.Value+'"');

 { function call }
 if (Expr^.Typ = mtFunctionCall) Then
 Begin
  Result := Expr^.Left^.IdentName+'(';

  For I := Low(Expr^.ParamList) To High(Expr^.ParamList) Do
  Begin
   Result += ExpressionToString(Expr^.ParamList[I]);

   if (I <> High(Expr^.ParamList)) Then
    Result += ', ';
  End;

  Result += ')';
  Exit;
 End;

 { method call }
 // @TODO

 { array element }
 if (Expr^.Typ = mtArrayElement) Then
 Begin
  Result := ExpressionToString(Expr^.Left)+'['+ExpressionToString(Expr^.Right)+']';
  Exit;
 End;

 { type casting }
 if (Expr^.Typ = mtTypeCast) Then
 Begin
  Result := 'cast<'+TType(LongWord(Expr^.Value)).asString+'>('+ExpressionToString(Expr^.Left)+')';
  Exit;
 End;

 { unary minus }
 if (Expr^.Typ = mtNeg) Then
 Begin
  Result := '-'+ExpressionToString(Expr^.Left);
  Exit;
 End;

 L := ExpressionToString(Expr^.Left);
 R := ExpressionToString(Expr^.Right);

 if (Expr^.Typ in [mtMul, mtDiv, mtMod]) Then
 Begin
  L := '('+L+')';
  R := '('+R+')';
 End;

 Result := L+ExpressionNodeString[Expr^.Typ]+R;
End;

// -------------------------------------------------------------------------- //
{ DisplayTree }
Procedure DisplayTree(const Tree: PExpressionNode; const Deep: uint8=0);

  // Fill
  Procedure Fill;
  Var I: int8;
  Begin
   For I := 0 To int8(Deep)-1 Do
    Write(' ');
//   Write('> ');
  End;

Var I: Integer;
Begin
 Fill;

 if (Tree = nil) Then
 Begin
  Writeln('nil');
  Exit;
 End;

 Write('Type = ', ExpressionNodeString[Tree^.Typ], ' | ');

 if (Tree^.Typ = mtIdentifier) Then
  Writeln('Identifier: '+Tree^.IdentName) Else

 if (Tree^.Value = null) Then
  Writeln('Value = [null]') Else
  Writeln('Value = '+VarToStr(Tree^.Value));

 Fill;
 Writeln('^L:');
 DisplayTree(Tree^.Left, Deep+5);

 Fill;
 Writeln('^R:');
 DisplayTree(Tree^.Right, Deep+5);

 Fill;
 Writeln('^P:');
 For I := Low(Tree^.ParamList) To High(Tree^.ParamList) Do
 Begin
  Fill;
  Writeln('arg ['+IntToStr(I)+']:');
  DisplayTree(Tree^.ParamList[I], Deep+5);
  Writeln;
 End;
End;

// -------------------------------------------------------------------------- //
(* TExpressionParser.CreateNode *)
Function TExpressionParser.CreateNode(const Left, Right: PExpressionNode; const Typ: TExpressionNodeType; const Value: Variant; const Token: TToken_P): PExpressionNode;
Begin
 New(Result);

 Result^.Left          := Left;
 Result^.Right         := Right;
 Result^.Typ           := Typ;
 Result^.Value         := Value;
 Result^.Token         := Token;
 Result^.Symbol        := nil;
 Result^.IdentName     := '';
 Result^.IdentType     := mtNothing;
 Result^.IdentValue    := null;
 Result^.ResultOnStack := False;

 SetLength(Result^.SSA.Values, 0);
 SetLength(Result^.ParamList, 0);
End;

(* TExpressionParser.Create *)
Constructor TExpressionParser.Create(const fCompiler: TCompiler);
Begin
 Compiler := fCompiler;
End;

(* TExpressionParser.Parse *)
Function TExpressionParser.Parse(const EndTokens: TTokenSet): PExpressionNode;
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
     Exit;
    End;

    Exit(Namespace.findSymbol(IdentifierName)); // assume no multiple symbols with the same name exist in one namespace
   End Else
   Begin
    IdentifierName := Token.Value;
    Exit(Compiler.findCandidate(Token.Value, nil, Token));
   End;
  End;

  { ParseOrder9 }
  Function ParseOrder9: PExpressionNode; //  const-expr   group-expr   array-expr   function-call   cast-expr
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
     Result := CreateNode(nil, nil, mtChar, Token.Value, Token);
    End;

    { an integer constant }
    _INT:
    Begin
     Result := CreateNode(nil, nil, mtInt, Token.Value, Token);
    End;

    { a float constant }
    _FLOAT:
    Begin
     Result := CreateNode(nil, nil, mtFloat, Token.Value, Token);
    End;

    { a string constant }
    _STRING:
    Begin
     Result := CreateNode(nil, nil, mtString, Token.Value, Token);
    End;

    { a variable or a function/method call }
    _IDENTIFIER:
    Begin
     // @TODO: true/false
     if (Scanner.next_t <> _DOUBLE_COLON) Then
     Begin
      if (Token.Value = 'true') Then // "true" built-in constant
       Exit(CreateNode(nil, nil, mtBool, True, Token));

      if (Token.Value = 'false') Then // "false" built-in constant
       Exit(CreateNode(nil, nil, mtBool, False, Token));
     End;

     Symbol := ReadSymbol(NamespaceName, IdentifierName, mNamespace);

     if (Symbol = nil) Then
     Begin
      Compiler.CompileError(Token, eUnknownIdentifier, [IdentifierName]);
      Exit;
     End;

     // fetch symbol
     Result            := CreateNode(nil, nil, mtIdentifier, null, First);
     Result^.IdentName := IdentifierName;
     Result^.Symbol    := Symbol;
    End;

    { cast }
    _CAST: // cast<symbol-name | type-decl>(expression)
    Begin
     Scanner.eat(_LOWER);
     mType := Scanner.read_type;
     Scanner.eat(_GREATER);

     Scanner.eat(_BRACKET1_OP);
     Result := CreateNode(Parse([_BRACKET1_CL]), nil, mtTypeCast, uint32(mType), Token);
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

   // function/cast call
   if (Scanner.next_t = _BRACKET1_OP) Then
   Begin
    Scanner.read;

    if (First.Token = _IDENTIFIER) Then // function call -> foo()
    Begin
     Result         := CreateNode(Result, nil, mtFunctionCall, IdentifierName, First);
     Result^.Symbol := Result^.Left^.Symbol;

     Symbol := (Result^.Symbol as TSymbol);

     if (not (Symbol.Typ in [stFunction, stVariable])) Then
     Begin
      Compiler.CompileError(Result^.Token, eCannotBeCalled, [Symbol.Name]);
      Exit;
     End;
    End Else

    if (First.Token = _BRACKET2_CL) Then // array element call -> array[10]()
    Begin
     Result := CreateNode(Result, nil, mtFunctionCall, 'array-call', First);
    End Else

    Begin // cast-call -> cast<function>(expression)();
     Result := CreateNode(Result, nil, mtFunctionCall, 'cast-call', First);
    End;

    if (Scanner.next_t = _BRACKET1_CL) Then
    Begin
     Scanner.read;
    End Else
    Begin
     While (Scanner.next_t <> _BRACKET1_CL) Do
     Begin
      {$DEFINE PList := Result^.ParamList}
      SetLength(PList, Length(PList)+1);
      PList[High(PList)] := Parse([_COMMA, _BRACKET1_CL]);
      {$UNDEF PList}

      Dec(Scanner.TokenPos);

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

   // (linked with) method call? | @TODO (in future): method field fetch (xyz.foo)
   While (Scanner.next_t = _POINT) Do
   Begin
    Token := Scanner.read; // `.`

    IdentifierName := Scanner.read_ident;
    Result         := CreateNode
                      (
                       Result,
                       CreateNode(nil, nil, mtIdentifier, IdentifierName, Token),
                       mtMethodCall,
                       null,
                       Token
                      );

    Result^.Right^.IdentName := IdentifierName;

    Scanner.eat(_BRACKET1_OP);

    While (Scanner.next_t <> _BRACKET1_CL) Do
    Begin
     {$DEFINE PList := Result^.ParamList}
     SetLength(PList, Length(PList)+1);
     PList[High(PList)] := Parse([_COMMA, _BRACKET1_CL]);
     {$UNDEF PList}

     Dec(Scanner.TokenPos);
     if (Scanner.next_t = _BRACKET1_CL) Then
      Break Else
      Scanner.eat(_COMMA);
    End;

    Scanner.eat(_BRACKET1_CL);
   End;

   // array element fetch
   While (Scanner.next_t = _BRACKET2_OP) Do
   Begin
    Scanner.read; // `[`
    Result := CreateNode(Result, Parse([_BRACKET2_CL]), mtArrayElement, null, Token);
   End;

   if (Scanner.next_t in [_BRACKET1_OP, _POINT, _BRACKET2_OP]) Then
   Begin
    First.Token := Scanner.next_t(-1);
    Token       := First;

    goto ParseModifiers;
   End;
  End;

  { ParseOrder8 }
  Function ParseOrder8: PExpressionNode; //  unary '-'   |   ^   &   !   new
  Var Op   : TExpressionNodeType;
      mType: TType;
      next : TToken;
      Token: TToken_P;
      Tmp  : PExpressionNode;
  Begin
   next := Scanner.next_t;

   if (next = _NEW) Then // "new" operator
   Begin
    Token := Scanner.read; // `new`
    mType := Scanner.read_type(False); // read type

    Result := CreateNode
              (
               CreateNode(nil, nil, mtInt, uint32(mType), Token),
               nil,
               mtNew,
               null,
               Token
              );

    Tmp := Result;

    While (true) Do
    Begin
     Token      := Scanner.eat(_BRACKET2_OP);
     Tmp^.Right := CreateNode(Parse([_BRACKET2_CL]), nil, mtArrayElement, null, Token);
     Tmp        := Tmp^.Right;

     if (Scanner.next_t = _BRACKET2_OP) Then
      Continue Else
      Break;
    End;

    Exit;
   End;

   if (next = _MINUS) Then // unary '-'
   Begin
    Token := Scanner.read;
    Exit(CreateNode(ParseOrder8(), nil, mtNeg, null, Token));
   End;

   if (next = _EXCLM_MARK) Then // unary '!'
   Begin
    Token := Scanner.read;
    Exit(CreateNode(ParseOrder8(), nil, mtLogicalNOT, null, Token));
   End;

   if (next = _TILDE) Then // unary '~'
   Begin
    Token := Scanner.read;
    Exit(CreateNode(ParseOrder8(), nil, mtBitwiseNOT, null, Token));
   End;

   Result := ParseOrder9();

   While (WhileCondition(Order8)) Do
   Begin
    Case LoopToken.Token of
     _PIPE     : Op := mtBitwiseOR;
     _CARON    : Op := mtXOR;
     _AMPERSAND: Op := mtBitwiseAND;

     else
      Compiler.CompileError(eInternalError, ['ParseOrder8() -> unexpected token']);
    End;

    Result := CreateNode(Result, ParseOrder9(), Op, null, LoopToken);
   End;
  End;

  { ParseOrder7 }
  Function ParseOrder7: PExpressionNode; //  ++   --
  Var Op   : TExpressionNodeType;
      Token: TToken_P;
  Begin
   if (Scanner.next_t in [_DOUBLE_PLUS, _DOUBLE_MINUS]) Then // pre
   Begin
    LoopToken := Scanner.read;

    Case LoopToken.Token of
     _DOUBLE_PLUS : Op := mtPreInc;
     _DOUBLE_MINUS: Op := mtPreDec;
    End;

    Result := ParseOrder9(); // parse expr

    if (Result = nil) or (not (Result^.Typ in [mtIdentifier, mtArrayElement])) Then // error: not an l-value!
    Begin
     if (Result = nil) Then
      Token := Scanner.next(-1) Else
      Token := Result^.Token;

     Compiler.CompileError(Token, eLValueExpected, []);
     Exit;
    End;

    Result := CreateNode(Result, nil, Op, null, LoopToken);
   End Else
   Begin
    Result := ParseOrder8();

    if (Scanner.next_t in [_DOUBLE_PLUS, _DOUBLE_MINUS]) Then // post
    Begin
     LoopToken := Scanner.read;

     if (Result = nil) or (not (Result^.Typ in [mtIdentifier, mtArrayElement])) Then // error: not an l-value!
     Begin
      if (Result = nil) Then
       Token := Scanner.next(-1) Else
       Token := Result^.Token;

      Compiler.CompileError(Token, eLValueExpected, []);
      Exit;
     End;

     Case LoopToken.Token of
      _DOUBLE_PLUS : Op := mtPostInc;
      _DOUBLE_MINUS: Op := mtPostDec;
     End;

     Result := CreateNode(Result, nil, Op, null, LoopToken);
    End;
   End;
  End;

  { ParseOrder6 }
  Function ParseOrder6: PExpressionNode; //  *   /   %
  Var Op: TExpressionNodeType;
  Begin
   Result := ParseOrder7();

   While (WhileCondition(Order6)) Do
   Begin
    Case LoopToken.Token of
     _STAR   : Op := mtMul;
     _SLASH  : Op := mtDiv;
     _PERCENT: Op := mtMod;

     else
      Compiler.CompileError(eInternalError, ['ParseOrder6() -> unexpected token']);
    End;

    Result := CreateNode(Result, ParseOrder7(), Op, null, LoopToken);
   End;
  End;

  { ParseOrder5 }
  Function ParseOrder5: PExpressionNode; //  +   -
  Var Op: TExpressionNodeType;
  Begin
   Result := ParseOrder6();

   While (WhileCondition(Order5)) Do
   Begin
    Case LoopToken.Token of
     _PLUS : Op := mtAdd;
     _MINUS: Op := mtSub;

     else
      Compiler.CompileError(eInternalError, ['ParseOrder5() -> unexpected token']);
    End;

    Result := CreateNode(Result, ParseOrder6(), Op, null, LoopToken);
   End;
  End;

  { ParseOrder4 }
  Function ParseOrder4: PExpressionNode; // <<   >>
  Var Op: TExpressionNodeType;
  Begin
   Result := ParseOrder5();

   While (WhileCondition(Order4)) Do
   Begin
    Case LoopToken.Token of
     _DOUBLE_LOWER  : Op := mtSHL;
     _DOUBLE_GREATER: Op := mtSHR;

     else
      Compiler.CompileError(eInternalError, ['ParseOrder4() -> unexpected token']);
    End;

    Result := CreateNode(Result, ParseOrder5(), Op, null, LoopToken);
   End;
  End;

  { ParseOrder3 }
  Function ParseOrder3: PExpressionNode; //  <   <=   ==   >=   >   !=
  Var Op: TExpressionNodeType;
  Begin
   Result := ParseOrder4();

   While (WhileCondition(Order3)) Do
   Begin
    Case LoopToken.Token of
     _LOWER        : Op := mtLower;
     _LOWER_EQUAL  : Op := mtLowerEqual;
     _EQUAL_EQUAL  : Op := mtEqual;
     _GREATER_EQUAL: Op := mtGreaterEqual;
     _GREATER      : Op := mtGreater;
     _DIFFERENT    : Op := mtDifferent;

     else
      Compiler.CompileError(eInternalError, ['ParseOrder3() -> unexpected token']);
    End;

    Result := CreateNode(Result, ParseOrder4(), Op, null, LoopToken);
   End;
  End;

  { ParseOrder2 }
  Function ParseOrder2: PExpressionNode; //  ||   &&
  Var Op: TExpressionNodeType;
  Begin
   Result := ParseOrder3();

   While (WhileCondition(Order2)) Do
   Begin
    Case LoopToken.Token of
     _DOUBLE_PIPE     : Op := mtLogicalOR;
     _DOUBLE_AMPERSAND: Op := mtLogicalAND;

     else
      Compiler.CompileError(eInternalError, ['ParseOrder2() -> unexpected token']);
    End;

    Result := CreateNode(Result, ParseOrder3(), Op, null, LoopToken);
   End;
  End;

  { ParseOrder1 }
  Function ParseOrder1: PExpressionNode; //  =   +=   -=   /=   *=   %=   <<=   >>=   |=   &=   ^=
  Var Op: TExpressionNodeType;
  Begin
   Result := ParseOrder2();

   While (WhileCondition(Order1)) Do
   Begin
    Case LoopToken.Token of
     _EQUAL            : Op := mtAssign;
     _PLUS_EQUAL       : Op := mtAddEq;
     _MINUS_EQUAL      : Op := mtSubEq;
     _SLASH_EQUAL      : Op := mtDivEq;
     _STAR_EQUAL       : Op := mtMulEq;
     _PERCENT_EQUAL    : Op := mtModEq;
     _DBL_LOWER_EQUAL  : Op := mtSHLEq;
     _DBL_GREATER_EQUAL: Op := mtSHREq;
     _PIPE_EQUAL       : Op := mtOREq;
     _AMPERSAND_EQUAL  : Op := mtANDEq;
     _CARON_EQUAL      : Op := mtXOREq;

     else
      Compiler.CompileError(eInternalError, ['ParseOrder1() -> unexpected token']);
    End;

    Result := CreateNode(Result, ParseOrder2(), Op, null, LoopToken);
   End;
  End;

Begin
 Scanner := Compiler.getScanner;
 Result  := ParseOrder1();

 if (not (Scanner.next_t in EndTokens)) Then // if finished parsing too early
 Begin
  Compiler.CompileError(Scanner.next, eExpectedOperator, [Scanner.next.Value]);
 End;

 Scanner.read;
End;
End.
