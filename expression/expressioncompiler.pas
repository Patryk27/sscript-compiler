(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)

{.$DEFINE DISPLAY_TREE} // compiler debug only
{$MODESWITCH ADVANCEDRECORDS}
{$MACRO ON}

Unit ExpressionCompiler;

 Interface
 Uses SSCompiler, Expression, Scanner, symdef, Tokens, Variants, TypInfo;

 Type TOption = (oGetFromCommandLine, oInsertConstants, oConstantFolding, oTreeSimplification, oDisplayParseErrors);
      TOptions = Set of TOption;

      TShortCircuit = (scNone, scOR, scAND);

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

 Function OptimizeExpression(const Compiler: TCompiler; var Tree: PExpressionNode; const Options: TOptions): Boolean;

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

 Function MakeExpression(const CompilerPnt: Pointer; const EndTokens: TTokenSet=[_SEMICOLON]; Options: TOptions=[oGetFromCommandLine]): PExpressionNode;
 Function CompileExpression(const CompilerPnt: Pointer; const Expr: PExpressionNode): TType;

 Implementation
Uses SysUtils, Math,
     CompilerUnit, Opcodes, Messages;

(* OptimizeExpression *)
{
 Optimizes expression with given options.
 Returns `true` if anything has been optimized.
}
Function OptimizeExpression(const Compiler: TCompiler; var Tree: PExpressionNode; const Options: TOptions): Boolean;
Var AnyChange: Boolean = False;

Type TSimplify1Data = Record
                       Pre, Post: TExpressionNodeType;
                      End;

{ x = x op expr -> x op= expr }
Const Simplify1Data: Array[0..8] of TSimplify1Data =
(
 (Pre: mtAdd; Post: mtAddEq),
 (Pre: mtSub; Post: mtSubEq),
 (Pre: mtMul; Post: mtMulEq),
 (Pre: mtDiv; Post: mtDivEq),
 (Pre: mtShl; Post: mtShlEq),
 (Pre: mtShr; Post: mtShrEq),
 (Pre: mtBitwiseAND; Post: mtAndEq),
 (Pre: mtBitwiseOR; Post: mtOrEq),
 (Pre: mtXor; Post: mtXorEq)
);

{ x = expr op x -> x op= expr }
Const Simplify2Data: Array[0..4] of TSimplify1Data =
(
 (Pre: mtAdd; Post: mtAddEq),
 (Pre: mtMul; Post: mtMulEq),
 (Pre: mtBitwiseAND; Post: mtAndEq),
 (Pre: mtBitwiseOR; Post: mtOrEq),
 (Pre: mtXor; Post: mtXorEq)
);

{ !(boolA op boolB) -> (boolA not-op boolB) }
Const Simplify3Data: Array[0..5] of TSimplify1Data =
(
 (Pre: mtEqual; Post: mtDifferent),
 (Pre: mtDifferent; Post: mtEqual),
 (Pre: mtLower; Post: mtGreaterEqual),
 (Pre: mtGreater; Post: mtLowerEqual),
 (Pre: mtLowerEqual; Post: mtGreater),
 (Pre: mtGreaterEqual; Post: mtLower)
);

{$I insert_constants.pas}
{$I opt_constant_folding.pas}
{$I opt_tree_simplification.pas}

Begin
 if (oInsertConstants in Options) Then
  __insert_constants(oDisplayParseErrors in Options);

 if (oConstantFolding in Options) Then
  __constant_folding(oDisplayParseErrors in Options);

 if (oTreeSimplification in Options) Then
  __tree_simplification(oDisplayParseErrors in Options);

 Result := AnyChange;
End;

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

// ---------- TExpressionParser ---------- //

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

  // @TODO: "cast" operator

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
    Exit(CreateNode(ParseOrder9(), nil, mtNeg, null, Token));
   End;

   if (next = _EXCLM_MARK) Then // unary '!'
   Begin
    Token := Scanner.read;
    Exit(CreateNode(ParseOrder9(), nil, mtLogicalNOT, null, Token));
   End;

   if (next = _TILDE) Then // unary '~'
   Begin
    Token := Scanner.read;
    Exit(CreateNode(ParseOrder9(), nil, mtBitwiseNOT, null, Token));
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

// ---------- </> ---------- //

(* MakeExpression *)
Function MakeExpression(const CompilerPnt: Pointer; const EndTokens: TTokenSet=[_SEMICOLON]; Options: TOptions=[oGetFromCommandLine]): PExpressionNode;
Var Compiler: TCompiler absolute CompilerPnt;
    Parser  : TExpressionParser;
Begin
 Parser := TExpressionParser.Create(Compiler);

 Try
  if (oGetFromCommandLine in Options) Then
  Begin
   Options -= [oGetFromCommandLine];

   if (Compiler.getBoolOption(opt__constant_folding)) Then
    Options += [oInsertConstants, oConstantFolding];
  End;

  Result := Parser.Parse(EndTokens);

  {$IFDEF DISPLAY_TREE}
   DisplayTree(Result);
   Writeln;
  {$ENDIF}
 Finally
  Parser.Free;
 End;
End;

{ getDisplay }
Function getDisplay(Expr: PExpressionNode): String;
Begin
 if (Expr^.Value = null) Then
  Result := ExpressionNodeString[Expr^.Typ] Else

 if (Expr^.IdentName <> '') Then
  Result := Expr^.IdentName Else

  Result := Expr^.Value;
End;

// TRVariable
Type TRVariable =
     Record
      Private
       pPushedValues: PInteger;

      Public
       Name        : String;
       Symbol      : Pointer;
       LocationData: TVarLocationData;
       RegChar     : Char;
       Typ         : TType;
       Value       : PExpressionNode;

       getArray: Byte;

       isConst: Boolean;

       mVariable: TVariable;

       Function PosStr: String;
       Function isStoredInRegister: Boolean;
      End;

// TRVariable.PosStr
Function TRVariable.PosStr: String;
Begin
 Result := TSymbol(Symbol).mVariable.getAllocationPos(-pPushedValues^);
End;

// TRVariable.isStoredInRegister
Function TRVariable.isStoredInRegister: Boolean;
Begin
 Result := (LocationData.Location = vlRegister);
End;

(* CompileExpression *)
Function CompileExpression(const CompilerPnt: Pointer; const Expr: PExpressionNode): TType;
Var Compiler    : TCompiler; // caller compiler pointer
    ExprLabel   : String; // unique name for each expression
    PushedValues: Integer=0; // amount of values pushed onto stack; used in eg.getting value of variables lying on the stack

{ Parse }
Function Parse(Expr: PExpressionNode; FinalRegID: Integer=0; FinalRegChar: Char=#0; const isSubCall: Boolean=True): TType;
Var Left, Right : PExpressionNode; // left and right side of current expression
    Push_IF_reg : Boolean=False; // if equal `true`, the `if` register is pushed at the end of parsing `Expr`
    FinalRegDone: Boolean=False; // if equal `true`, no additional "mov(FinalReg, e_1)" opcode will be automatically put (if FinalRegID > 0)

  { Error }
  Procedure Error(const Error: TCompileError; const Args: Array of Const);
  Begin
   Compiler.CompileError(Expr^.Token, Error, Args);
  End;

  { Error }
  Procedure Error(const Token: TToken_P; const Error: TCompileError; const Args: Array of Const);
  Begin
   Compiler.CompileError(Token, Error, Args);
  End;

  { Hint }
  Procedure Hint(const Hint: TCompileHint; const Args: Array of Const);
  Begin
   Compiler.CompileHint(Expr^.Token, Hint, Args);
  End;

  { RePop }
  Procedure RePop(const Expr: PExpressionNode; const TypeID: TType; const Reg: Byte);
  Begin
   if (TypeID = nil) Then
    Exit;

   if (Expr^.ResultOnStack) Then
   Begin
    if (not (Reg in [1..4])) Then
     Error(eInternalError, ['RePop called with invalid register ID: '+IntToStr(Reg)]);

    Compiler.PutOpcode(o_pop, ['e'+TypeID.RegPrefix+IntToStr(Reg)]);
    Expr^.ResultOnStack := False;
    Dec(PushedValues);
   End;
  End;

  { getVariable }
  Function getVariable(Expr: PExpressionNode; const FailWhenNotFound: Boolean=False; const AllowConstants: Boolean=False): TRVariable;
  Label Failed;
  Begin
   { set default values }
   With Result do
   Begin
    pPushedValues := @PushedValues;
//    LocationData  := Default(LocationData); @TODO: FPC 2.7.0+
    RegChar       := #0;
    Typ           := nil;
    isConst       := False;
    mVariable     := nil;
   End;

   Result.getArray := 0;

   { is it an array element? }
   While (Expr^.Typ = mtArrayElement) do
   Begin
    Expr := Expr^.Left;
    Inc(Result.getArray);
   End;

   Result.Name   := Expr^.IdentName;
   Result.Symbol := Expr^.Symbol;

   if (Result.Symbol = nil) Then // variable or constant not found
    goto Failed;

   With Result do
   Begin
    mVariable    := TSymbol(Result.Symbol).mVariable;

    if (mVariable = nil) Then
     goto Failed;

    LocationData := mVariable.LocationData;
    Typ          := mVariable.Typ;
    RegChar      := mVariable.Typ.RegPrefix;
    Value        := mVariable.Value;
    isConst      := mVariable.isConst;
   End;

  Failed:
   if (Result.Symbol = nil) and (FailWhenNotFound) Then // var not found
   Begin
    Error(eUnknownVariable, [Result.Name]);
    Exit;
   End;

   if (Result.isConst) and (not AllowConstants) Then // not a constant
    Error(Expr^.Token, eLValueExpected, []);
  End;

  { getType }
  Function getType(const Value: Variant): TType; inline;
  Begin
   if (Value = null) Then
   Begin
    DevLog(dvWarning, 'getType', 'Value = null; returned `nil`');
    Exit(nil);
   End;

   Result := TType(uint32(Value));
  End;

  { getTypeFromMExpr }
  Function getTypeFromMExpr(const Expr: PExpressionNode): TType; inline;
  Begin
   Case Expr^.Typ of
    mtBool      : Result := TYPE_BOOL;
    mtChar      : Result := TYPE_CHAR;
    mtInt       : Result := TYPE_INT;
    mtFloat     : Result := TYPE_FLOAT;
    mtString    : Result := TYPE_STRING;
    mtIdentifier: Result := getVariable(Expr).Typ;

    else
     Result := TYPE_ANY;
   End;
  End;

  { isLValue }
  Function isLValue(const Expr: PExpressionNode): Boolean; inline;
  Begin
   Result := (Expr^.Typ in [mtIdentifier, mtArrayElement]);

   if (Expr^.Typ = mtIdentifier) and (Expr^.Symbol <> nil) Then // check if passed variable identifier isn't actually a constant
    Result := not TSymbol(Expr^.Symbol).mVariable.isConst;
  End;

  { countLeaves }
  Function countLeaves(const Expr: PExpressionNode): uint32; inline;
  Var Param: PExpressionNode;
  Begin
   if (Expr = nil) Then
    Exit(0);

   With Expr^ do
   Begin
    Result := 1 + countLeaves(Left) + countLeaves(Right);

    For Param in ParamList Do
     Result += countLeaves(Param);

    if (Typ in [mtMethodCall, mtFunctionCall]) Then
     Inc(Result);
   End;
  End;

{ variable handling }
{$I variable_handling.pas}

{ CompileSimple }
{$I compile_simple.pas}

{ ParseCompare }
// <   >   ==   <=   >=   !=
{$I compare.pas}

{ ParsePrimaryOperator }
// +   -   *   /   %   <<   >>
// +=  -=  *=  /=  %=  <<=  >>=
{$I primary_operator.pas}

{ ParseAssign }
// =
{$I assign.pas}

{ ParseLogicalOR }
// ||
{$I logical_or.pas}

{ ParseLogicalAND }
// &&
{$I logical_and.pas}

{ ParseBitwiseOR }
// |
{$I bitwise_or.pas}

{ ParseBitwiseAND }
// &
{$I bitwise_and.pas}

{ ParseXOR }
// ^
{$I xor.pas}

{ ParsePIncDec }
// ++
// --
{$I pre_post_inc_dec.pas}

{ ParseNEG }
// -
{$I neg.pas}

{ ParseLogicalNOT }
// !
{$I logical_not.pas}

{ ParseBitwiseNOT }
// ~
{$I bitwise_not.pas}

{ ParseCall }
// ()
{$I call.pas}

{ ParseArrayElement }
// []
{$I array_element.pas}

{ ParseNEW }
// new
{$I new.pas}

{ ParseTypeCast }
// cast<type>(value)
{$I typecast.pas}

Var Variable: TRVariable;
Label Over;
Begin
 if (Expr = nil) Then
  Exit(TYPE_VOID);

 Result := nil; //TYPE_VOID; // assuming `void` type
 Left   := Expr^.Left;
 Right  := Expr^.Right;

 if (Left = nil) and (Right = nil) Then // both child nodes are `nil`
 Begin
  // load value onto the register `FinalRegID`
  if (Expr^.Typ = mtIdentifier) Then // if variable...
  Begin
   Variable := getVariable(Expr, False, True);

   if (Variable.Symbol = nil) Then // variable not found
   Begin
    { special variable: `__line` }
    if (Variable.Name = '__line') and (Compiler.getBoolOption(opt_internal_const)) Then
    Begin
     if (FinalRegChar = #0) Then
      FinalRegChar := 'i';

     if (FinalRegID > 0) Then // put into register?
     Begin
      Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), Expr^.Token.Line]);
     End Else
     Begin // push onto stack?
      Compiler.PutOpcode(o_push, [Expr^.Token.Line]);
      Inc(PushedValues);
     End;

     Exit(TYPE_INT);
    End Else

    { special variable: `__linestr` }
    if (Variable.Name = '__linestr') and (Compiler.getBoolOption(opt_internal_const)) Then
    Begin
     if (FinalRegChar = #0) Then
      FinalRegChar := 's';

     if (FinalRegID > 0) Then // put into register?
     Begin
      Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), '"'+IntToStr(Expr^.Token.Line)+'"']);
     End Else
     Begin // push onto stack?
      Compiler.PutOpcode(o_push, ['"'+IntToStr(Expr^.Token.Line)+'"']);
      Inc(PushedValues);
     End;

     Exit(TYPE_STRING);
    End Else

    { not a special variable thus show "var not found" error }
    Begin
     Error(eUnknownVariable, [Variable.Name]);
     Exit;
    End;
   End;

   if (FinalRegChar = #0) Then
    FinalRegChar := Variable.RegChar;

   if (FinalRegID > 0) Then
   Begin
    __variable_getvalue_reg(Variable, FinalRegID, FinalRegChar); // load a variable's value to specified register
   End Else
   Begin
    __variable_getvalue_stack(Variable);
    Expr^.ResultOnStack := True;
   End;

   Result := Variable.Typ;

  // if (Compiler.isTypeFunctionPointer(Result)) and (not isSubCall) Then
  //  Hint(hDidntYouMean, [Variable.Name+'()']);
  {
   @TODO:
   function<function<void>()> something()
   {
    return another_function; // hint: didn't you mean (...)
   }
  }
  End Else
  Begin // if const value
   Result := getTypeFromMExpr(Expr);

   if (FinalRegChar = #0) Then
    FinalRegChar := Result.RegPrefix;

   if (FinalRegID > 0) Then
    Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), getValueFromExpression(Expr)]) Else // load a const value into the register
    Begin
     Compiler.PutOpcode(o_push, [getValueFromExpression(Expr)]); // otherwise push onto the stack
     Expr^.ResultOnStack := True;
     Inc(PushedValues);
    End;
  End;
  Exit;
 End;

 // parse operators
 Case Expr^.Typ of
  { binary operators }
  mtAdd, mtSub, mtMul, mtDiv, mtMod, mtSHL, mtSHR                                        : ParsePrimaryOperator(False);
  mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtSHLEq, mtSHREq, mtOREq, mtANDEq, mtXOREq: ParsePrimaryOperator(True);

  mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent: ParseCompare;

  mtAssign    : ParseAssign;
  mtLogicalOR : ParseLogicalOR;
  mtLogicalAND: ParseLogicalAND;
  mtBitwiseOR : ParseBitwiseOR;
  mtBitwiseAND: ParseBitwiseAND;
  mtXOR       : ParseXOR;

  { unary operators }
  mtNeg       : ParseNEG;
  mtLogicalNOT: ParseLogicalNOT;
  mtBitwiseNOT: ParseBitwiseNOT;

  mtPreInc, mtPreDec, mtPostInc, mtPostDec: ParsePIncDec;

  { other operators }
  mtFunctionCall: ParseCall(False);
  mtMethodCall  : ParseCall(True);
  mtArrayElement: ParseArrayElement;
  mtNew         : ParseNEW;
  mtTypeCast    : ParseTypeCast;
 End;

Over:
 if (FinalRegID > 0) and (FinalRegChar <> #0) Then // load calculated value into the register?
 Begin
  if (not FinalRegDone) Then
  Begin
   if (Push_IF_reg) Then // special case
   Begin
    Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), 'if']);
   End Else
   Begin
    if (Result <> nil) Then
     Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), 'e'+Result.RegPrefix+'1']);
   End;
  End;

  Exit;
 End;

 if (Result = nil) Then
 Begin
  DevLog(dvInfo, 'CompileExpression::Parse', 'Result = nil; assuming `TYPE_ANY`');
  Exit(TYPE_ANY);
 End;

 if (FinalRegChar = #0) Then
  FinalRegChar := Result.RegPrefix;

 if (FinalRegChar <> #0) Then
 Begin
  if (Push_IF_reg) Then
   Compiler.PutOpcode(o_push, ['if']) Else // when values are compared, the comparing result is in the 'if' register
   if (FinalRegChar in ['b', 'c', 'i', 'f', 's', 'r']) Then // is it valid FinalRegChar?
    Compiler.PutOpcode(o_push, ['e'+FinalRegChar+'1']); // save value onto the stack
  Expr^.ResultOnStack := True;

  Inc(PushedValues);
 End;
End;

Begin
 if (Expr = nil) Then
  Exit;

 Compiler := TCompiler(CompilerPnt);

 if (not Compiler.inFunction) Then
  Compiler.CompileError(Expr^.Token, eInternalError, ['CompileExpression() called outside any of compiled function.']);

 Compiler.PutComment(IntToStr(Expr^.Token.Line)+': '+ExpressionToString(Expr));

 ExprLabel := Compiler.getCurrentFunction.LabelName+'__expression_'+IntToStr(Compiler.LabelCounter);
 Inc(Compiler.LabelCounter);

 Result := Parse(Expr, 0, #0, False);
End;
End.
