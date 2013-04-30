(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)

{.$DEFINE DISPLAY_TREE}

Unit ExpressionCompiler;

 Interface
 Uses Compile1, MTypes, symdef, Tokens, Variants, TypInfo;

 Const STACK_SIZE = 5000; // internal compiler stack size (in elements)

 Const UnaryOperations  = ['!'];
       BinaryOperations = ['+', '-', '*', '/', '%', '='];

       _UNARY_MINUS = #01;

 Type TOption = (oGetFromCommandLine, oInsertConstants, oConstantFolding, oDisplayParseErrors);
      TOptions = Set of TOption;

 { TStackValue }
 Type TStackValue = Record // value on the stack
                     Typ  : TMExpressionType;
                     Value: Variant;
                     Token: TToken_P;

                     ParamCount : Integer; // if `Typ == stFunction`, here's hold read parameters count
                     NamespaceID: Integer; // function calls, variables and constants only
                    End;

 { TInterpreter }
 Type TInterpreter = Class
                      Private
                       Compiler: TCompiler;

                       Stack   : Array of TStackValue; // whole stack
                       StackPos: Integer;

                       FinalExpr   : Array of TStackValue; // final expression
                       FinalExprPos: Integer;

                       Procedure StackPush(Val: TStackValue);
                       Procedure StackPush(fTyp: TMExpressionType; fValue: Variant; fToken: TToken_P);
                       Procedure StackPush(fTyp: TMExpressionType; fValue: Variant);
                       Procedure StackPush(Typ: TMExpressionType; Token: TToken_P);
                       Function StackPop: TStackValue;
                       Function StackPeek: TStackValue;

                       Procedure FinalExprPush(Val: TStackValue);
                       Procedure FinalExprPush(Typ: TMExpressionType; Value: Variant; Token: TToken_P; NamespaceID: Integer=-1);
                       Function FinalExprPop: TStackValue;
                       Function FinalExprPeek: TStackValue;

                       Function CreateNode(Left, Right: PMExpression; Typ: TMExpressionType; Value: Variant; Token: TToken_P; NamespaceID: Integer=-1): PMExpression;

                      Public
                       Constructor Create(fCompiler: TCompiler);

                       Procedure Parse(EndTokens: TTokenSet);
                       Function MakeTree: PMExpression;
                       Function Optimize(const Tree: PMExpression; Options: TOptions): PMExpression;
                      End;

 Function EmptyExpression: PMExpression;

 Function MakeStringExpression(const Value: String): PMExpression;
 Function MakeIntExpression(const Value: Integer): PMExpression;
 Function MakeIntExpression(const Value: String): PMExpression;
 Function MakeFloatExpression(const Value: Extended): PMExpression;
 Function getValueFromExpression(const Expr: PMExpression; Beautify: Boolean=False): String;

 Function MakeConstruction(const CompilerPnt: Pointer; EndTokens: TTokenSet=[_SEMICOLON]; Options: TOptions=[oGetFromCommandLine]): TMConstruction;
 Function CompileConstruction(const CompilerPnt: Pointer; Expr: PMExpression): TType;

 Implementation
Uses SysUtils,
     CompilerUnit, Opcodes, Messages;

{ EmptyExpression }
Function EmptyExpression: PMExpression;
Begin
 New(Result);

 With Result^ do
 Begin
  Left  := nil;
  Right := nil;

  Typ   := mtNothing;
  Value := 0;
 End;
End;

{ MakeStringExpression }
Function MakeStringExpression(const Value: String): PMExpression;
Begin
 Result := EmptyExpression;

 Result^.Typ   := mtString;
 Result^.Value := Value;
End;

{ MakeIntExpression }
Function MakeIntExpression(const Value: Integer): PMExpression;
Begin
 Result := EmptyExpression;

 Result^.Typ   := mtInt;
 Result^.Value := Value;
End;

{ MakeIntExpression }
Function MakeIntExpression(const Value: String): PMExpression;
Begin
 Result := EmptyExpression;

 Result^.Typ   := mtInt;
 Result^.Value := Value;
End;

{ MakeFloatExpression }
Function MakeFloatExpression(const Value: Extended): PMExpression;
Begin
 Result := EmptyExpression;

 Result^.Typ   := mtFloat;
 Result^.Value := Value;
End;

{ getValueFromExpression }
Function getValueFromExpression(const Expr: PMExpression; Beautify: Boolean=False): String;
Var Value: String;
Begin
 Value := VarToStr(Expr^.Value);

 if (Beautify) Then
 Begin
  Case Expr^.Typ of
   mtChar  : Exit('''\0x'+IntToHex(ord(Value[1]), 2)+'''');
   mtString: Exit('"'+Value+'"');
  End;
 End;

 Case Expr^.Typ of
  mtChar  : Exit('#'+IntToStr(ord(Value[1])));
  mtString: Exit('"'+Value+'"');
 End;

 Exit(Value);
End;

{ DisplayTree }
Procedure DisplayTree(Tree: PMExpression; Deep: Byte=0);

  // Writeln
  Procedure Writeln(const Text: String);
  Var I: Integer;
  Begin
   For I := 0 To Deep-1 Do
    Write('-');
   System.Writeln('> ', Text);
  End;

Var I: Integer;
Begin
 if (Tree = nil) Then
 Begin
  Writeln('nil');
  Exit;
 End;

 if (Tree^.Value = null) Then
  Writeln('Type = '+MExpressionDisplay[Tree^.Typ]+' | Value = [null]') Else
  Writeln('Type = '+MExpressionDisplay[Tree^.Typ]+' | Value = '+VarToStr(Tree^.Value));

 Writeln('^L:');
 DisplayTree(Tree^.Left, Deep+5);

 Writeln('^R:');
 DisplayTree(Tree^.Right, Deep+5);

 Writeln('^P:');
 For I := Low(Tree^.ParamList) To High(Tree^.ParamList) Do
 Begin
  Writeln('arg ['+IntToStr(I)+']:');
  DisplayTree(Tree^.ParamList[I], Deep+5);
  Writeln('');
 End;
End;

{ getOrder }
Function getOrder(E: String): Integer;
Const Order: Array[0..7] of Array[0..7] of String =
(
 // operators' precedence (from the most important to the less)
 (_UNARY_MINUS, '@', '&', '|', '^', '!', 'new', 'delete'),
 ('++', '--', '', '', '', '', '', ''),
 ('*', '/', '%', '', '', '', '', ''),
 ('+', '-', '', '', '', '', '', ''),
 ('||', '&&', '', '', '', '', '', ''),
 ('<', '<=', '==', '>=', '>', '!=', '', ''),
 ('<<', '>>', '', '', '', '', '', ''),
 ('=', '+=', '-=', '/=', '*=', '%=', '<<=', '>>=')
);
Var I, Q: Integer;
Begin
 Result := -1;

 if (Length(E) = 0) Then
  Exit;

 For I := Low(Order) To High(Order) Do
  For Q := Low(Order[I]) To High(Order[I]) Do
   if (Order[I][Q] = E) Then
    Exit(High(Order)-I);
End;

{ getOrder }
Function getOrder(E: TStackValue): Integer;
Begin
 if (E.Typ in MOperators) Then
  Result := getOrder(MExpressionDisplay[E.Typ]) Else
  Result := -1;
End;

{ isRightAssoc }
Function isRightAssoc(X: String): Boolean;
Begin
 Result := (X[1] in ['!', '=']) or (X = '++') or (X = '--') or (X = 'new');
End;

{ isLeftAssoc }
Function isLeftAssoc(X: String): Boolean;
Begin
 Result := ((X[1] in BinaryOperations) or (X = '<<') or (X = '>>') or (X = '==') or (X = '!=') or (X = '>=') or (X = '<=') or (X = '<') or (X = '>')
                                       or (X = '+=') or (X = '-=') or (X = '*=') or (X = '/=') or (X = '%=') or (X = '&&') or (X = '||')
                                       and (not isRightAssoc(X)));
End;

// ---------- TInterpreter ---------- //

{ TInterpreter.StackPush }
Procedure TInterpreter.StackPush(Val: TStackValue);
Begin
 Stack[StackPos] := Val;
 Inc(StackPos);
End;

{ TInterpreter.StackPush }
Procedure TInterpreter.StackPush(fTyp: TMExpressionType; fValue: Variant; fToken: TToken_P);
Begin
 With Stack[StackPos] do
 Begin
  Typ   := fTyp;
  Value := fValue;
  Token := fToken;
 End;
 Inc(StackPos);
End;

{ TInterpreter.StackPush }
Procedure TInterpreter.StackPush(fTyp: TMExpressionType; fValue: Variant);
Begin
 With Stack[StackPos] do
 Begin
  Typ   := fTyp;
  Value := fValue;
 End;
 Inc(StackPos);
End;

{ TInterpreter.StackPush }
Procedure TInterpreter.StackPush(Typ: TMExpressionType; Token: TToken_P);
Begin
 StackPush(Typ, null, Token);
End;

{ TInterpreter.StackPop }
Function TInterpreter.StackPop: TStackValue;
Begin
 if (StackPos <= 0) Then
  Compiler.CompileError(eInvalidExpression, []);

 Dec(StackPos);
 Result := Stack[StackPos];
End;

{ TInterpreter.StackPeek }
Function TInterpreter.StackPeek: TStackValue;
Begin
 if (StackPos <= 0) Then
  Compiler.CompileError(eInvalidExpression, []);

 Result := Stack[StackPos-1];
End;

{ TInterpreter.FinalExprPush }
Procedure TInterpreter.FinalExprPush(Val: TStackValue);
Begin
 FinalExpr[FinalExprPos] := Val;
 Inc(FinalExprPos);
End;

{ TInterpreter.FinalExprPush }
Procedure TInterpreter.FinalExprPush(Typ: TMExpressionType; Value: Variant; Token: TToken_P; NamespaceID: Integer=-1);
Var Val: TStackValue;
Begin
 Val.Typ         := Typ;
 Val.Value       := Value;
 Val.Token       := Token;
 Val.NamespaceID := NamespaceID;
 FinalExprPush(Val);
End;

{ TInterpreter.FinalExprPop }
Function TInterpreter.FinalExprPop: TStackValue;
Begin
 if (FinalExprPos <= 0) Then
  Compiler.CompileError(eInvalidExpression, []);

 Dec(FinalExprPos);
 Result := FinalExpr[FinalExprPos];
End;

{ TInterpreter.FinalExprPeek }
Function TInterpreter.FinalExprPeek: TStackValue;
Begin
 if (FinalExprPos <= 0) Then
  Compiler.CompileError(eInvalidExpression, []);

 Result := FinalExpr[FinalExprPos-1];
End;

{ TInterpreter.CreateNode }
Function TInterpreter.CreateNode(Left, Right: PMExpression; Typ: TMExpressionType; Value: Variant; Token: TToken_P; NamespaceID: Integer=-1): PMExpression;
Var I: Integer;
Begin
 New(Result);

 if (NamespaceID = -1) Then
  Result^.Namespaces := Compiler.SelectedNamespaces Else
  Begin
   SetLength(Result^.Namespaces, 1);
   Result^.Namespaces[0] := NamespaceID;
  End;

 Result^.Left           := Left;
 Result^.Right          := Right;
 Result^.Typ            := Typ;
 Result^.Value          := Value;
 Result^.Token          := Token;
 Result^.IdentID        := -1;
 Result^.IdentNamespace := -1;
 Result^.isLocal        := False;

 Result^.ResultOnStack := False;
End;

{ TInterpreter.Create }
Constructor TInterpreter.Create(fCompiler: TCompiler);
Begin
 Compiler := fCompiler;

 SetLength(Stack, STACK_SIZE);
 StackPos := 0;

 SetLength(FinalExpr, STACK_SIZE);
 FinalExprPos := 0;
End;

{ TInterpreter.Parse }
Procedure TInterpreter.Parse(EndTokens: TTokenSet);
Var Token: TToken_P;

    Str   : String;
    Value : TStackValue;
    TypeID: TType;

    Bracket        : Integer=0;
    Bracket2       : Integer=0;
    FunctionBracket: Integer=0;
    Expect         : (eValueOrOperator, eValue, eOperator);

    Parsed: Boolean;

    NamespaceID, PreviousNamespace: Integer;

    OperatorNew: Boolean = False;

  // ReadParamCount
  Function ReadParamCount: Integer;
  Var TmpPos, Bracket: Integer;
      Token          : TToken_P;
  Begin
   Result  := 1;
   TmpPos  := Compiler.Parser.getPosition;
   Bracket := 0; { bracket level/deep }

   if (Compiler.Parser.next_t(0) = _BRACKET1_CL) Then { func() }
    Exit(0);

   With Compiler.Parser do
    Dec(TokenPos);

   While (true) Do
   Begin
    Token := Compiler.Parser.read;
    Case Token.Token of
     _BRACKET1_OP: Inc(Bracket);
     _BRACKET1_CL: Begin
                    Dec(Bracket);
                    if (Bracket = 0) Then
                     Break;
                   End;
     _COMMA: if (Bracket = 1) Then
              Inc(Result);

     _SEMICOLON, _BRACKET3_OP, _BRACKET3_CL: Compiler.CompileError(eExpected, [')', Token.Display]);
    End;
   End;

   Compiler.Parser.TokenPos := TmpPos;
  End;

  // is_a_post_operator
  Function is_a_post_operator: Boolean;
  Var Pos, BracketDeep: Integer;
  Begin
   Result := False;
   Pos    := -2;

   With Compiler.Parser do
    Case next_t(Pos) of
     _IDENTIFIER : Exit(True);
     _BRACKET2_CL:
     Begin
      BracketDeep := 0;

      Repeat
       if (-Pos > getPosition) Then // Magic. Do not touch.
        Exit(False);

       Case next_t(Pos) of
        _BRACKET2_OP: Inc(BracketDeep);
        _BRACKET2_CL: Dec(BracketDeep);
       End;
       Dec(Pos);
      Until (BracketDeep = 0);

      Exit(next_t(Pos) = _IDENTIFIER);
     End;
   End;
  End;

{ function's body }
Begin
With Compiler, Parser do
Begin
 StackPos          := 0;
 FinalExprPos      := 0;
 FunctionBracket   := 0;
 NamespaceID       := -1;
 PreviousNamespace := -1;
 Expect            := eValue;

 While (true) do
 Begin
  Token := read;
  Str   := Token.Display;

  if (Token.Token in EndTokens) and (Bracket = 0) Then
  Begin
   Case Expect of
    eValue: CompileError(eExpectedValue, [Token.Display]);
   End;

   Break;
  End;

  if (Token.Token in [_SEMICOLON, _BRACKET3_OP, _BRACKET3_CL]) Then
   if (Bracket = 0) Then
    Compiler.CompileError(eUnexpected, [Token.Display]) Else
    Compiler.CompileError(eExpected, [')', Token.Display]);

  Parsed := True;
  Case Token.Token of
   { values }
   _INT   : FinalExprPush(mtInt, StrToInt64(Str), Token);
   _FLOAT : FinalExprPush(mtFloat, StrToFloat(Str), Token);
   _STRING: FinalExprPush(mtString, Str, Token);
   _CHAR  : FinalExprPush(mtChar, Str[1], Token);

   _BREAK, _CONTINUE: Compiler.CompileError(eNotAllowed, [Token.Display]);

   else Parsed := False;
  End;

  { constant value }
  if (Token.Token in [_INT, _FLOAT, _STRING, _CHAR]) Then
  Begin
   if (Expect = eOperator) Then
    Compiler.CompileError(eExpectedOperator, [Token.Display]);

   Expect := eOperator;
  End;

  if (Parsed) Then
   Continue;

  { type casting }
  if (Token.Token = _CAST) Then
  Begin
   eat(_LOWER);
   TypeID := read_type;
   eat(_GREATER);

   if (next_t <> _BRACKET1_OP) Then
   Begin
    read;
    CompileError(eExpected, ['(', next(-1).Display]);
   End;

   StackPush(mtTypeCast, LongWord(TypeID), Token);

   Expect := eValue;
  End Else

  { namespace operator }
  if (Token.Token = _IDENTIFIER) and (next_t = _DOUBLE_COLON) Then
  Begin
   PreviousNamespace := NamespaceID;
   NamespaceID       := findNamespace(Token.Display);

   if (NamespaceID = -1) Then
   Begin
    CompileError(eUnknownNamespace, [Token.Display]);
    NamespaceID := 0;
   End;

   eat(_DOUBLE_COLON);

   if (next_t <> _IDENTIFIER) Then
    CompileError(eExpectedIdentifier, [next.Display]);

   if (next_t(1) = _DOUBLE_COLON) Then // namespace-in-namespace isn't supported for now
   Begin
    read;
    CompileError(eUnimplemented, ['namespace-in-namespace']);
   End;
  End Else

  { function or method call }
  if (next_t(-2) in [_IDENTIFIER, _BRACKET1_CL, _BRACKET2_CL]) and (Token.Token = _BRACKET1_OP) Then
  Begin
   Inc(Bracket);

   if (FunctionBracket = 0) Then
    FunctionBracket := Bracket;

   if (next_t(-3) = _POINT) Then
   Begin
    StackPush(mtMethodCall, Token.Display, Token); // method call
   End Else
    StackPush(mtFunctionCall, Token.Display, Token); // function call

   Stack[StackPos-1].ParamCount := ReadParamCount;

   Expect := eValue;
   StackPush(mtOpeningBracket, null, Token);
  End Else

  { method call }
  if (next_t(-2) in [_IDENTIFIER, _BRACKET1_CL, _BRACKET2_CL]) and (Token.Token = _POINT) Then
  Begin
   // methods are parsed in `if` above
   Expect := eValue;
  End Else

  { function parameters separator (comma) }
  if (Token.Token = _COMMA) Then
  Begin
   if (Expect = eValue) Then
    Compiler.CompileError(eExpectedValue, [Token.Display]);
   if (FunctionBracket = 0) Then
    Compiler.CompileError(eExpectedOperator, [Token.Display]);

   Expect := eValue;

   While (StackPos > 0) and (StackPeek.Typ <> mtOpeningBracket) do
    FinalExprPush(StackPop);
  End Else

  { variable or constant }
  if (Token.Token = _IDENTIFIER) Then
  Begin
   if (Expect = eOperator) Then
    Compiler.CompileError(eExpectedOperator, [Token.Display]);

   Expect := eOperator;
   FinalExprPush(mtVariable, Token.Display, Token, NamespaceID); // add it directly to the final expression

   NamespaceID := PreviousNamespace;
  End Else

  { opening bracket }
  if (Token.Token = _BRACKET1_OP) Then
  Begin
   Inc(Bracket);

   if ((Expect = eOperator) and (next_t(-2) <> _IDENTIFIER)) Then
    Compiler.CompileError(eExpectedOperator, ['(']);

   Expect := eValue;
   StackPush(mtOpeningBracket, null, Token);
  End Else

  { closing bracket }
  if (Token.Token = _BRACKET1_CL) Then
  Begin
   if (Bracket = FunctionBracket) Then
    FunctionBracket := 0;

   Dec(Bracket);
   if (Bracket < 0) Then
    Compiler.CompileError(next, eUnexpected, [')']);

   if (next_t(-2) = _BRACKET1_OP) Then // construction `()` is valid only when calling a function or method
   Begin
    if not (Stack[StackPos-2].Typ in [mtFunctionCall, mtMethodCall]) Then
     Compiler.CompileError(eExpectedValue, [')']);
   End Else
    if (Expect = eValue) Then
     Compiler.CompileError(eExpectedValue, [')']);

   Expect := eOperator;

   While (StackPos > 0) do
   Begin
    Value := StackPop;
    if (Value.Typ = mtOpeningBracket) Then
     Break;
    FinalExprPush(Value);
   End;
   if (StackPos > 0) Then
    if (StackPeek.Typ in [mtFunctionCall, mtMethodCall]) Then
     FinalExprPush(StackPop);
  End Else

  { array bracket open }
  if (Token.Token = _BRACKET2_OP) Then
  Begin
   Inc(Bracket2);

   Expect := eValue;
   StackPush(mtOpeningBracket2, null, Token);

   if (OperatorNew) Then
   Begin
    StackPush(mtNothing, null, Token);
    OperatorNew := False;
   End;
  End Else

  { array bracket close }
  if (Token.Token = _BRACKET2_CL) Then
  Begin
   Dec(Bracket2);
   if (Bracket2 < 0) Then
    Compiler.CompileError(eUnexpected, [']']);

   if (Expect = eValue) Then
    Compiler.CompileError(eExpectedValue, [']']);

   Expect := eValueOrOperator;

   While (StackPos > 0) do
   Begin
    Value := StackPop;
    if (Value.Typ = mtOpeningBracket2) Then
     Break;
    FinalExprPush(Value);
   End;

   FinalExprPush(mtArrayElement, null, Token);
  End Else

  { operator }
  Begin
   if (Token.Token = _MINUS) Then // is it unary or binary minus?
   Begin
    if not (next_t(-2) in [_INT, _FLOAT, _STRING, _CHAR, _IDENTIFIER, _BRACKET1_CL, _BRACKET2_CL]) Then
     Str := _UNARY_MINUS;
   End;

   if (Expect = eValue) and (Str <> _UNARY_MINUS) and not (Token.Token in [_EXCLM_MARK, _TILDE, _DOUBLE_PLUS, _DOUBLE_MINUS, _NEW]) Then
    Compiler.CompileError(eExpectedValue, [Token.Display]);

   Expect := eValue;

   if (StackPos > 0) Then // is there any element on the list?
   Begin
    While (isLeftAssoc(Str) and (getOrder(Str) <= getOrder(StackPeek))) or
          (isRightAssoc(Str) and (getOrder(Str) < getOrder(StackPeek))) Do
    Begin
     FinalExprPush(StackPop);
     if (StackPos <= 0) Then // no more elements on the list
      Break;
    End;
   End;

   Case Token.Token of // translate token into the operator
    _PLUS   : StackPush(mtAdd, Token); // +
    _MINUS  : if (Str = _UNARY_MINUS) Then StackPush(mtNeg, Token) Else StackPush(mtSub, Token); // -
    _STAR   : StackPush(mtMul, Token); // *
    _SLASH  : StackPush(mtDiv, Token); // /
    _PERCENT: StackPush(mtMod, Token); // %
    _EQUAL  : StackPush(mtAssign, Token); // =

    _PLUS_EQUAL   : StackPush(mtAddEq, Token); // +=
    _MINUS_EQUAL  : StackPush(mtSubEq, Token); // -=
    _STAR_EQUAL   : StackPush(mtMulEq, Token); // *=
    _SLASH_EQUAL  : StackPush(mtDivEq, Token); // /=
    _PERCENT_EQUAL: StackPush(mtModEq, Token); // %=

    _EXCLM_MARK: StackPush(mtLogicalNOT, Token); // !
    _TILDE     : StackPush(mtBitwiseNOT, Token); // ~

    _DOUBLE_PLUS: // ++
    if (is_a_post_operator) Then
     StackPush(mtPostInc, Token) Else
     StackPush(mtPreInc, Token);

    _DOUBLE_MINUS: // --
    if (is_a_post_operator) Then
     StackPush(mtPostDec, Token) Else
     StackPush(mtPreDec, Token);

    _LOWER        : StackPush(mtLower, Token); // <
    _GREATER      : StackPush(mtGreater, Token); // >
    _EQUAL_EQUAL  : StackPush(mtEqual, Token); // ==
    _LOWER_EQUAL  : StackPush(mtLowerEqual, Token); // <=
    _GREATER_EQUAL: StackPush(mtGreaterEqual, Token); // >=
    _DIFFERENT    : StackPush(mtDifferent, Token); // !=

    _PIPE            : StackPush(mtBitwiseOR, Token); // |
    _DOUBLE_PIPE     : StackPush(mtLogicalOR, Token); // ||
    _AMPERSAND       : StackPush(mtBitwiseAND, Token); // &
    _DOUBLE_AMPERSAND: StackPush(mtLogicalAND, Token); // &&
    _CARON           : StackPush(mtXOR, Token); // ^

    _DOUBLE_LOWER: // <<
    if (next_t = _EQUAL) Then
    Begin
     read;
     StackPush(mtSHLEq, Token);
    End Else
     StackPush(mtSHL, Token);

    _DOUBLE_GREATER: // >>
    if (next_t = _EQUAL) Then
    Begin
     read;
     StackPush(mtSHREq, Token);
    End Else
     StackPush(mtSHR, Token);

    _NEW: // `new`
    Begin
     StackPush(mtNew, Token);

     TypeID := read_type(False); // read type
     StackPush(mtType, LongWord(TypeID), Token);

     if (next_t <> _BRACKET2_OP) Then // fast syntax-check
     Begin
      read;
      Compiler.CompileError(eExpected, ['[', next(-1).Display]);
     End;

     OperatorNew := True;
    End;

    { invalid operator }
    else
     Case Expect of
      eOperator: Compiler.CompileError(eExpectedOperator, [Token.Display]);
      eValue   : Compiler.CompileError(eExpectedValue, [Token.Display]);
      else Compiler.CompileError(eUnexpected, [Token.Display]);
     End;
   End;

   if (Stack[StackPos-1].Typ in [mtPostInc, mtPostDec]) Then
    Expect := eOperator;
  End;
 End;

 // and add remaining operators to the final operator list
 While (StackPos > 0) Do
  FinalExprPush(StackPop);

 if (FinalExprPos = 0) Then
  FinalExpr[0].Token.Token := noToken;
End;
End;

{ TInterpreter.MakeTree }
Function TInterpreter.MakeTree: PMExpression;
Var Pos, I: Integer;
    Value : TStackValue;
    MType : TMExpressionType;
    Node  : PMExpression;

    ID, Namespace: Integer;

{ CreateNodeFromStack }
Function CreateNodeFromStack: PMExpression;
Var Value: TStackValue;

    Expr : PMExpression;
    Tmp  : PMExpression;

    Name      : String;
    Namespaces: TMIntegerArray;
Begin
 Value := StackPop;

 { whole expression tree }
 if (Value.Typ = mtTree) Then
 Begin
  if (Value.Value = null) Then
   Compiler.CompileError(eInternalError, ['Value.Value = null']);

  Expr := PMExpression(LongWord(Value.Value));

  if (Expr^.Typ = mtFunctionCall) Then
  Begin
   Tmp := Expr^.Left;

   if (Tmp^.Typ = mtVariable) Then // calling an identifier
   Begin
    Name       := Tmp^.Value;
    Namespaces := Tmp^.Namespaces;

    if (Name <> '') Then
    Begin
     ID := Compiler.findLocalVariable(Name); // local things at first

     if (ID = -1) Then // not a local variable
     Begin
      Compiler.findGlobalCandidate(Name, Namespaces, ID, Namespace, @Expr^.Token);

      if (ID = -1) or not (Compiler.NamespaceList[Namespace].SymbolList[ID].Typ in [gsVariable, gsFunction]) Then // nor a global variable/function - so raise error
       Compiler.CompileError(Tmp^.Token, eUnknownFunction, [Name]) Else
      Begin // global variable
       Expr^.IdentID        := ID;
       Expr^.IdentNamespace := Namespace;
       Expr^.isLocal        := False;
      End;
     End Else // local variable
     Begin
      Expr^.IdentID        := ID;
      Expr^.IdentNamespace := Namespace;
      Expr^.isLocal        := True;
     End;
    End;
   End Else // calling not identifier (cast-call, like: `(cast<function<void>()>(somevar))()` )
   Begin
    Expr^.Value := 'cast-call';
   End;
  End;

  Exit(Expr);
 End;

 { something else (variable, some constant value etc.) }
 Result      := CreateNode(nil, nil, mtNothing, Value.Value, Value.Token, Value.NamespaceID);
 Result^.Typ := Value.Typ;

 { variable or constant }
 if (Value.Typ = mtVariable) Then
 Begin
  Result^.VarName := VarToStr(Result^.Value);
  ID              := Compiler.findLocalVariable(Result^.Value); // local things at first

  if (ID = -1) Then // not a local variable
  Begin
   Compiler.findGlobalCandidate(Result^.Value, Result^.Namespaces, ID, Namespace, @Result^.Token);

   if (ID <> -1) and (Compiler.NamespaceList[Namespace].SymbolList[ID].Typ in [gsVariable, gsConstant, gsFunction]) Then // global variable
   Begin
    Result^.IdentID        := ID;
    Result^.IdentNamespace := Namespace;
    Result^.isLocal        := False;
   End;
  End Else // local variable
  Begin
   Result^.IdentID        := ID;
   Result^.IdentNamespace := Namespace;
   Result^.isLocal        := True;
  End;
 End;
End;

Begin
 Pos      := 0;
 StackPos := 0;

 if (FinalExpr[0].Token.Token = noToken) Then
  Exit(nil);

 Repeat
  Value := FinalExpr[Pos];

  { constant value }
  if (Value.Typ in [mtNothing, mtType, mtBool, mtChar, mtInt, mtFloat, mtString]) Then
  Begin
   StackPush(Value);
  End Else

  { variable }
  if (Value.Typ = mtVariable) Then
  Begin
   if (Value.Value = 'true') Then // internal constants
   Begin
    Value.Typ   := mtBool;
    Value.Value := True;
   End Else
   if (Value.Value = 'false') Then
   Begin
    Value.Typ   := mtBool;
    Value.Value := False;
   End;

   StackPush(Value);
  End Else

  { type cast }
  if (Value.Typ = mtTypeCast) Then
  Begin
   Node := CreateNode(nil, nil, mtTypeCast, Value.Value, Value.Token, Value.NamespaceID);

   Node^.Left := CreateNodeFromStack;

   StackPush(mtTree, LongWord(Node));
  End Else

  { function call }
  if (Value.Typ = mtFunctionCall) Then
  Begin
   Node := CreateNode(nil, nil, mtFunctionCall, Value.Value, Value.Token, Value.NamespaceID);

   SetLength(Node^.ParamList, Value.ParamCount);
   For I := Low(Node^.ParamList) To High(Node^.ParamList) Do
    Node^.ParamList[I] := CreateNodeFromStack;

   Node^.Left := CreateNodeFromStack;

   StackPush(mtTree, LongWord(Node));
  End Else

  { method call }
  if (Value.Typ = mtMethodCall) Then
  Begin
   Node := CreateNode(nil, nil, mtMethodCall, Value.Value, Value.Token, Value.NamespaceID);

   SetLength(Node^.ParamList, Value.ParamCount);
   For I := Low(Node^.ParamList) To High(Node^.ParamList) Do
    Node^.ParamList[I] := CreateNodeFromStack;

   Node^.Right := CreateNodeFromStack;
   Node^.Left  := CreateNodeFromStack;

   StackPush(mtTree, LongWord(Node));
  End Else

  { operator }
  if (Value.Typ in MOperators) Then
  Begin
   // binary operators
   if (Value.Typ in MBinaryOperators) Then
   Begin
    MType := Value.Typ;
    Node  := CreateNode(nil, nil, MType, null, Value.Token);

    Node^.Right := CreateNodeFromStack;
    Node^.Left  := CreateNodeFromStack;

    StackPush(mtTree, LongWord(Node));
   End Else

   // unary operators
   if (Value.Typ in MUnaryOperators) Then
   Begin
    MType := Value.Typ;
    Node  := CreateNode(nil, nil, MType, null, Value.Token);

    Node^.Left := CreateNodeFromStack;
    StackPush(mtTree, LongWord(Node));
   End Else

   // `new` operator
   if (Value.Typ = mtNew) Then
   Begin
    Node := CreateNode(nil, nil, mtNew, null, Value.Token);

    Node^.Left  := CreateNodeFromStack;
    Node^.Right := CreateNodeFromStack;

    StackPush(mtTree, LongWord(Node));
   End Else

   // array element
   if (Value.Typ = mtArrayElement) Then
   Begin
    Node := CreateNode(nil, nil, mtArrayElement, null, Value.Token);

    Node^.Right := CreateNodeFromStack;
    Node^.Left  := CreateNodeFromStack;

    StackPush(mtTree, LongWord(Node));
   End;
  End;

  Inc(Pos);
 Until (Pos >= FinalExprPos);

 if (StackPos <> 1) Then
  Compiler.CompileError(eInvalidExpression, []) Else
  Result := CreateNodeFromStack;
End;

{ TInterpreter.Optimize }
Function TInterpreter.Optimize(const Tree: PMExpression; Options: TOptions): PMExpression;

{$I insert_constants.pas}
{$I constant_folding.pas}

Begin
 if (oInsertConstants in Options) Then
  __insert_constants(oDisplayParseErrors in Options);

 if (oConstantFolding in Options) Then
  __constant_folding(oDisplayParseErrors in Options);

 Exit(Tree);
End;

// ---------- </> ---------- //

{ MakeConstruction }
Function MakeConstruction(const CompilerPnt: Pointer; EndTokens: TTokenSet=[_SEMICOLON]; Options: TOptions=[oGetFromCommandLine]): TMConstruction;
Var Compiler   : TCompiler absolute CompilerPnt;
    Interpreter: TInterpreter;
Begin
 Interpreter := TInterpreter(Compiler.Interpreter);

 if (oGetFromCommandLine in Options) Then
 Begin
  Options -= [oGetFromCommandLine];

  if (Compiler.getBoolOption(opt__constant_folding)) Then
   Options += [oInsertConstants, oConstantFolding];
 End;

 Interpreter.Parse(EndTokens);

 Result.Typ := ctExpression;
 SetLength(Result.Values, 1);
 Result.Values[0] := Interpreter.Optimize(Interpreter.MakeTree, Options);

 {$IFDEF DISPLAY_TREE}
  DisplayTree(Result.Values[0]);
 {$ENDIF}
End;

{ getDisplay }
Function getDisplay(Expr: PMExpression): String;
Begin
 if (Expr^.Value = null) Then
  Result := MExpressionDisplay[Expr^.Typ] Else

 if (Expr^.VarName <> '') Then
  Result := Expr^.VarName Else

  Result := Expr^.Value;
End;

// TRVariable
Type TRVariable = Record
                   Name   : String;
                   ID     : Integer;
                   MemPos : Integer;
                   RegChar: Char;
                   Typ    : TType;
                   PosStr : String;
                   Value  : PMExpression;

                   getArray: Byte;

                   isConst: Boolean;

                   mVariable: TVariable;
                  End;

{ CompileConstruction }
Function CompileConstruction(const CompilerPnt: Pointer; Expr: PMExpression): TType;
Var Compiler    : TCompiler;
    PushedValues: Integer=0;

{ Parse }
Function Parse(Expr: PMExpression; FinalRegID: Integer=0; FinalRegChar: Char=#0; const isSubCall: Boolean=True): TType;
Var Right, Left: PMExpression;
    Push_IF_reg: Boolean=False;

{ Error }
Procedure Error(Error: TCompileError; Args: Array of Const);
Begin
 Compiler.CompileError(Expr^.Token, Error, Args);
End;

{ Error }
Procedure Error(Token: TToken_P; Error: TCompileError; Args: Array of Const);
Begin
 Compiler.CompileError(Token, Error, Args);
End;

{ Hint }
Procedure Hint(Hint: TCompileHint; Args: Array of Const);
Begin
 Compiler.CompileHint(Expr^.Token, Hint, Args);
End;

{ RePop }
Procedure RePop(Expr: PMExpression; TypeID: TType; Reg: Byte);
Begin
 if (Expr^.ResultOnStack) Then
 Begin
  if not (Reg in [1..4]) Then
   Error(eInternalError, ['RePop called with invalid register ID: '+IntToStr(Reg)]);

  Compiler.PutOpcode(o_pop, ['e'+TypeID.RegPrefix+IntToStr(Reg)]);
  Expr^.ResultOnStack := False;
  Dec(PushedValues);
 End;
End;

{ getVariable }
Function getVariable(Expr: PMExpression; const FailWhenNotFound: Boolean=False; const AllowConstants: Boolean=False): TRVariable;
Label Failed;
Begin
 { set default values }
 With Result do
 Begin
  MemPos    := 0;
  RegChar   := #0;
  Typ       := nil;
  isConst   := False;
  PosStr    := '[0]';
  mVariable := nil;
 End;

 Result.getArray := 0;

 { is it an array element? }
 While (Expr^.Typ = mtArrayElement) do
 Begin
  Expr := Expr^.Left;
  Inc(Result.getArray);
 End;

 Result.Name := Expr^.VarName;
 Result.ID   := Expr^.IdentID;

 if (Result.ID = -1) Then // variable or constant not found
  goto Failed;

 if (Expr^.isLocal) Then
 Begin
  { local variable or constant }
  With Compiler.getCurrentFunction, Result do
  Begin
   mVariable := SymbolList[ID].mVariable;
   MemPos    := mVariable.MemPos;
   Typ       := mVariable.Typ;
   RegChar   := mVariable.Typ.RegPrefix;
   Value     := mVariable.Value;
   isConst   := mVariable.isConst;

   if (MemPos > 0) Then
    PosStr := 'e'+RegChar+IntToStr(MemPos) Else
    PosStr := '['+IntToStr(MemPos-PushedValues)+']';
  End;
 End Else
 Begin
  { global variable or constant }
  With Result do
  Begin
   mVariable := Compiler.NamespaceList[Expr^.IdentNamespace].SymbolList[Result.ID].mVariable;

   if (mVariable = nil) Then
    goto Failed;

   MemPos  := mVariable.MemPos;
   Typ     := mVariable.Typ;
   RegChar := mVariable.Typ.RegPrefix;
   Value   := mVariable.Value;
   isConst := mVariable.isConst;
  End;
 End;

Failed:
 if (Result.ID = -1) and (FailWhenNotFound) Then // var not found
 Begin
  Error(eUnknownVariable, [Result.Name]);
  Exit;
 End;

 if (Result.isConst) and (not AllowConstants) Then // not a constant
  Error(Expr^.Token, eLValueRequired, []);
End;

{ getType }
Function getType(Value: Variant): TType;
Begin
 if (Value = null) Then
 Begin
  DevLog('Warning: getType() -> Value = null; returned `nil`');
  Exit(nil);
 End;

 Result := TType(LongWord(Value));
End;

{ getTypeFromMExpr }
Function getTypeFromMExpr(Expr: PMExpression): TType;
Begin
 Result := TYPE_ANY;
 Case Expr^.Typ of
  mtBool    : Result := TYPE_BOOL;
  mtChar    : Result := TYPE_CHAR;
  mtInt     : Result := TYPE_INT;
  mtFloat   : Result := TYPE_FLOAT;
  mtString  : Result := TYPE_STRING;
  mtVariable: Result := getVariable(Expr).Typ;
 End;
End;

{ getValueFromMExpr }
Function getValueFromMExpr(Expr: PMExpression): String;
Begin
 if (Expr^.Value = null) Then
  Error(eInternalError, ['Expr^.Value = null']);

 Result := getValueFromExpression(Expr);
End;

{ isLValue }
Function isLValue(Expr: PMExpression): Boolean;
Begin
 Result := (Expr^.Typ in [mtVariable, mtArrayElement]);

 if (Expr^.Typ = mtVariable) and (Expr^.IdentID <> -1) Then // check if passed variable identifier isn't a constant
 Begin
  if (Expr^.isLocal) Then
   Exit(not Compiler.getCurrentFunction.SymbolList[Expr^.IdentID].mVariable.isConst) Else
   Exit(not Compiler.NamespaceList[Expr^.IdentNamespace].SymbolList[Expr^.IdentID].mVariable.isConst);
 End;
End;

{ countLeaves }
Function countLeaves(Expr: PMExpression): LongWord;
Var I: Integer;
Begin
 if (Expr = nil) Then
  Exit(0);

 With Expr^ do
 Begin
  Result := 1;

  if (Left <> nil) Then
   Result += countLeaves(Left);
  if (Right <> nil) Then
   Result += countLeaves(Right);

  For I := Low(ParamList) To High(ParamList) Do
   Result += countLeaves(ParamList[I]);

  if (Typ in [mtMethodCall, mtFunctionCall]) Then
   Inc(Result);
 End;
End;

{ variables }
{$I variable_handling.pas}

{ CompileSimple }
{$I compile_simple.pas}

{ ParseCompare }
// <   >   ==   <=   >=   !=
{$I compare.pas}

{ ParseArithmeticOperator }
// +   -   *   /   %   <<   >>
// +=  -=  *=  /=  %=  <<=  >>=
{$I arithmetic_operator.pas}

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
  if (Expr^.Typ = mtVariable) Then // if variable...
  Begin
   Variable := getVariable(Expr, False, True);

   if (Variable.ID = -1) Then // variable not found
   Begin
    { special variable: `__line` }
    if (Variable.Name = '__line') and (Compiler.getBoolOption(opt_internal_const)) Then
    Begin
     if (FinalRegChar = #0) Then
      FinalRegChar := 'i';

     if (FinalRegID > 0) Then // put into register?
      Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), Expr^.Token.Line]) Else
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
      Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), '"'+IntToStr(Expr^.Token.Line)+'"']) Else
      Begin // push onto stack?
       Compiler.PutOpcode(o_push, ['"'+IntToStr(Expr^.Token.Line)+'"']);
       Inc(PushedValues);
      End;

     Exit(TYPE_STRING);
    End Else

    { not a special variable - so var not found }
    Begin
     Error(eUnknownVariable, [Variable.Name]);
     Exit;
    End;
   End;

   if (FinalRegChar = #0) Then
    FinalRegChar := Variable.RegChar;

   if (FinalRegID > 0) Then
    __variable_getvalue_reg(Variable, FinalRegID, FinalRegChar) Else // load a variable's value into the register
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
    Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), getValueFromMExpr(Expr)]) Else // load a const value into the register
    Begin
     Compiler.PutOpcode(o_push, [getValueFromMExpr(Expr)]); // otherwise push onto the stack
     Expr^.ResultOnStack := True;
     Inc(PushedValues);
    End;
  End;
  Exit;
 End;

 // parse binary operators
 if (Expr^.Typ in MBinaryOperators) Then
 Begin
  if (Expr^.Typ in [mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent]) Then
   ParseCompare;

  Case Expr^.Typ of
   mtAdd, mtSub, mtMul, mtDiv, mtMod, mtSHL, mtSHR              : ParseArithmeticOperator(False);
   mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtSHLEq, mtSHREq: ParseArithmeticOperator(True);
   mtAssign    : ParseAssign;
   mtLogicalOR : ParseLogicalOR;
   mtLogicalAND: ParseLogicalAND;
   mtBitwiseOR : ParseBitwiseOR;
   mtBitwiseAND: ParseBitwiseAND;
   mtXOR       : ParseXOR;
  End;
 End;

 // parse unary operators
 if (Expr^.Typ in MUnaryOperators) Then
 Begin
  if (Expr^.Left = nil) Then
   Error(eInternalError, ['Expr^.Left = nil']);

  if (Expr^.Typ in [mtPreInc, mtPreDec, mtPostInc, mtPostDec]) Then // pre/post increment/decrement
  Begin
   ParsePIncDec;
   goto Over;
  End;

  Case Expr^.Typ of
   mtNeg: ParseNEG;
   mtLogicalNOT: ParseLogicalNOT;
   mtBitwiseNOT: ParseBitwiseNOT;
  End;
 End;

 // parse other operators
 Case Expr^.Typ of
  mtFunctionCall: ParseCall(False);
  mtMethodCall  : ParseCall(True);
  mtArrayElement: ParseArrayElement;
  mtNew         : ParseNEW;
  mtTypeCast    : ParseTypeCast;
 End;

Over:
 if (FinalRegID > 0) and (FinalRegChar <> #0) Then // load calculated value into the register?
 Begin
  if (Push_IF_reg) Then // special case
   Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), 'if']) Else
   Begin
    if (Result <> nil) Then
     Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), 'e'+Result.RegPrefix+'1']);
   End;
  Exit;
 End;

 if (Result = nil) Then
 Begin
  DevLog('Info: ExpressionCompiler :: Result = nil; assuming TYPE_ANY');
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
 Compiler := TCompiler(CompilerPnt);

 if (not Compiler.inFunction) Then
  Compiler.CompileError(Expr^.Token, eInternalError, ['not inFunction']);

 Result := Parse(Expr, 0, #0, False);
End;
End.
