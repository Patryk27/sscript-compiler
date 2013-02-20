(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)

{ ... and now the fun begins ;) }

{.$DEFINE DISPLAY_FINALEXPR} // debugging only
{.$DEFINE DISPLAY_TREE}      // see above

Unit ExpressionCompiler;

 Interface
 Uses Compile1, MTypes, Variants, Tokens;

 Const STACK_SIZE = 5000;

 Const UnaryOperations  = ['!'];
       BinaryOperations = ['+', '-', '*', '/', '%', '='];

       _UNARY_MINUS = #01;

 Type TOptions = Set of (oGetFromCommandLine, oConstantFolding);

 { TStackValue }
 Type TStackValue = Record // value on the stack
                     Typ  : TMExpressionType;
                     Value: Variant;
                     Token: TToken_P;
                     Deep : Integer;

                     ParamCount : Integer; // if `Typ == stFunction`, there's hold read param count
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

                       Function CreateNode(Left, Right: PMExpression; Typ: TMExpressionType; Value: Variant; Token: TToken_P; Deep: Integer; NamespaceID: Integer=-1): PMExpression;
                      Public
                       Constructor Create(fCompiler: TCompiler);

                       Procedure Parse(EndTokens: TTokenSet);
                       Function MakeTree: PMExpression;
                       Function Optimize(const Tree: PMExpression; Options: TOptions): PMExpression;
                      End;

 Function MakeStringExpression(const Value: String): PMExpression;
 Function MakeIntExpression(const Value: Integer): PMExpression;
 Function MakeIntExpression(const Value: String): PMExpression;
 Function MakeFloatExpression(const Value: Extended): PMExpression;
 Function getValueFromExpression(const Compiler: Pointer; Expr: PMExpression; Beautify: Boolean=False): String;

 Function MakeConstruction(const CompilerPnt: Pointer; EndTokens: TTokenSet=[_SEMICOLON]; Options: TOptions=[oGetFromCommandLine]): TMConstruction;
 Function CompileConstruction(const CompilerPnt: Pointer; Expr: PMExpression): TVType;

 Implementation
Uses SysUtils,
     CompilerUnit, Opcodes, Messages;

{ MakeStringExpression }
Function MakeStringExpression(const Value: String): PMExpression;
Begin
 New(Result);

 Result^.Typ   := mtString;
 Result^.Left  := nil;
 Result^.Right := nil;
 Result^.Value := Value;
End;

{ MakeIntExpression }
Function MakeIntExpression(const Value: Integer): PMExpression;
Begin
 New(Result);

 Result^.Typ   := mtInt;
 Result^.Left  := nil;
 Result^.Right := nil;
 Result^.Value := Value;
End;

{ MakeIntExpression }
Function MakeIntExpression(const Value: String): PMExpression;
Begin
 New(Result);

 Result^.Typ   := mtInt;
 Result^.Left  := nil;
 Result^.Right := nil;
 Result^.Value := Value;
End;

{ MakeFloatExpression }
Function MakeFloatExpression(const Value: Extended): PMExpression;
Begin
 New(Result);

 Result^.Typ   := mtFloat;
 Result^.Left  := nil;
 Result^.Right := nil;
 Result^.Value := Value;
End;

{ getValueFromExpression }
Function getValueFromExpression(const Compiler: Pointer; Expr: PMExpression; Beautify: Boolean=False): String;
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
  mtString: Exit(TCompiler(Compiler).AddString(Value));
 End;

 Exit(Value);
End;

{$IFDEF DISPLAY_TREE}
{ DisplayTree }
Procedure DisplayTree(Tree: PMExpression; Deep: Byte=0);

Procedure Align;
Var I: Integer;
Begin
 if (Deep = 0) Then
  Exit;
 For I := 0 To Deep-1 Do
  Write('-');
 Write('> ');
End;

Var I: Integer;
Begin
 Align;

 if (Tree = nil) Then
 Begin
  Writeln('nil');
  Exit;
 End;

 Write('Typ = ', Tree^.Typ, ' | ');
 if (Tree^.Value = null) Then
  Writeln('Value = [null]') Else
  Writeln('Value = ', Tree^.Value);

 Align;
 Writeln('^L:');
 DisplayTree(Tree^.Left, Deep+5);

 Align;
 Writeln('^R:');
 DisplayTree(Tree^.Right, Deep+5);

 Align;
 Writeln('^P:');
 For I := Low(Tree^.ParamList) To High(Tree^.ParamList) Do
 Begin
  Align;
  Writeln('[',I,']:');
  DisplayTree(Tree^.ParamList[I], Deep+5);
  Writeln;
 End;
End;
{$ENDIF}

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

{ isRightC }
Function isRightC(X: String): Boolean;
Begin
 Result := (X[1] in ['!', '=']) or (X = '++') or (X = '--') or (X = 'new');
End;

{ isLeftC }
Function isLeftC(X: String): Boolean;
Begin
 Result := ((X[1] in BinaryOperations) or (X = '<<') or (X = '>>') or (X = '==') or (X = '!=') or (X = '>=') or (X = '<=') or (X = '<') or (X = '>')
                                       or (X = '+=') or (X = '-=') or (X = '*=') or (X = '/=') or (X = '%=') or (X = '&&') or (X = '||')
                                       and (not isRightC(X)));
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
  Deep  := Compiler.CurrentDeep;
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
  Compiler.CompileError(eInternalError, ['StackPos <= 0']);
 Dec(StackPos);
 Result := Stack[StackPos];
End;

{ TInterpreter.StackPeek }
Function TInterpreter.StackPeek: TStackValue;
Begin
 if (StackPos <= 0) Then
  Compiler.CompileError(eInternalError, ['StackPos <= 0']);
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
 Val.Deep        := Compiler.CurrentDeep;
 Val.NamespaceID := NamespaceID;
 FinalExprPush(Val);
End;

{ TInterpreter.FinalExprPop }
Function TInterpreter.FinalExprPop: TStackValue;
Begin
 if (FinalExprPos <= 0) Then
  Compiler.CompileError(eInternalError, ['FinalExprPos <= 0']);
 Dec(FinalExprPos);
 Result := FinalExpr[FinalExprPos];
End;

{ TInterpreter.FinalExprPeek }
Function TInterpreter.FinalExprPeek: TStackValue;
Begin
 if (FinalExprPos <= 0) Then
  Compiler.CompileError(eInternalError, ['FinalExprPos <= 0']);
 Result := FinalExpr[FinalExprPos-1];
End;

{ TInterpreter.CreateNode }
Function TInterpreter.CreateNode(Left, Right: PMExpression; Typ: TMExpressionType; Value: Variant; Token: TToken_P; Deep: Integer; NamespaceID: Integer=-1): PMExpression;
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
 Result^.Deep           := Deep;
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
    TypeID: Integer;

    Bracket        : Integer=0;
    Bracket2       : Integer=0;
    FunctionBracket: Integer=0;
    Expect         : (eNothing, eValue, eOperator);

    Parsed: Boolean;

    NamespaceID, PreviousNamespace: Integer;

    {$IFDEF DISPLAY_FINALEXPR}
    I: Integer;
    {$ENDIF}

{ ReadParamCount }
Function ReadParamCount: Integer;
Var TmpPos, Bracket: Integer;
    Token          : TToken_P;
Begin
 Result  := 1;
 TmpPos  := Compiler.getPosition;
 Bracket := 0; { bracket level/deep }

 if (Compiler.next_t(0) = _BRACKET1_CL) Then { func() }
  Exit(0);

 With Compiler do
  setPosition(getPosition-1);

 While (true) Do
 Begin
  Token := Compiler.read;
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

 Compiler.setPosition(TmpPos);
End;

{ is_a_post_operator }
Function is_a_post_operator: Boolean;
Var Pos, BracketDeep: Integer;
Begin
 Result := False;
 Pos    := -2;

 With Compiler do
  Case next_t(Pos) of
   _IDENTIFIER : Exit(True);
   _BRACKET2_CL:
   Begin
    BracketDeep := 0;

    Repeat
     if (-Pos > getPosition) Then
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

Begin
With Compiler do
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
    eValue   : Compiler.CompileError(eExpectedValue, [Token.Display]);
   // eOperator: Compiler.CompileError(eExpectedOperator, [Token.Display]);
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
   _INTEGER: FinalExprPush(mtInt, StrToInt(Str), Token);
   _FLOAT  : FinalExprPush(mtFloat, StrToFloat(Str), Token);
   _STRING : FinalExprPush(mtString, Str, Token);
   _CHAR   : FinalExprPush(mtChar, Str[1], Token);

   _BREAK, _CONTINUE: Compiler.CompileError(eNotAllowed, [Token.Display]);

   else Parsed := False;
  End;

  if (Token.Token in [_INTEGER, _FLOAT, _STRING, _CHAR]) Then
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

   StackPush(mtTypeCast, TypeID, Token);

   Expect := eValue;
  End Else

  { namespace identifier }
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

  { function call }
  if (Token.Token = _BRACKET1_OP) and (next_t(-2) in [_IDENTIFIER, _BRACKET1_CL, _BRACKET2_CL]) Then
  Begin
   Inc(Bracket);

   if (FunctionBracket = 0) Then
    FunctionBracket := Bracket;

   StackPush(mtFunctionCall, Token.Display, Token); // push it onto the stack
   Stack[StackPos-1].ParamCount := ReadParamCount;

   Expect := eValue;
   StackPush(mtOpeningBracket, null, Token);
  End Else

  { function parameters separator (comma) }
  if (Token.Token = _COMMA) Then
  Begin
   if (Expect = eValue) Then
    Compiler.CompileError(eExpectedValue, [Token.Display]);
   if (FunctionBracket = 0) Then
    Compiler.CompileError(eExpectedOperator, [Token.Display]);

   Expect := eNothing;

  // if (StackPos > 0) Then
    While (StackPos > 0) and (StackPeek.Typ <> mtOpeningBracket) do
     FinalExprPush(StackPop);
  End Else

  { variable or constant }
  if (Token.Token = _IDENTIFIER) Then
  Begin
   if (Expect = eOperator) Then
    Compiler.CompileError(eExpectedOperator, [Token.Display]);

   Expect := eOperator;
   FinalExprPush(mtVariable, Token.Display, Token, NamespaceID); // add it directly into the final expression

   NamespaceID := PreviousNamespace;
  End Else

  { opening bracket }
  if (Token.Token = _BRACKET1_OP) Then
  Begin
   Inc(Bracket);

   if ((Expect = eOperator) and (next_t(-2) <> _IDENTIFIER)) or (Expect = eNothing) Then
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
    Compiler.CompileError(eUnexpected, [')']);

   if (Expect = eValue) Then
    if not ((next_t(-3) = _IDENTIFIER) and (next_t(-2) = _BRACKET1_OP)) Then // not a function call
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
    if (StackPeek.Typ = mtFunctionCall) Then
     FinalExprPush(StackPop);
  End Else

  { array bracket open }
  if (Token.Token = _BRACKET2_OP) Then
  Begin
   Inc(Bracket2);

   Expect := eValue;
   StackPush(mtOpeningBracket2, null, Token);
  End Else

  { array bracket close }
  if (Token.Token = _BRACKET2_CL) Then
  Begin
   Dec(Bracket2);
   if (Bracket2 < 0) Then
    Compiler.CompileError(eUnexpected, [']']);

   if (Expect = eValue) Then
    Compiler.CompileError(eExpectedValue, [']']);

   Expect := eNothing;

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
    if not (next_t(-2) in [_INTEGER, _FLOAT, _STRING, _CHAR, _IDENTIFIER, _BRACKET1_CL, _BRACKET2_CL]) Then
     Str := _UNARY_MINUS;
   End;

   if (Expect = eValue) and (Str <> _UNARY_MINUS) and not (Token.Token in [_EXCLM_MARK, _TILDE, _DOUBLE_PLUS, _DOUBLE_MINUS, _NEW]) Then
    Compiler.CompileError(eExpectedValue, [Token.Display]);

   Expect := eValue;

   if (StackPos > 0) Then // is there any element on the list?
   Begin
    While (isLeftC(Str) and (getOrder(Str) <= getOrder(StackPeek))) or
          (isRightC(Str) and (getOrder(Str) < getOrder(StackPeek))) Do
    Begin
     FinalExprPush(StackPop);
     if (StackPos <= 0) Then // no more elements on the list
      Break;
    End;
   End;

   Case Token.Token of // translate token into the operator
    _PLUS   : StackPush(mtAdd, Token);
    _MINUS  : if (Str = _UNARY_MINUS) Then StackPush(mtNeg, Token) Else StackPush(mtSub, Token);
    _STAR   : StackPush(mtMul, Token);
    _SLASH  : StackPush(mtDiv, Token);
    _PERCENT: StackPush(mtMod, Token);
    _EQUAL  : StackPush(mtAssign, Token);

    _PLUS_EQUAL   : StackPush(mtAddEq, Token);
    _MINUS_EQUAL  : StackPush(mtSubEq, Token);
    _STAR_EQUAL   : StackPush(mtMulEq, Token);
    _SLASH_EQUAL  : StackPush(mtDivEq, Token);
    _PERCENT_EQUAL: StackPush(mtModEq, Token);

    _EXCLM_MARK: StackPush(mtLogicalNOT, Token);
    _TILDE     : StackPush(mtBitwiseNOT, Token);

    _DOUBLE_PLUS:
    if (is_a_post_operator) Then
     StackPush(mtPostInc, Token) Else
     StackPush(mtPreInc, Token);

    _DOUBLE_MINUS:
    if (is_a_post_operator) Then
     StackPush(mtPostDec, Token) Else
     StackPush(mtPreDec, Token);

    _LOWER        : StackPush(mtLower, Token);
    _GREATER      : StackPush(mtGreater, Token);
    _EQUAL_EQUAL  : StackPush(mtEqual, Token);
    _LOWER_EQUAL  : StackPush(mtLowerEqual, Token);
    _GREATER_EQUAL: StackPush(mtGreaterEqual, Token);
    _DIFFERENT    : StackPush(mtDifferent, Token);

    _PIPE            : StackPush(mtBitwiseOR, Token);
    _DOUBLE_PIPE     : StackPush(mtLogicalOR, Token);
    _AMPERSAND       : StackPush(mtBitwiseAND, Token);
    _DOUBLE_AMPERSAND: StackPush(mtLogicalAND, Token);
    _CARON           : StackPush(mtXOR, Token);

    _DOUBLE_LOWER:
    if (next_t = _EQUAL) Then
    Begin
     read;
     StackPush(mtSHLEq, Token);
    End Else
     StackPush(mtSHL, Token);

    _DOUBLE_GREATER:
    if (next_t = _EQUAL) Then
    Begin
     read;
     StackPush(mtSHREq, Token);
    End Else
     StackPush(mtSHL, Token);

    _NEW:
    Begin
     StackPush(mtNew, Token);

     TypeID := read_type(False); // read type
     StackPush(mtInt, TypeID, Token);

     if (next_t <> _BRACKET2_OP) Then // fast syntax-check
     Begin
      read;
      Compiler.CompileError(eExpected, ['[', next(-1).Display]);
     End;

     setPosition(getPosition-1);
    End;

    else
     Case Expect of
      eOperator: Compiler.CompileError(eExpectedOperator, [Token.Display]);
      eValue   : Compiler.CompileError(eExpectedValue, [Token.Display]);
      else Compiler.CompileError(eUnexpected, [Token.Display]);
     End;
   End;

   if (Stack[StackPos-1].Typ in [mtPreInc, mtPostInc, mtPreDec, mtPostDec]) Then
    Expect := eNothing;

   if (Stack[StackPos-1].Typ in [mtLogicalNot, mtBitwiseNot]) Then
    Expect := eValue;
  End;
 End;

 // and add remaining operators to the final operator list
 While (StackPos > 0) Do
  FinalExprPush(StackPop);

 if (FinalExprPos = 0) Then
  FinalExpr[0].Token.Token := noToken;

 {$IFDEF DISPLAY_FINALEXPR}
 For I := 0 To FinalExprPos-1 Do
 Begin
  Write(FinalExpr[I].Token.Line, ' :: ');
  if (FinalExpr[I].Value = null) Then
   Writeln(MExpressionDisplay[FinalExpr[I].Typ]) Else

   Case FinalExpr[I].Typ of
    mtFunctionCall: Writeln(FinalExpr[I].Value, ' (', FinalExpr[I].ParamCount, ')');
    else Writeln(FinalExpr[I].Value);
   End;
 End;
 Writeln;
 {$ENDIF}
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

   if (Tmp^.Typ <> mtVariable) Then
    Compiler.CompileError(eUnimplemented, ['variable-calls']);

   Name       := Tmp^.Value;
   Namespaces := Tmp^.Namespaces;

   if (Name <> 'array_length') Then
   Begin
    ID := Compiler.findLocalVariable(Name); // local things at first

    if (ID = -1) Then // not a local variable
    Begin
     Compiler.findGlobalCandidate(Name, Namespaces, ID, Namespace, @Expr^.Token);

     if (ID = -1) Then // nor a global variable/function - so raise error
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
  End;

  Exit(Expr);
 End;

 { something else (variable, some constant value etc.) }
 Result      := CreateNode(nil, nil, mtNothing, Value.Value, Value.Token, Value.Deep, Value.NamespaceID);
 Result^.Typ := Value.Typ;

 { variable or constant }
 if (Value.Typ = mtVariable) Then
 Begin
  ID := Compiler.findLocalVariable(Result^.Value); // local things at first

  if (ID = -1) Then // not a local variable
  Begin
   Compiler.findGlobalCandidate(Result^.Value, Result^.Namespaces, ID, Namespace, @Result^.Token);

   if (ID <> -1) Then // nor a global variable - so raise error
   Begin // global variable
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
  if (Value.Typ in [mtBool, mtChar, mtInt, mtFloat, mtString]) Then
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
   Node := CreateNode(nil, nil, mtTypeCast, Value.Value, Value.Token, Value.Deep, Value.NamespaceID);

   Node^.Left := CreateNodeFromStack;

   StackPush(mtTree, LongWord(Node));
  End Else

  { function call }
  if (Value.Typ = mtFunctionCall) Then
  Begin
   Node := CreateNode(nil, nil, mtFunctionCall, Value.Value, Value.Token, Value.Deep, Value.NamespaceID);

   SetLength(Node^.ParamList, Value.ParamCount);
   For I := Low(Node^.ParamList) To High(Node^.ParamList) Do
    Node^.ParamList[I] := CreateNodeFromStack;

   Node^.Left := CreateNodeFromStack;

   StackPush(mtTree, LongWord(Node));
  End Else

  { operator }
  if (Value.Typ in MOperators) Then
  Begin
   // binary operators
   if (Value.Typ in MBinaryOperators) Then
   Begin
    MType := Value.Typ;
    Node  := CreateNode(nil, nil, MType, null, Value.Token, Value.Deep);

    Node^.Right := CreateNodeFromStack;
    Node^.Left  := CreateNodeFromStack;

    StackPush(mtTree, LongWord(Node));
   End Else

   // unary operators
   if (Value.Typ in MUnaryOperators) Then
   Begin
    MType := Value.Typ;
    Node  := CreateNode(nil, nil, MType, null, Value.Token, Value.Deep);

    Node^.Left := CreateNodeFromStack;
    StackPush(mtTree, LongWord(Node));
   End Else

   // `new` operator
   if (Value.Typ = mtNew) Then
   Begin
    Node := CreateNode(nil, nil, mtNew, null, Value.Token, Value.Deep);

    Node^.Left  := CreateNodeFromStack;
    Node^.Right := CreateNodeFromStack;

    StackPush(mtTree, LongWord(Node));
   End Else

   // array element
   if (Value.Typ = mtArrayElement) Then
   Begin
    Node := CreateNode(nil, nil, mtArrayElement, null, Value.Token, Value.Deep);

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

{$I constant_folding.pas}

Begin
 if (oConstantFolding in Options) Then
  __constant_folding;

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
  if (Compiler.getBoolOption(opt__constant_folding)) Then
   Include(Options, oConstantFolding);
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
  Result := Expr^.Value;
End;

// TRVariable
Type TRVariable = Record
                   Name   : String;
                   ID     : Integer;
                   RegID  : Integer;
                   RegChar: Char;
                   Typ    : TVType;
                   PosStr : String;
                   Value  : TMExpression;

                   getArray: Byte;

                   isConst: Boolean;
                  End;

{ CompileConstruction }
Function CompileConstruction(const CompilerPnt: Pointer; Expr: PMExpression): TVType;
Var Compiler    : TCompiler;
    PushedValues: Integer=0;

{ Parse }
Function Parse(Expr: PMExpression; FinalRegID: Integer=0; FinalRegChar: Char=#0; const isSubCall: Boolean=True): TVType;
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
Procedure RePop(Expr: PMExpression; TypeID: TVType; Reg: Byte);
Begin
 if (Expr^.ResultOnStack) Then
 Begin
  Compiler.PutOpcode(o_pop, ['e'+Compiler.getTypePrefix(TypeID)+IntToStr(Reg)]);
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
  RegID   := 0;
  RegChar := #0;
  Typ     := TYPE_ANY;
  isConst := False;
  PosStr  := '[0]';
 End;

 Result.getArray := 0;

 { is it an array element? }
 While (Expr^.Typ = mtArrayElement) do
 Begin
  Expr := Expr^.Left;
  Inc(Result.getArray);
 End;

 Result.Name := Expr^.Value;
 Result.ID   := Expr^.IdentID;

 if (Result.ID = -1) Then // variable or constant not found
  goto Failed;

 if (Expr^.isLocal) Then
 Begin
  { local variable or constant }
  With Result do
  Begin
   RegID   := Compiler.getVariableRegID(ID);
   RegChar := Compiler.getVariableRegChar(ID);
   Typ     := Compiler.getVariableType(ID);
   Value   := Compiler.getVariableValue(ID);
   isConst := Compiler.isVariableConstant(ID);

   if (RegID > 0) Then
    PosStr := 'e'+RegChar+IntToStr(RegID) Else
    PosStr := '['+IntToStr(RegID-PushedValues)+']';
  End;
 End Else
 Begin
  { global variable or constant }
  With Compiler.NamespaceList[Expr^.IdentNamespace].GlobalList[Result.ID], Result do
  Begin
   RegID   := mVariable.RegID;
   RegChar := mVariable.RegChar;
   Typ     := mVariable.Typ;
   Value   := mVariable.Value;
   isConst := mVariable.isConst;
  End;
 End;

Failed:
 if (Result.ID = -1) and (FailWhenNotFound) Then
 Begin
  Error(eUnknownVariable, [Result.Name]);
  Exit;
 End;

 if (Result.isConst) and (not AllowConstants) Then
  Error(eLValueExpected, [Expr^.Value]);
End;

{ getTypeFromMExpr }
Function getTypeFromMExpr(Expr: PMExpression): TVType;
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

 Result := getValueFromExpression(Compiler, Expr);
End;

{ isLValue }
Function isLValue(Expr: PMExpression): Boolean;
Var ID, Namespace: Integer;
Begin
 Result := (Expr^.Typ in [mtVariable, mtArrayElement]);

 if (Expr^.Typ = mtVariable) Then // check if passed variable identifier isn't a constant
 Begin
  ID := Compiler.findLocalVariable(Expr^.Value);

  if (ID = -1) Then
  Begin
   Compiler.findGlobalVariableCandidate(Expr^.Value, Expr^.Namespaces, ID, Namespace, @Expr^.Token);

   if (ID > -1) Then
    With Compiler do
     if (NamespaceList[Namespace].GlobalList[ID].mVariable.isConst) Then // global constant
      Exit(False);
  End Else
   With Compiler do
    if (getCurrentFunction.VariableList[ID].isConst) Then // local constant
     Exit(False);
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

  if (Typ = mtFunctionCall) Then
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
// type<type>(value)
{$I typecast.pas}

Var Variable: TRVariable;
Label Over;
Begin
 if (Expr = nil) Then
  Exit(TYPE_VOID);

 Result := TYPE_ANY; // assuming no type
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
    if (Variable.Name = '__line') and (Compiler.getBoolOption(opt_internal_const)) Then // is it a special variable?
    Begin
     if (FinalRegChar = #0) Then
      FinalRegChar := 'i';

     if (FinalRegID > 0) Then
      Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), Expr^.Token.Line+1]) Else
      Begin
       Compiler.PutOpcode(o_push, [Expr^.Token.Line+1]);
       Inc(PushedValues);
      End;

     Exit(TYPE_INT);
    End Else // no, it's not... so - display error
    Begin
     Error(eUnknownVariable, [Variable.Name]);
     Exit;
    End;
   End;

   if (FinalRegChar = #0) Then
    FinalRegChar := Variable.RegChar;

   if (FinalRegID > 0) Then
    __variable_getvalue_reg(Variable, FinalRegID, FinalRegChar) Else // load a variable's value into the register
    __variable_getvalue_stack(Variable);

   Result := Variable.Typ;

   if (Compiler.isTypeFunctionPointer(Result)) and (not isSubCall) Then
    Hint(hDidntYouMean, [Variable.Name+'()']);
  End Else
  Begin // if const value
   Result := getTypeFromMExpr(Expr);

   if (FinalRegChar = #0) Then
    FinalRegChar := Compiler.getTypePrefix(Result);

   if (FinalRegID > 0) Then
    Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), getValueFromMExpr(Expr)]) Else // load a const value into the register
    Begin
     Compiler.PutOpcode(o_push, [getValueFromMExpr(Expr)]); // otherwise push onto the stack
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
  mtFunctionCall: ParseCall;
  mtArrayElement: ParseArrayElement;
  mtNew         : ParseNEW;
  mtTypeCast    : ParseTypeCast;
 End;

Over:
 if (FinalRegID > 0) and (FinalRegChar <> #0) Then // load just calculated value into the register
 Begin
  if (Push_IF_reg) Then
   Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), 'if']) Else
   Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), 'e'+Compiler.getTypePrefix(Result)+'1']);
  Exit;
 End;

 if (FinalRegChar = #0) Then
  FinalRegChar := Compiler.getTypePrefix(Result);

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
