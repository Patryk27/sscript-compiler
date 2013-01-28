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

 Const STACK_SIZE = 1000;

 Const UnaryOperations  = ['!'];
       BinaryOperations = ['+', '-', '*', '/', '%', '='];

       _UNARY_MINUS = #01;

 Type TStackValue = Record // value on the stack
                     Typ  : TMExpressionType;
                     Value: Variant;
                     Token: TToken_P;
                     Deep : Integer;

                     ParamCount: Integer; // if `Typ == stFunction`, there's hold read param count
                    End;

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
                       Procedure FinalExprPush(Typ: TMExpressionType; Value: Variant; Token: TToken_P);
                       Function FinalExprPop: TStackValue;
                       Function FinalExprPeek: TStackValue;

                       Function CreateNode(Left, Right: PMExpression; Typ: TMExpressionType; Value: Variant; Token: TToken_P; Deep: Integer): PMExpression;
                      Public
                       Constructor Create(fCompiler: TCompiler);

                       Procedure Parse(EndTokens: TTokenSet);
                       Function MakeTree: PMExpression;
                      End;

 Function MakeConstruction(Compiler: Pointer; EndTokens: TTokenSet=[_SEMICOLON]): TMConstruction;
 Function CompileConstruction(CompilerPnt: Pointer; Expr: PMExpression): TVType;

 Implementation
Uses SysUtils,
     CompilerUnit, Opcodes, Messages;

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
Const Order: Array[0..7] of Array[0..5] of String =
(
 // operators' precedence (from the most important to the less)
 (_UNARY_MINUS, '@', '&', '|', '^', '!'),
 ('++', '--', '', '', '', ''),
 ('*', '/', '%', '', '', ''),
 ('+', '-', '', '', '', ''),
 ('||', '&&', '!=', '', '', ''),
 ('<', '<=', '==', '>=', '>', ''),
 ('<<', '>>', '', '', '', ''),
 ('=', '+=', '-=', '/=', '*=', '%=')
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
 Result := (X[1] in ['!', '=']) or (X = '++') or (X = '--');
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
Procedure TInterpreter.FinalExprPush(Typ: TMExpressionType; Value: Variant; Token: TToken_P);
Var Val: TStackValue;
Begin
 Val.Typ   := Typ;
 Val.Value := Value;
 Val.Token := Token;
 Val.Deep  := Compiler.CurrentDeep;
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
Function TInterpreter.CreateNode(Left, Right: PMExpression; Typ: TMExpressionType; Value: Variant; Token: TToken_P; Deep: Integer): PMExpression;
Begin
 New(Result);

 Result^.Left  := Left;
 Result^.Right := Right;
 Result^.Typ   := Typ;
 Result^.Value := Value;
 Result^.Token := Token;
 Result^.Deep  := Deep;

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
Var Token   : TToken_P;
    Str     : String;
    Value   : TStackValue;
    Bracket : Integer=0;
    Bracket2: Integer=0;
    Expect  : (eNothing, eValue, eOperator);
    Parsed  : Boolean;

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

 if (Compiler.next_t(1) = _BRACKET1_CL) Then { func() }
  Exit(0);

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

Begin
With Compiler do
Begin
 StackPos     := 0;
 FinalExprPos := 0;
 Expect       := eValue;

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

  { function }
  if (Token.Token = _IDENTIFIER) and (next_t = _BRACKET1_OP) Then
  Begin
   StackPush(mtFunction, Token.Display, Token); // push it onto the stack
   Stack[StackPos-1].ParamCount := ReadParamCount;

   Expect := eValue;
  End Else

  { function parameters separator (comma) }
  if (Token.Token = _COMMA) Then
  Begin
   if (Expect = eValue) Then
    Compiler.CompileError(eExpectedValue, [Token.Display]);

   Expect := eNothing;

  // if (StackPos > 0) Then
    While (StackPos > 0) and (StackPeek.Typ <> mtOpeningBracket) do
     FinalExprPush(StackPop);
  End Else

  { variable or constant }
  if (Token.Token = _IDENTIFIER) Then
  Begin
   Expect := eOperator;
   FinalExprPush(mtVariable, Token.Display, Token); // add it directly into the final expression
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
    if (StackPeek.Typ = mtFunction) Then
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

   if (Expect = eValue) and (Str <> _UNARY_MINUS) and not (Token.Token in [_EXCLM_MARK, _TILDE, _DOUBLE_PLUS, _DOUBLE_MINUS]) Then
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
    if (next_t(-2) = _IDENTIFIER) Then
     StackPush(mtPostInc, Token) Else
     StackPush(mtPreInc, Token);

    _DOUBLE_MINUS:
    if (next_t(-2) = _IDENTIFIER) Then
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
    mtFunction: Writeln(FinalExpr[I].Value, ' (', FinalExpr[I].ParamCount, ')');
    else Writeln(FinalExpr[I].Value);
   End;
 End;
 Writeln;
 {$ENDIF}
End;
End;

{ TInterpreter.MakeTree }
Function TInterpreter.MakeTree: PMExpression;
Const Constants: Set of TMExpressionType = [mtBool, mtChar, mtInt, mtFloat, mtString];
Var Pos, I: Integer;
    Value : TStackValue;
    Value2: TStackValue;
    MType : TMExpressionType;
    Node  : PMExpression;

{ CreateNodeFromStack }
Function CreateNodeFromStack: PMExpression;
Var Value: TStackValue;
Begin
 Value := StackPop;

 if (Value.Typ = mtTree) Then // whole expression tree
 Begin
  if (Value.Value = null) Then
   Compiler.CompileError(eInternalError, ['Value.Value = null']);

  Exit(PMExpression(LongWord(Value.Value)));
 End;

 // something else (variable, value etc.)
 Result      := CreateNode(nil, nil, mtNothing, Value.Value, Value.Token, Value.Deep);
 Result^.Typ := Value.Typ;
End;

{ AreStackNodesConstants }
Function AreStackNodesConstants: Boolean;
Begin
 Result := (Stack[StackPos-1].Typ in Constants) and
           (Stack[StackPos-2].Typ in Constants);
End;

{ isStackNodeConstant }
Function isStackNodeConstant: Boolean;
Begin
 Result := (Stack[StackPos-1].Typ in Constants);
End;

Label CannotOptimize, CannotOptimize2;
Begin
 Pos      := 0;
 StackPos := 0;

 if (FinalExpr[0].Token.Token = noToken) Then
  Exit(nil);

 Repeat
  Value := FinalExpr[Pos];

  // constant
  if (Value.Typ in [mtBool, mtChar, mtInt, mtFloat, mtString]) Then
  Begin
   StackPush(Value);
  End Else

  // variable
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

  // function
  if (Value.Typ = mtFunction) Then
  Begin
   Node := CreateNode(nil, nil, mtFunction, Value.Value, Value.Token, Value.Deep);

   SetLength(Node^.ParamList, Value.ParamCount);
   For I := Low(Node^.ParamList) To High(Node^.ParamList) Do
    Node^.ParamList[I] := CreateNodeFromStack;

   StackPush(mtTree, LongWord(Node));
  End Else

  // operator
  if (Value.Typ in MOperators) Then
  Begin
   // binary operators
   if (Value.Typ in MBinaryOperators) Then
   Begin
    MType := Value.Typ;
    Node  := CreateNode(nil, nil, MType, null, Value.Token, Value.Deep);

    if (AreStackNodesConstants) and (_Of in Compiler.Options) Then
    Begin
     { constant folding }
     Value2 := Stack[StackPos-1];
     Value  := Stack[StackPos-2];

     if (Value.Typ = mtInt) and (Value2.Typ = mtFloat) Then // extend types
      Value.Typ := mtFloat;
     if (Value.Typ = mtFloat) and (Value2.Typ = mtInt) Then
      Value2.Typ := mtFloat;

     if (Value.Typ = Value2.Typ) Then // types must be the same
     Begin
      if (Value.Typ in [mtInt, mtFloat]) Then // for numerical types
      Begin
       Case MType of
        mtAdd: Value.Value += Value2.Value;
        mtSub: Value.Value -= Value2.Value;
        mtMul: Value.Value *= Value2.Value;
        mtDiv: if (Value2.Value = 0) Then
                Compiler.CompileError(eDivByZero, []) Else
                Value.Value /= Value2.Value;
        mtMod: if (Value2.Value = 0) Then
                Compiler.CompileError(eDivByZero, []) Else
                Value.Value := Value.Value mod Value2.Value;
        else
         goto CannotOptimize; // @TODO: bitwise and, or, xor
       End;
      End Else

      if (Value.Typ in [mtString]) Then // for string types
      Begin
       Case MType of
        mtAdd: Value.Value += Value2.Value;

        else
         goto CannotOptimize;
       End;
      End Else

      if (Value.Typ in [mtBool]) Then // for boolean types
      Begin
       Case MType of
        mtLogicalOR: Value.Value := Value.Value or Value2.Value;
        mtLogicalAND: Value.Value := Value.Value and Value2.Value;

        else
         goto CannotOptimize;
       End;
      End Else
       goto CannotOptimize; // unknown type

      if (MType in MCompareOperators) Then
      Begin
       Value.Typ := mtBool;

       Case MType of
        mtLower       : Value.Value := Value.Value < Value2.Value;
        mtGreater     : Value.Value := Value.Value > Value2.Value;
        mtEqual       : Value.Value := Value.Value = Value2.Value;
        mtDifferent   : Value.Value := Value.Value <> Value2.Value;
        mtLowerEqual  : Value.Value := Value.Value <= Value2.Value;
        mtGreaterEqual: Value.Value := Value.Value >= Value2.Value;
       End;
      End;

      Dec(StackPos, 2);
      StackPush(Value);

      Inc(Pos); // next token
      Continue; // go to next step
     End Else
      goto CannotOptimize;
    End;

   CannotOptimize:
    Node^.Right := CreateNodeFromStack;
    Node^.Left  := CreateNodeFromStack;

    StackPush(mtTree, LongWord(Node));
   End Else

   // unary operators
   if (Value.Typ in MUnaryOperators) Then
   Begin
    MType := Value.Typ;
    Node  := CreateNode(nil, nil, MType, null, Value.Token, Value.Deep);

    if (isStackNodeConstant) and (_Of in Compiler.Options) Then
    Begin
     { constant folding }
     Value := Stack[StackPos-1];

     if (Value.Typ in [mtInt, mtFloat]) Then
     Begin
      Case MType of
       mtNeg: Value.Value := -Value.Value;
       else goto CannotOptimize2;
      End;
     End Else
      goto CannotOptimize2;

     Dec(StackPos);
     StackPush(Value);

     Inc(Pos); // next token
     Continue; // go to next step
    End;

   CannotOptimize2:
    Node^.Left := CreateNodeFromStack;
    StackPush(mtTree, LongWord(Node));
   End Else

   // other operators
   if (Value.Typ = mtArrayElement) Then // array element
   Begin
    Node := CreateNode(nil, nil, mtArrayElement, null, Value.Token, Value.Deep);

    Node^.Right := CreateNodeFromStack;
    Node^.Left  := CreateNodeFromStack;

    StackPush(mtTree, LongWord(Node));
   End;
  End;

  Inc(Pos);
 Until (Pos >= FinalExprPos);

 Result := CreateNodeFromStack;
End;

// ---------- </> ---------- //

{ MakeConstruction }
Function MakeConstruction(Compiler: Pointer; EndTokens: TTokenSet=[_SEMICOLON]): TMConstruction;
Var Interpreter: TInterpreter;
Begin
 // we need to make RPN and then evaluate it; as a result, we'll have TMConstruction with 1 value - the whole expression tree.
 Interpreter := TInterpreter(TCompiler(Compiler).Interpreter);
 Interpreter.Parse(EndTokens);
 Result.Typ := ctExpression;
 SetLength(Result.Values, 1);
 Result.Values[0] := Interpreter.MakeTree;

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

                   getArray: Byte;
                  End;

{ CompileConstruction }
Function CompileConstruction(CompilerPnt: Pointer; Expr: PMExpression): TVType;
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
Function getVariable(Expr: PMExpression; const FailWhenNotFound: Boolean=False): TRVariable;
Begin
 Result.getArray :=  0;

 While (Expr^.Typ = mtArrayElement) do
 Begin
  Expr := Expr^.Left;
  Inc(Result.getArray);
 End;

 Result.Name := Expr^.Value;
 Result.ID   := Compiler.findVariable(Result.Name, Expr^.Deep);

 if (Result.ID = -1) Then
 Begin
  With Result do
  Begin
   RegID   := 0;
   RegChar := #0;
   Typ     := TYPE_ANY;
   PosStr  := '[0]';
  End;

  if (FailWhenNotFound) Then
   Error(eUnknownVariable, [Result.Name]);
 End Else
 Begin
  With Result do
  Begin
   RegID   := Compiler.getVariableRegID(ID);
   RegChar := Compiler.getVariableRegChar(ID);
   Typ     := Compiler.getVariableType(ID);

   if (RegID > 0) Then
    PosStr := 'e'+RegChar+IntToStr(RegID) Else
    PosStr := '['+IntToStr(RegID)+']';
  End;
 End;
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

 Result := VarToStr(Expr^.Value);

 Case Expr^.Typ of
  mtChar: Result := '#'+IntToStr(ord(Result[1]));
  mtString: Result := Compiler.AddString(Result);
 End;
End;

{ isLValue }
Function isLValue(Expr: PMExpression): Boolean;
Begin
 Result := (Expr^.Typ in [mtVariable, mtArrayElement]);
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

  if (Typ = mtFunction) Then
   Inc(Result);
 End;
End;

{ CompileSimple }
Function CompileSimple(out TypeLeft, TypeRight: TVType; const isLeftVariable: Boolean=False): TVType;
Var Variable   : TRVariable;
    LeftFirst  : Boolean=False;
    isComparing: Boolean;
Begin
 isComparing := Expr^.Typ in [mtEqual, mtDifferent, mtGreater, mtLower, mtGreaterEqual, mtLowerEqual];

 if (isLeftVariable) Then
 Begin
  Variable := getVariable(Left, False);

  TypeLeft  := Variable.Typ;
  TypeRight := Parse(Right, 2);
 End Else
 Begin
  if (countLeaves(Right) >= countLeaves(Left)) Then
  Begin
   LeftFirst := False;
   TypeRight := Parse(Right, 2); // right to second register
   TypeLeft  := Parse(Left, 1); // left to first register
  End Else
  Begin
   LeftFirst := True;
   TypeLeft  := Parse(Left, 1); // left to first register
   TypeRight := Parse(Right, 2); // right to second register
  End;
 End;

 // we must 'pop' result back to the corresponding register
 if (LeftFirst) Then
 Begin
  RePop(Right, TypeRight, 2);
  RePop(Left, TypeLeft, 1);
 End Else
 Begin
  RePop(Left, TypeLeft, 1);
  RePop(Right, TypeRight, 2);
 End;

 With Compiler do
 Begin
  { cast table }

  if (isTypeFloat(TypeLeft)) and (isTypeInt(TypeRight)) Then // extend types { float, int -> float, float }
  Begin
   PutOpcode(o_mov, ['e'+getTypePrefix(TypeLeft)+'2', 'e'+getTypePrefix(TypeRight)+'2']);
   TypeRight := TYPE_FLOAT;
  End;

  if (isTypeInt(TypeLeft)) and (isTypeFloat(TypeRight)) Then // extend types { int, float -> float, float }
  Begin
   PutOpcode(o_mov, ['e'+getTypePrefix(TypeRight)+'1', 'e'+getTypePrefix(TypeLeft)+'1']);
   TypeLeft := TYPE_FLOAT;
  End;

  if (isTypeString(TypeLeft)) and (isTypeChar(TypeRight)) and (not isComparing) Then // { string, char -> string, string }
  Begin
   if (isTypeInt(TypeRight)) Then
   Begin
    PutOpcode(o_mov, ['e'+getTypePrefix(TYPE_CHAR)+'2', 'e'+getTypePrefix(TypeRight)+'2']);
    TypeRight := TYPE_CHAR;
   End;

   PutOpcode(o_mov, ['e'+getTypePrefix(TypeLeft)+'2', 'e'+getTypePrefix(TypeRight)+'2']);
   TypeRight := TYPE_STRING;
  End;

  if (isTypeChar(TypeLeft)) and (isTypeString(TypeRight)) and (not isComparing) Then { char, string -> string, string }
  Begin
   if (isTypeInt(TypeRight)) Then
   Begin
    PutOpcode(o_mov, ['e'+getTypePrefix(TYPE_CHAR)+'1', 'e'+getTypePrefix(TypeLeft)+'1']);
    TypeRight := TYPE_CHAR;
   End;

   PutOpcode(o_mov, ['e'+getTypePrefix(TypeRight)+'1', 'e'+getTypePrefix(TypeLeft)+'1']);
   TypeLeft := TYPE_STRING;
  End;

  if (isTypeChar(TypeLeft)) and (isTypeInt(TypeRight)) Then // { char, int -> int, int }
  Begin
   PutOpcode(o_mov, ['e'+getTypePrefix(TypeRight)+'1', 'e'+getTypePrefix(TypeLeft)+'1']);
   TypeLeft := TYPE_INT;
  End;

  if (isTypeInt(TypeLeft)) and (isTypeChar(TypeRight)) Then // { int, char -> int, int }
  Begin
   PutOpcode(o_mov, ['e'+getTypePrefix(TypeLeft)+'1', 'e'+getTypePrefix(TypeRight)+'1']);
   TypeLeft := TYPE_INT;
  End;

  if (not CompareTypes(TypeLeft, TypeRight)) Then // unsupported operator (eg.`int+string`)
  Begin
   Error(eUnsupportedOperator, [getTypeName(TypeLeft), getDisplay(Expr), getTypeName(TypeRight)]);
   Exit;
  End;
 End;

 Result := TypeLeft;
End;

{ ParseCompare }
Procedure ParseCompare;
Var TypeLeft, TypeRight: TVType;
    Opcode             : TOpcode_E;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 Case Expr^.Typ of
  mtLower       : Opcode := o_if_l;
  mtGreater     : Opcode := o_if_g;
  mtEqual       : Opcode := o_if_e;
  mtLowerEqual  : Opcode := o_if_le;
  mtGreaterEqual: Opcode := o_if_ge;
  mtDifferent   : Opcode := o_if_ne;
 End;

 Compiler.PutOpcode(Opcode, ['e'+Compiler.getTypePrefix(TypeLeft)+'1', 'e'+Compiler.getTypePrefix(TypeRight)+'2']);

 Result      := TYPE_BOOL;
 Push_IF_reg := True;
End;

{ ParseArithmeticOperator }
Procedure ParseArithmeticOperator(const WithAssign: Boolean);
Var TypeLeft, TypeRight: TVType;
    Opcode             : TOpcode_E;
    Variable           : TRVariable;
Begin
 if (WithAssign) Then
 Begin
  if (not isLValue(Left)) Then
  Begin
   Error(eLValueExpected, [getDisplay(Left)]);
   Exit;
  End;

  Variable := getVariable(Left, True);
  if (Variable.ID = -1) Then // variable not found
   Exit;

  if (Variable.getArray <> 0) Then
   Error(eInternalError, ['Unsupported for now, sorry ;<']);
 End;

 // @TODO:
 {
  var<string> str = "asdf";
  str[1] += char(1);
  // str = "bsdf";
 }

 Result := CompileSimple(TypeLeft, TypeRight, WithAssign);

 Case Expr^.Typ of
  mtAdd, mtAddEq: if (Compiler.isTypeString(TypeLeft)) Then
                   Opcode := o_strjoin Else
                   Opcode := o_add;
  mtSub, mtSubEq: Opcode := o_sub;
  mtMul, mtMulEq: Opcode := o_mul;
  mtDiv, mtDivEq: Opcode := o_div;
  mtMod, mtModEq: Opcode := o_mod; // @TODO: ints only
  mtSHL, mtSHLEq: Opcode := o_shl;
  mtSHR, mtSHREq: Opcode := o_shr;
 End;

 if ((not (Expr^.Typ in [mtAdd, mtAddEq])) and (not Compiler.isTypeNumerical(Result))) Then // numerical types only (except '+' and '+=' for strings)
 Begin
  Error(eUnsupportedOperator, [Compiler.getTypeName(TypeLeft), getDisplay(Expr), Compiler.getTypeName(TypeRight)]);
  Exit;
 End;

 Case WithAssign of
  True: Compiler.PutOpcode(Opcode, [Variable.PosStr, 'e'+Compiler.getTypePrefix(TypeRight)+'2']);
  False: Compiler.PutOpcode(Opcode, ['e'+Compiler.getTypePrefix(TypeLeft)+'1', 'e'+Compiler.getTypePrefix(TypeRight)+'2']);
 End;
End;

{ ParseAssign }
Procedure ParseAssign;
Var Variable: TRVariable;
    TypeID  : TVType;
    Index   : Byte;
    fIndex  : TVType;
Begin
 // left side is l-value, right side is the expression to parse
 if (not isLValue(Left)) Then
 Begin
  Error(eLValueExpected, [getDisplay(Left)]);
  Exit;
 End;

 Variable := getVariable(Left, True);
 if (Variable.ID = -1) Then // variable not found
  Exit;

 if (Variable.getArray = 0) Then // if variable isn't an array
 Begin
  if (Variable.RegID > 0) Then // if variable is stored in the register, we can directly set variable's value (without using a helper-register)
  Begin
   TypeID := Parse(Right, Variable.RegID, Variable.RegChar);
  End Else // parse expression
  Begin
   TypeID := Parse(Right, 1); // parse expression and load it into the first register
   RePop(Right, TypeID, 1);

   Compiler.__variable_setvalue_reg(Variable.ID, 1, Compiler.getTypePrefix(TypeID), PushedValues);
  End;

  if (not Compiler.CompareTypes(Variable.Typ, TypeID)) Then // type check
  Begin
   Error(eWrongType, [Compiler.getTypeName(TypeID), Compiler.getTypeName(Variable.Typ)]);
   Exit;
  End;

  Exit;
 End;

 (* ===== array only ===== *)

 Index := Variable.getArray;

 if (Index > 1) Then { shouldn't happen, in fact }
  Error(eInternalError, ['Index > 1']);

 if (Index = 1) Then
 Begin
  fIndex := Parse(Left^.Right, 2); // load current array index element into 'ei2'
  Compiler.PutOpcode(o_push, ['ei2']);
  Inc(PushedValues);

  TypeID := Parse(Right, 1); // a value which we want to save into the array

  RePop(Right, TypeID, 1);

  Compiler.PutOpcode(o_pop, ['ei2']);
  Dec(PushedValues);
  RePop(Left^.Right, fIndex, 1);

  if (not Compiler.isTypeArray(Variable.Typ)) or (not Compiler.isTypeInt(fIndex)) Then // check types (variable must be an array, index must be an int)
  Begin
   With Compiler do
    Error(eInvalidArraySubscript, [getTypeName(Variable.Typ), getTypeName(fIndex)]);
   Exit;
  End;

  if (not Compiler.isTypeChar(TypeID)) Then // also temporary solution... this part of code is actually only a partial solution (as we support array operations on string only) :P
  Begin
   With Compiler do
    Error(eWrongType, [getTypeName(TypeID), getTypeName(TYPE_CHAR)]);
   Exit;
  End;

  Compiler.PutOpcode(o_arset, [Variable.PosStr, 'ei2', 'e'+Compiler.getTypePrefix(TypeID)+'1']);

  Result := TYPE_CHAR;
 End;
End;

{ ParseLogicalOR }
Procedure ParseLogicalOR;
Var TypeLeft, TypeRight: TVType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
  if (not isTypeBool(Result)) Then
   Error(eUnsupportedOperator, [getTypeName(TypeLeft), getDisplay(Expr), getTypeName(TypeRight)]) Else
   PutOpcode(o_or, ['eb1', 'eb2']);
End;

{ ParseLogicalAND }
Procedure ParseLogicalAND;
Var TypeLeft, TypeRight: TVType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
  if (not isTypeBool(Result)) Then
   Error(eUnsupportedOperator, [getTypeName(TypeLeft), getDisplay(Expr), getTypeName(TypeRight)]) Else
   PutOpcode(o_and, ['eb1', 'eb2']);
End;

{ ParseBitwiseOR }
Procedure ParseBitwiseOR;
Var TypeLeft, TypeRight: TVType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
  if (not isTypeInt(Result)) Then
   CompileError(eUnsupportedOperator, [getTypeName(TypeLeft), getDisplay(Expr), getTypeName(TypeRight)]) Else
   PutOpcode(o_or, ['ei1', 'ei2']);
End;

{ ParseBitwiseAND }
Procedure ParseBitwiseAND;
Var TypeLeft, TypeRight: TVType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
  if (not isTypeInt(Result)) Then
   CompileError(eUnsupportedOperator, [getTypeName(TypeLeft), getDisplay(Expr), getTypeName(TypeRight)]) Else
   PutOpcode(o_and, ['ei1', 'ei2']);
End;

{ ParseXOR }
Procedure ParseXOR;
Var TypeLeft, TypeRight: TVType;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 With Compiler do
 Begin
  if (isTypeInt(Result)) Then
   PutOpcode(o_xor, ['ei1', 'ei2']) Else
  if (isTypeBool(Result)) Then
   PutOpcode(o_xor, ['eb1', 'eb1']) Else
   CompileError(eUnsupportedOperator, [getTypeName(TypeLeft), getDisplay(Expr), getTypeName(TypeRight)])
 End;
End;

{ ParseCall }
Procedure ParseCall;
Var FuncID, Param, TypeID: Integer;
Begin
 FuncID := Compiler.findFunction(Expr^.Value); // get function ID

 if (FuncID = -1) Then // function not found
 Begin
  FuncID := Compiler.findTypeByName(Expr^.Value); // so, is it a type-casting?

  if (FuncID = -1) Then // No, it's not ;<
  Begin
   Error(eUnknownFunction, [Expr^.Value]);
   Exit;
  End;

  // Yes, it is - so do casting
  if (Length(Expr^.ParamList) <> 1) Then
  Begin
   Error(eWrongParamCount, [Expr^.Value, Length(Expr^.ParamList), 1]);
   Exit;
  End;

  // load a value to cast onto the register
  Expr^.ResultOnStack := False;
  Parse(Expr^.ParamList[0], 1, Compiler.getTypePrefix(FuncID));

  Result := FuncID;

  Exit;
 End;

 With Compiler.FunctionList[FuncID] do
 Begin
  // check param count
  if (Length(Expr^.ParamList) <> Length(ParamList)) Then
  Begin
   Error(eWrongParamCount, [Name, Length(ParamList), Length(Expr^.ParamList)]);
   Exit;
  End;

  // push parameters onto the stack
  For Param := Low(ParamList) To High(ParamList) Do
  Begin
   TypeID := Parse(Expr^.ParamList[Param], -1);

   With Compiler do
    if (not CompareTypes(ParamList[High(ParamList)-Param].Typ, TypeID)) Then
     Error(eWrongType, [getTypeName(TypeID), getTypeName(ParamList[High(ParamList)-Param].Typ)]);
  End;

  Dec(PushedValues, Length(ParamList));

  // call function
  Compiler.PutOpcode(o_call, [':'+MName]);

  Result := Return;
 End;
End;

{ ParsePIncDec }
Procedure ParsePIncDec;
Var Variable: TRVariable;
Begin
 // left side have to be a l-value
 if (not islValue(Left)) Then
 Begin
  Error(eLValueExpected, [getDisplay(Left)]);
  Exit;
 End;

 Variable := getVariable(Expr^.Left, True);
 if (Variable.ID = -1) Then // variable not found
  Exit;

 if (not Compiler.isTypeNumerical(Variable.Typ)) Then
  if (Expr^.Typ in [mtPreInc, mtPreDec]) Then
   Error(eUnsupportedUOperator, [getDisplay(Expr), Compiler.getTypeName(Variable.Typ)]) Else
   Error(eUnsupportedUOperator, [Compiler.getTypeName(Variable.Typ), getDisplay(Expr)]);

 if (Expr^.Typ in [mtPostInc, mtPostDec]) and (isSubCall) Then
  Compiler.PutOpcode(o_mov, ['e'+Variable.RegChar+'1', Variable.PosStr]);

 Case Expr^.Typ of
  mtPreInc, mtPostInc: Compiler.PutOpcode(o_add, [Variable.PosStr, 1]);
  mtPreDec, mtPostDec: Compiler.PutOpcode(o_sub, [Variable.PosStr, 1]);
 End;

 if (Expr^.Typ in [mtPreInc, mtPreDec]) and (isSubCall) Then
  Compiler.PutOpcode(o_mov, ['e'+Variable.RegChar+'1', Variable.PosStr]);

 Result := Variable.Typ;
End;

{ ParseNEG }
Procedure ParseNEG;
Begin
 // load value to the first register
 Result := Parse(Left, 1);
 RePop(Left, Result, 1);

 if (not Compiler.isTypeNumerical(Result)) Then // numerical types only
 Begin
  Error(eUnsupportedUOperator, [getDisplay(Expr), Compiler.getTypeName(Result)]);
  Exit;
 End;

 Compiler.PutOpcode(o_neg, ['e'+Compiler.getTypePrefix(Result)+'1']);
End;

{ ParseLogicalNOT }
Procedure ParseLogicalNOT;
Begin
 // load value to the first register
 Result := Parse(Left, 1);
 RePop(Left, Result, 1);

 if (not Compiler.isTypeInt(Result)) and (not Compiler.isTypeBool(Result)) Then
 Begin
  Error(eUnsupportedUOperator, [getDisplay(Expr), Compiler.getTypeName(Result)]);
  Exit;
 End;

 if (Compiler.isTypeInt(Result)) Then // implicit cast: int->bool
  Compiler.PutOpcode(o_mov, ['eb1', 'ei1']);

 Compiler.PutOpcode(o_not, ['eb1']);

 Result := TYPE_BOOL;
End;

{ ParseBitwiseNOT }
Procedure ParseBitwiseNOT;
Begin
 // load value to the first register
 Result := Parse(Left, 1);
 RePop(Left, Result, 1);

 if (not Compiler.isTypeInt(Result)) Then
 Begin
  Error(eUnsupportedUOperator, [getDisplay(Expr), Compiler.getTypeName(Result)]);
  Exit;
 End;

 Compiler.PutOpcode(o_not, ['ei1']);
End;

{ ParseArrayElement }
Procedure ParseArrayElement;
Var vArray, vIndex: TVType;
Begin
 vArray := Parse(Left, 1); // array to the first register
 vIndex := Parse(Right, 1); // so do the index
 {
  @note:
  even though it looks a bit something-is-not-right-here, take into consideration fact that array
  would be loaded into `er1` and index `ei1` ;)
 }
 RePop(Right, vIndex, 1);
 RePop(Left, vArray, 1);

 if (not Compiler.isTypeArray(vArray)) Then // must be an array
 Begin
  Error(eUnsupportedUOperator, [Compiler.getTypeName(vArray), getDisplay(Expr)]);
  Exit;
 End;

 if (not Compiler.isTypeInt(vIndex)) Then // index must be a numeric value
 Begin
  Error(eWrongType, [Compiler.getTypeName(vIndex), Compiler.getTypeName(TYPE_INT)]);
  Exit;
 End;

 // @TODO: it's a temporary solution (until I'll write code for array support)
 Compiler.PutOpcode(o_arget, ['es1', 'ei1', 'ec1']); { ec1 = es1[ei1] }

 Result := TYPE_CHAR;
End;

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
  if (Expr^.Typ = mtFunction) Then // function call
  Begin
   ParseCall;
   goto Over;
  End;

  // load value onto the register `FinalRegID`
  if (Expr^.Typ = mtVariable) Then // is variable...
  Begin
   Variable := getVariable(Expr);

   if (Variable.ID = -1) Then // variable not found
   Begin
    Error(eUnknownVariable, [Variable.Name]);
    Exit;
   End;

   if (FinalRegChar = #0) Then
    FinalRegChar := Variable.RegChar;

   if (FinalRegID > 0) Then
    Compiler.__variable_getvalue_reg(Variable.ID, FinalRegID, FinalRegChar, PushedValues) Else // load a variable's value into the register
    Begin
     Compiler.__variable_getvalue_stack(Variable.ID, PushedValues);
     Inc(PushedValues);
    End;

   Result := Variable.Typ;
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
  mtArrayElement: ParseArrayElement;
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

 Result := Parse(Expr, 0, #0, False);
End;
End.
