(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)

{$MODESWITCH ADVANCEDRECORDS}
{$MACRO ON}

Unit ExpressionCompiler;

 Interface
 Uses SysUtils, Math, SSCompiler, Expression, symdef, Tokens, ExpressionParser, Variants, TypInfo;

 Type TOptimizerOption = (oGetFromCommandLine, oInsertConstants, oConstantFolding, oTreeSimplification, oDisplayParseErrors);
      TOptimizerOptions = Set of TOptimizerOption;

      TShortCircuit = (scNone, scOR, scAND);

 Function OptimizeExpression(const Compiler: TCompiler; var Tree: PExpressionNode; const Options: TOptimizerOptions): Boolean;

 Function MakeExpression(const CompilerPnt: Pointer; const EndTokens: TTokenSet=[_SEMICOLON]; Options: TOptimizerOptions=[oGetFromCommandLine]): PExpressionNode;
 Function CompileExpression(const CompilerPnt: Pointer; const Expr: PExpressionNode): TType;

 Implementation
Uses Logging, CommandLine, Opcodes, Messages,
     ExpressionConstantInsertion, ExpressionConstantFolding, ExpressionTreeSimplification;

(* OptimizeExpression *)
{
 Optimizes expression with given options.
 Returns `true` if anything has been optimized.
}
Function OptimizeExpression(const Compiler: TCompiler; var Tree: PExpressionNode; const Options: TOptimizerOptions): Boolean;
Begin
 Result := False;

 // constant insertion
 if (oInsertConstants in Options) Then
 Begin
  With TExpressionConstantInsertion.Create(Compiler, Compiler.getCurrentFunction, oDisplayParseErrors in Options) do
  Begin
   Result := Result or Execute(Tree);
   Free;
  End;
 End;

 // constant folding
 if (oConstantFolding in Options) Then
 Begin
  With TExpressionConstantFolding.Create(Compiler, Compiler.getCurrentFunction, oDisplayParseErrors in Options) do
  Begin
   Result := Result or Execute(Tree);
   Free;
  End;
 End;

 // tree simplification
 if (oTreeSimplification in Options) Then
 Begin
  With TExpressionTreeSimplification.Create(Compiler, Compiler.getCurrentFunction, oDisplayParseErrors in Options) do
  Begin
   Result := Result or Execute(Tree);
   Free;
  End;
 End;
End;

(* MakeExpression *)
Function MakeExpression(const CompilerPnt: Pointer; const EndTokens: TTokenSet=[_SEMICOLON]; Options: TOptimizerOptions=[oGetFromCommandLine]): PExpressionNode;
Var Compiler: TCompiler absolute CompilerPnt;
    Parser  : TExpressionParser;
Begin
 Parser := TExpressionParser.Create(Compiler);

 Try
  if (oGetFromCommandLine in Options) Then
  Begin
   Options -= [oGetFromCommandLine];

   if (CmdLine.getBoolSwitch(opt__constant_folding)) Then
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
    DevLog(dvWarning, 'Value = null; returned `nil`');
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

   if (Variable.Symbol = nil) Then // variable not found - maybe a internal one?
   Begin
    if (CmdLine.getBoolSwitch(opt__internal_const)) Then
    Begin
     Case Variable.Name of
     { __line }
      '__line':
      Begin
       if (FinalRegID > 0) Then // load into register
       Begin
        if (FinalRegChar = #0) Then
         FinalRegChar := 'i';

        Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), Expr^.Token.Line]);
       End Else
       Begin // load onto stack
        Compiler.PutOpcode(o_push, [Expr^.Token.Line]);
        Inc(PushedValues);
       End;

       Exit(TYPE_INT);
      End;

     { __linestr }
      '__linestr':
      Begin
       if (FinalRegID > 0) Then // put into register?
       Begin
        if (FinalRegChar = #0) Then
         FinalRegChar := 's';

        Compiler.PutOpcode(o_mov, ['e'+FinalRegChar+IntToStr(FinalRegID), '"'+IntToStr(Expr^.Token.Line)+'"']);
       End Else
       Begin // push onto stack?
        Compiler.PutOpcode(o_push, ['"'+IntToStr(Expr^.Token.Line)+'"']);
        Inc(PushedValues);
       End;

       Exit(TYPE_STRING);
      End;

      { other - show error }
      else
      Begin
       Error(eUnknownVariable, [Variable.Name]);
       Exit;
      End;
     End;
    End Else
    Begin // error: unknown variable
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
  DevLog(dvInfo, 'Result = nil; assuming `TYPE_ANY`');
  Exit(TYPE_ANY);
 End;

 if (FinalRegChar = #0) Then
  FinalRegChar := Result.RegPrefix;

 if (FinalRegChar <> #0) Then
 Begin
  if (Push_IF_reg) Then
  Begin
   // when values are compared, the comparing result is in the 'if' register
   Compiler.PutOpcode(o_push, ['if']);
  End Else
  Begin
   if (FinalRegChar in ['b', 'c', 'i', 'f', 's', 'r']) Then // is it valid FinalRegChar?
    Compiler.PutOpcode(o_push, ['e'+FinalRegChar+'1']); // save value onto the stack
  End;

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
