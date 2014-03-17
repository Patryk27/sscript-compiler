(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
// @TODO: int overflow
Unit ExpressionConstantFolding;

 Interface
 Uses ExpressionOptimizer, Expression;

 { TExpressionConstantFolding }
 Type TExpressionConstantFolding =
      Class (TExpressionOptimizer)
       Private
        Function Parse(var Node: PExpressionNode): Boolean;

       Public
        Function Execute(var Node: PExpressionNode): Boolean; override;
       End;

 Implementation
Uses Messages;

(* TExpressionConstantFolding.Parse *)
Function TExpressionConstantFolding.Parse(var Node: PExpressionNode): Boolean;
Var Tmp, Tmp2, Left, Right: PExpressionNode;
    I                     : int16;
Begin
 Result := False;

 if (Node = nil) Then // nothing to do
  Exit;

 Left  := Node^.Left;
 Right := Node^.Right;

 if (Left = nil) and (Right = nil) Then // nothing to be done
  Exit;

 Parse(Left);
 Parse(Right);

 For I := Low(Node^.ParamList) To High(Node^.ParamList) Do
  Parse(Node^.ParamList[I]);

 if (Left <> nil) and (Left^.Typ = mtIdentifier) and (Left^.Value <> null) and (not (Node^.Typ in MLValueOperators)) Then
 Begin
  Left^.Typ   := Left^.IdentType;
  Left^.Left  := nil;
  Left^.Right := nil;
 End;

 if (Right <> nil) and (Right^.Typ = mtIdentifier) and (Right^.Value <> null) Then
 Begin
  Right^.Typ   := Right^.IdentType;
  Right^.Left  := nil;
  Right^.Right := nil;
 End;

 (* binary operators *)
 if (Left^.isConstant and Right^.isConstant) Then
 Begin
  if (Left^.Typ <> Right^.Typ) Then
  Begin
   if (Left^.Typ = mtInt) and (Right^.Typ = mtFloat) Then // int, float -> float, float
    Left^.Typ := mtFloat Else
   if (Left^.Typ = mtFloat) and (Right^.Typ = mtInt) Then // float, int -> float, float
    Right^.Typ := mtFloat Else
    Exit; // wrong types
  End;

  { bool }
  if (Left^.Typ = mtBool) Then
  Begin
   Result := True;

   Case Node^.Typ of
    mtXOR        { ^ } : Node^.Value := Left^.Value xor Right^.Value;
    mtLogicalOR  { || }: Node^.Value := Left^.Value or Right^.Value;
    mtLogicalAND { && }: Node^.Value := Left^.Value and Right^.Value;

    else
     Result := False;
   End;
  End;

  { int }
  if (Left^.Typ = mtInt) Then
  Begin
   Result := True;

   Case Node^.Typ of
    mtMod { % }:
     if (Right^.Value = 0) Then
      Compiler.CompileError(Node^.Token, eDivByZero, []) Else
      Node^.Value := Left^.Value mod Right^.Value;

    mtSHL { << }: Node^.Value := Left^.Value shl Right^.Value;
    mtSHR { >> }: Node^.Value := Left^.Value shr Right^.Value;
    mtXOR { ^ } : Node^.Value := Left^.Value xor Right^.Value;

    mtBitwiseOR  { | }: Node^.Value := Left^.Value or Right^.Value;
    mtBitwiseAND { & }: Node^.Value := Left^.Value and Right^.Value;

    else
     Result := False;
   End;
  End;

  { int or float }
  if (Left^.Typ in [mtInt, mtFloat]) and (not Result) Then
  Begin
   Result := True;

   Case Node^.Typ of
    mtAdd { + }: Node^.Value := Left^.Value + Right^.Value;
    mtSub { - }: Node^.Value := Left^.Value - Right^.Value;
    mtMul { * }: Node^.Value := Left^.Value * Right^.Value;
    mtDiv { / }:
     if (Right^.Value = 0) Then
     Begin
      Compiler.CompileError(Node^.Token, eDivByZero, []);
     End Else
     Begin
      Node^.Value := Left^.Value / Right^.Value;

      if (Left^.Typ = mtInt) and (Right^.Typ = mtInt) Then
       Node^.Value := Round(Node^.Value);
     End;

    mtLower        { <  }: Node^.Value := Left^.Value < Right^.Value;
    mtLowerEqual   { <= }: Node^.Value := Left^.Value <= Right^.Value;
    mtEqual        { == }: Node^.Value := Left^.Value = Right^.Value;
    mtGreater      { >  }: Node^.Value := Left^.Value > Right^.Value;
    mtGreaterEqual { >= }: Node^.Value := Left^.Value >= Right^.Value;

    else
     Result := False;
   End;

   if (Node^.Typ in [mtLower, mtLowerEqual, mtEqual, mtGreater, mtGreaterEqual]) Then
    Left^.Typ := mtBool;
  End;

  { string }
  if (Left^.Typ = mtString) Then
  Begin
   Result := True;

   Case Node^.Typ of
    mtAdd { + }: Node^.Value := Left^.Value + Right^.Value;

    else
     Result := False;
   End;
  End;

  if (Result) Then
  Begin
   Node^.Typ   := Left^.Typ;
   Node^.Left  := nil;
   Node^.Right := nil;

//   Dispose(Left);
//   Dispose(Right);
  End Else

   if (RaiseErrors) Then
    Compiler.CompileError(Node^.Token, eUnsupportedOperator, [ExpressionNodeString[Left^.Typ], ExpressionNodeString[Node^.Typ], ExpressionNodeString[Right^.Typ]]);
 End;

 (* unary operators *)
 if (Left^.isConstant) and
    (Node^.Typ in [mtNeg, mtLogicalNOT, mtBitwiseNOT]) Then
 Begin
  if (Node^.Typ = mtNeg) Then { - }
  Begin
   { bool, int, float }
   if (Left^.Typ in [mtBool, mtInt, mtFloat]) Then
   Begin
    Result   := True;
    Node^.Value := -Left^.Value;
   End;
  End;

  { bool }
  if (Left^.Typ = mtBool) and (not Result) Then
  Begin
   Result := True;

   Case Node^.Typ of
    mtLogicalNOT { ! }: Node^.Value := not Left^.Value;

    else
     Result := False;
   End;
  End;

  { int }
  if (Left^.Typ = mtInt) and (not Result) Then
  Begin
   Result := True;

   Case Node^.Typ of
    mtBitwiseNOT { ~ }: Node^.Value := not Left^.Value;

    else
     Result := False;
   End;
  End;

  if (Result) Then
  Begin
   Node^.Left := nil;
   Node^.Typ  := Left^.Typ;

//   Dispose(Left);
  End Else

   if (RaiseErrors) Then
    Compiler.CompileError(Node^.Token, eUnsupportedUOperator, [ExpressionNodeString[Node^.Typ], ExpressionNodeString[Left^.Typ]]);
 End;

 // ----------------------------------------- //

 { try to compute expressions like `numvar *= 5;` at compile time (if `numvar` is known) }
 For I := Low(OperatorSimplifyData1) To High(OperatorSimplifyData1) Do
  if (Node^.Typ = OperatorSimplifyData1[I].After) Then
  Begin
   if (Node^.Left^.IdentValue = null) Then // if value of 'numvar' isn't known, give up
    Break;

   // otherwise, optimize
   New(Tmp);
   Tmp^ := Node^.Left^; // do copy of left node, in case parsing failed

   Node^.Typ := OperatorSimplifyData1[I].Before;

   Node^.Left^.Typ   := Node^.Left^.IdentType;
   Node^.Left^.Value := Node^.Left^.IdentValue;

   if (Parse(Node)) Then // if optimization succeeded
   Begin
    Left^.SSA := Left^.PostSSA;
    Left^.Typ := mtIdentifier;

    New(Tmp2);
    Tmp2^.Left  := Left;
    Tmp2^.Right := Node;
    Tmp2^.Token := Node^.Token;
    Tmp2^.Typ   := mtAssign;

    Node := Tmp2;
   End Else
   Begin // couldn't optimize - restore previous vaules
    Node^.Typ   := OperatorSimplifyData1[I].After;
    Node^.Left^ := Tmp^;
   End;
  End;

 { try to compute "numvar++", "++numvar", "numvar--" and "--numvar" at compile time, if "numvar" is known }
 {if (Node^.Typ in [mtPostInc, mtPostDec, mtPreInc, mtPreDec]) and (TSymbol(Node^.Symbol).mVariable.Typ.isNumerical) Then
 Begin
  @TODO
 End;}
End;

(* TExpressionConstantFolding.Execute *)
Function TExpressionConstantFolding.Execute(var Node: PExpressionNode): Boolean;
Begin
 if (Node <> nil) and (Node^.Left = nil) and (Node^.Right = nil) and (Node^.Typ = mtIdentifier) and (Node^.Value <> null) Then // @TODO: this is ugly
  Node^.Typ := Node^.IdentType;

 Result := Parse(Node);
End;

End.
