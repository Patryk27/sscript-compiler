(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
Unit Expression;

 Interface
 Uses Tokens, FGL;

 Type TIntegerArray = Array of Integer;

 // TExpression
 Type TExpressionType = (mtNothing, mtVariable, mtFunctionCall, mtMethodCall, mtTypeCast, mtTree, mtType, mtOpeningBracket, mtClosingBracket, mtOpeningBracket2, mtClosingBracket2, mtArrayElement,
                         mtBool, mtChar, mtInt, mtFloat, mtString,
                         mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq,
                         mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent,
                         mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtXOR, mtSHL, mtSHR,
                         mtSHLEq, mtSHREq, mtOREq, mtANDEq, mtXOREq,
                         mtNeg, mtLogicalNOT, mtBitwiseNOT, mtPreInc, mtPreDec, mtPostInc, mtPostDec,
                         mtNew);

 Const ExpressionDisplay: Array[TExpressionType] of String =
 ('<nothing>', '<variable>', '<function call>', '<method call>', '<type cast>', '<tree>', '<type>', '(', ')', '[', ']', '[]',
  'bool', 'char', 'int', 'float', 'string',
  '+', '-', '*', '/', '%', '=', '+=', '-=', '*=', '/=', '%=',
  '<', '>', '==', '<=', '>=', '!=',
  '&&', '||', '&', '|', '^', '<<', '>>',
  '<<=', '>>=', '|=', '&=', '^=',
  '-', '!', '~', '++', '--', '++', '--',
  'new');

 Type TExpressionTypeSet = Set of TExpressionType;

 Const MBinaryOperators: TExpressionTypeSet  = [mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent, mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtXOR, mtSHL, mtSHR, mtSHLEq, mtSHREq, mtOREq, mtANDEq, mtXOREq];
 Const MUnaryOperators: TExpressionTypeSet   = [mtNeg, mtLogicalNot, mtBitwiseNot, mtPreInc, mtPreDec, mtPostInc, mtPostDec];
 Const MCompareOperators: TExpressionTypeSet = [mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent];
 Const MOperators: TExpressionTypeSet        = [mtArrayElement, mtNew, mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent, mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtNeg, mtLogicalNOT, mtBitwiseNOT, mtXOR, mtSHL, mtSHR, mtSHLEq, mtSHREq, mtOREq, mtANDEq, mtXOREq, mtPreInc, mtPreDec, mtPostInc, mtPostDec];

 Const MLValueOperators: TExpressionTypeSet = [mtAssign, mtPreInc, mtPostInc, mtPreDec, mtPostDec, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtShlEq, mtShrEq, mtOREq, mtANDEq, mtXOREq];

 Type PSSAVarID = ^TSSAVarID;
      TSSAVarID = Record
                   Typ  : (sstNone, sstSingle, sstPhi);
                   Value: Array of uint32;
                  End;

 Type PExpression = ^TExpression;
      TExpressionList = specialize TFPGList<PExpression>;

      TExpression = Record
                     Left, Right: PExpression;

                     Typ      : TExpressionType;
                     Value    : Variant;
                     Token    : TToken_P;

                     IdentName: String; // used in expression folding
                     IdentType: TExpressionType; // used in expression folding
                     ParamList: Array of PExpression; // for mtFunctionCall

                     ResultOnStack: Boolean;

                     Namespaces: TIntegerArray; // function calls, variables and constants only
                     Symbol    : Pointer;
                     isLocal   : Boolean;

                     SSA: TSSAVarID;

                     Function FindAssignment(const VarName: String): PExpression;
                     Procedure RemoveAssignments(const VarName: String);

                     Function HasCall: Boolean;
                     Function isConstant: Boolean;
                    End;

 // primary types; order (those numbers) is important, as it is the same in the virtual machine!
 Const PrimaryTypeNames: Array[0..7] of String = ('any', 'null', 'void', 'bool', 'char', 'int', 'float', 'string');
 Const TYPE_ANY_id    = 0;
       unused_0       = 1;
       TYPE_VOID_id   = 2;
       TYPE_BOOL_id   = 3;
       TYPE_CHAR_id   = 4;
       TYPE_INT_id    = 5;
       TYPE_FLOAT_id  = 6;
       TYPE_STRING_id = 7;

 Operator = (A, B: TSSAVarID): Boolean;
 Operator in (A: Integer; B: Array of LongWord): Boolean;

 Implementation
Uses SysUtils;

(* TSSAVarID = TSSAVarID *)
Operator = (A, B: TSSAVarID): Boolean;
Var I: Integer;
Begin
 if (Length(A.Value) <> Length(B.Value)) Then
  Exit(False);

 For I := Low(A.Value) To High(A.Value) Do
  if (A.Value[I] <> B.Value[I]) Then
   Exit(False);

 Exit(True);
End;

(* Integer in Array of Integer *)
Operator in (A: Integer; B: Array of LongWord): Boolean;
Var I: Integer;
Begin
 Result := False;

 For I in B Do
  if (I = A) Then
   Exit(True);
End;

// -------------------------------------------------------------------------- //
(* TExpression.FindAssignment *)
{
 Finds the first assignment to a variable named `VarName` and returns it, otherwise returns `nil`.

 @TODO: arrays!
}
Function TExpression.FindAssignment(const VarName: String): PExpression;
Var I: Integer;
Begin
 Result := nil;

 if (Typ = mtAssign) and (Left^.IdentName = VarName) Then
  Exit(@self);

 if (Result = nil) and (Left <> nil) Then
  Result := Left^.FindAssignment(VarName);

 if (Result = nil) and (Right <> nil) Then
  Result := Right^.FindAssignment(VarName);

 For I := Low(ParamList) To High(ParamList) Do
  if (Result = nil) Then
   Result := ParamList[I]^.FindAssignment(VarName);
End;

(* TExpression.RemoveAssignments *)
{
 Removes all assignments to variable named `VarName`.
}
Procedure TExpression.RemoveAssignments(const VarName: String);
Var Assign: PExpression;
Begin
 While (true) Do
 Begin
  Assign := FindAssignment(VarName);
  if (Assign = nil) Then
   Break;

  Assign^ := Assign^.Right^;
 End;
End;

(* TExpression.HasCall *)
{
 Returns `true` if the expression or any of its children (and children's children, and so on) is a function or method call.
}
Function TExpression.HasCall: Boolean;
Begin
 Result := False;

 if (Typ = mtFunctionCall) or (Typ = mtMethodCall) Then
  Exit(True);

 if (Left <> nil) Then
  Result := Result or Left^.HasCall;

 if (Right <> nil) Then
  Result := Result or Right^.Hascall;
End;

(* TExpression.isConstant *)
{
 Returns `true` when the expression is a constant value.
 Expression must be already folded.
}
Function TExpression.isConstant: Boolean;
Begin
 if (@self = nil) Then
  Exit(False);

 Result := (Left = nil) and (Right = nil) and (Typ in [mtBool, mtChar, mtInt, mtFloat, mtString]);
End;
End.
