(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
Unit Expression;

 Interface
 Uses Serialization, Tokens, SysUtils, Variants;

 Type TIntegerArray = Array of Integer;

 { EExpressionNodeException }
 Type EExpressionNodeException = Class(Exception);

 { TExpressionNodeType }
 Type TExpressionNodeType =
      (
       mtNothing, mtIdentifier, mtFunctionCall, mtMethodCall, mtTypeCast, mtArrayElement,
       mtBool, mtChar, mtInt, mtFloat, mtString,
       mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq,
       mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent,
       mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtXOR, mtSHL, mtSHR,
       mtSHLEq, mtSHREq, mtOREq, mtANDEq, mtXOREq,
       mtNeg, mtLogicalNOT, mtBitwiseNOT, mtPreInc, mtPreDec, mtPostInc, mtPostDec,
       mtNew
      );

 { TExpressionNodeTypeSet }
 Type TExpressionNodeTypeSet = Set of TExpressionNodeType;

 Const ExpressionNodeString: Array[TExpressionNodeType] of String =
 (
  '<nothing>', '<identifier>', '<function call>', '<method call>', '<type cast>', '[]',
  'bool', 'char', 'int', 'float', 'string',
  '+', '-', '*', '/', '%', '=', '+=', '-=', '*=', '/=', '%=',
  '<', '>', '==', '<=', '>=', '!=',
  '&&', '||', '&', '|', '^', '<<', '>>',
  '<<=', '>>=', '|=', '&=', '^=',
  '-', '!', '~', '++', '--', '++', '--',
  'new'
 );

 Const MLValueOperators: TExpressionNodeTypeSet = [mtAssign, mtPreInc, mtPostInc, mtPreDec, mtPostDec, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtShlEq, mtShrEq, mtOREq, mtANDEq, mtXOREq];

 { TSSAVarID }
 Type PSSAVarID = ^TSSAVarID;
      TSSAVarID =
      Record
       Values: Array of uint32;
      End;

 { TExpressionNode }
 Type PExpressionNode = ^TExpressionNode;
      TExpressionNode =
      Record
       Public
        Left, Right: PExpressionNode;

        Typ      : TExpressionNodeType;
        Value    : Variant;
        Token    : TToken_P;

        IdentName : String; // used in expression folding
        IdentType : TExpressionNodeType; // used in expression folding
        IdentValue: Variant; // used in late expression folding; value put here is fetched by the constant propagation algorithm

        ParamList: Array of PExpressionNode; // for mtFunctionCall

        Symbol      : TObject;
        SSA, PostSSA: TSSAVarID;

        ResultOnStack: Boolean; // equal 'true' if expression's result is left on the stack

        isNull: Boolean; // special case for the "null" variable

       Public
        Class Function Create(const fType: TExpressionNodeType=mtNothing; const fToken: PToken_P=nil): PExpressionNode; static;
        Class Function Create(const fType: TExpressionNodeType; const fValue: Variant; const fToken: PToken_P=nil): PExpressionNode; static;

        Class Function Create(const Node: Serialization.TNode): PExpressionNode; static;

        Function FindAssignment(const VarSymbol: TObject; const CheckAllLValueOperators: Boolean=False): PExpressionNode;
        Procedure RemoveAssignments(const VarSymbol: TObject);

        Function getType: TExpressionNodeType;

        Function containsCall: Boolean;
        Function hasValue: Boolean;
        Function isConstant: Boolean;

        Function getSerializedForm: String;
       End;

 // primary types; order (those numbers) is important, as it is the same in the virtual machine!
 Const PrimaryTypeNames: Array[0..6] of String = ('bool', 'char', 'int', 'float', 'string', 'any', 'void');
 Const TYPE_BOOL_id   = 0;
       TYPE_CHAR_id   = 1;
       TYPE_INT_id    = 2;
       TYPE_FLOAT_id  = 3;
       TYPE_STRING_id = 4;

       TYPE_ANY_id  = 5;
       TYPE_VOID_id = 6;

 Operator = (A, B: TSSAVarID): Boolean;
 Operator in (A: Integer; B: Array of LongWord): Boolean;

 Implementation
Uses symdef;

(* TSSAVarID = TSSAVarID *)
Operator = (A, B: TSSAVarID): Boolean;
Var I: Integer;
Begin
 if (Length(A.Values) <> Length(B.Values)) Then
  Exit(False);

 For I := Low(A.Values) To High(A.Values) Do
  if (A.Values[I] <> B.Values[I]) Then
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
(* TExpressionNode.Create *)
Class Function TExpressionNode.Create(const fType: TExpressionNodeType; const fToken: PToken_P): PExpressionNode;
Begin
 New(Result);

 With Result^ do
 Begin
  Left  := nil;
  Right := nil;

  Typ   := fType;
  Value := 0;

  IdentName  := '';
  IdentType  := mtNothing;
  IdentValue := null;

  SetLength(ParamList, 0);
  SetLength(SSA.Values, 0);
  SetLength(PostSSA.Values, 0);

  ResultOnStack := False;

  Symbol := nil;
  isNull := False;
 End;

 if (fToken <> nil) Then
  Result^.Token := fToken^;
End;

(* TExpressionNode.Create *)
Class Function TExpressionNode.Create(const fType: TExpressionNodeType; const fValue: Variant; const fToken: PToken_P): PExpressionNode;
Begin
 Result        := TExpressionNode.Create(fType, fToken);
 Result^.Value := fValue;
End;

(* TExpressionNode.Create *)
{
 Creates a contant node from serialized form of expression.
}
Class Function TExpressionNode.Create(const Node: Serialization.TNode): PExpressionNode;
Begin
 Result := TExpressionNode.Create(TExpressionNodeType(Node[0].getInt), Node[1].getVariant);
End;

(* TExpressionNode.FindAssignment *)
{
 Finds the first assignment to a variable named `VarName` and returns it, otherwise returns `nil`.
 When `CheckAllLValueOperators` is `true`, returns first MLValueOperators occurence.

 @TODO: arrays!
}
Function TExpressionNode.FindAssignment(const VarSymbol: TObject; const CheckAllLValueOperators: Boolean): PExpressionNode;
Var I: Integer;
Begin
 Result := nil;

 if (Typ = mtAssign) and (Left^.Symbol = VarSymbol) Then
  Exit(@self);

 if (CheckAllLValueOperators) Then
 Begin
  if (Typ in MLValueOperators) and (Left^.Symbol = VarSymbol) Then
   Exit(@self);
 End;

 if (Result = nil) and (Left <> nil) Then
  Result := Left^.FindAssignment(VarSymbol, CheckAllLValueOperators);

 if (Result = nil) and (Right <> nil) Then
  Result := Right^.FindAssignment(VarSymbol, CheckAllLValueOperators);

 For I := Low(ParamList) To High(ParamList) Do
 Begin
  if (Result = nil) Then
   Result := ParamList[I]^.FindAssignment(VarSymbol, CheckAllLValueOperators) Else
   Break;
 End;
End;

(* TExpressionNode.RemoveAssignments *)
{
 Removes all assignments to variable named `VarName`, but leaves the right side of the assignment.
}
Procedure TExpressionNode.RemoveAssignments(const VarSymbol: TObject);
Var Assign: PExpressionNode;
Begin
 While (true) Do
 Begin
  Assign := FindAssignment(VarSymbol);

  if (Assign = nil) Then
   Break;

  Assign^ := Assign^.Right^;
 End;
End;

(* TExpressionNode.getType *)
{
 Returns either 'mtNothing' or value from 'mtBool'..'mtString', depending on expression type.
 Does a small expression evaluation thus it shouldn't be called often.

 @TODO: function and method calls
}
Function TExpressionNode.getType: TExpressionNodeType;
Var LT, RT: TExpressionNodeType;
    mVar  : TVariable;
Begin
 Result := mtNothing;

 if (@self = nil) Then
  Exit;

 if (Typ in [mtFunctionCall, mtMethodCall]) Then // @TODO
  Exit;

 if (Typ in [mtBool, mtChar, mtInt, mtFloat, mtString]) Then
  Exit(Typ);

 if (Typ = mtIdentifier) Then
 Begin
  if (Symbol = nil) Then
   Exit;

  mVar := (Symbol as TSymbol).mVariable;

  if (mVar.Typ.isBool) Then
   Exit(mtBool);

  if (mVar.Typ.isChar) Then
   Exit(mtChar);

  if (mVar.Typ.isInt) Then
   Exit(mtInt);

  if (mVar.Typ.isFloat) Then
   Exit(mtFloat);

  if (mVar.Typ.isString) Then
   Exit(mtString);

  Exit;
 End;

 LT := Left^.getType;
 RT := Right^.getType;

 if (Typ in [mtLogicalNOT, mtBitwiseNOT, mtNEG]) Then
  Exit(LT);

 if (LT in [mtInt, mtFloat]) and (RT in [mtInt, mtFloat]) and (LT <> RT) Then
  Exit(mtFloat);

 if (LT = RT) Then
  Exit(LT);

 Exit(mtNothing);
End;

(* TExpressionNode.containsCall *)
{
 Returns `true` if the expression or any of its children (and children's children, and so on) is a function or method call.
}
Function TExpressionNode.containsCall: Boolean;
Begin
 Result := False;

 if (Typ = mtFunctionCall) or (Typ = mtMethodCall) Then
  Exit(True);

 if (Left <> nil) Then
  Result := Result or Left^.containsCall;

 if (Right <> nil) Then
  Result := Result or Right^.containsCall;
End;

(* TExpressionNode.hasValue *)
{
 Returns `true` when this expression has a value (directly or not).
 These are: function and method calls, variables and constants.
}
Function TExpressionNode.hasValue: Boolean;
Begin
 if (@self = nil) Then
  Exit(False);

 Result := (Typ in [mtFunctionCall, mtMethodCall, mtIdentifier, mtBool, mtChar, mtInt, mtFloat, mtString]);
End;

(* TExpressionNode.isConstant *)
{
 Returns `true` when the expression is a constant value.
 Expression must be already folded.
}
Function TExpressionNode.isConstant: Boolean;
Begin
 if (@self = nil) Then
  Exit(False);

 Result := (Left = nil) and (Right = nil) and (Typ in [mtBool, mtChar, mtInt, mtFloat, mtString]);
End;

(* TExpressionNode.getSerializedForm *)
{
 Returns serialized form of a constant node.
 If node isn't a contant, throws exception.
}
Function TExpressionNode.getSerializedForm: String;
Begin
 if (not isConstant) Then
  raise EExpressionNodeException.Create('Cannot generate serialized form of not constant expression!');

 Result := Format('(%d$%s)', [ord(Typ), VarToStr(Value)]);
End;
End.
