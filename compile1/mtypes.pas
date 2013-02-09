(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit MTypes;

 Interface
 Uses Tokens;

 Type TVType       = Integer; // must be a `signed` type (as some functions from compiler returns eg.`-1` when a variable is not found, and we don't want any overflows...)
      TMVisibility = (mvPublic, mvPrivate);

 // TMString
 Type TMString = Record
                  Name, Value: String;
                  Pos        : LongWord; // position in `strings` section in the output file
                 End;

 // TMExport
 Type TMExport = Record
                  Name   : String;
                  NamePos: LongWord;
                  Pos    : Integer;
                 End;

 // TMImport
 Type TMImportFunc = Record
                      FuncName: String;
                      FuncPos : LongWord;
                     End;

 Type TMImport = Record
                  FuncCount: LongWord;
                  FuncList : Array of TMImportFunc;
                 End;

 // TMType
 Type TMType = Record
                Name     : String;
                RegPrefix: Char;

                InternalID: Byte;

                ArrayBase    : TVType; // array base type (it's in most cases a primary type, like `int` or `char`); it CANNOT be any array-derived type!
                ArrayDimCount: Byte; // array dimension amount

                isStrict: Boolean;
               End;

 // TMExpression
 Type TMExpressionType = (mtNothing, mtVariable, mtFunction, mtTree, mtOpeningBracket, mtClosingBracket, mtOpeningBracket2, mtClosingBracket2, mtArrayElement,
                          mtBool, mtChar, mtInt, mtFloat, mtString,
                          mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq,
                          mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent,
                          mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtXOR, mtSHL, mtSHR, mtSHLEq, mtSHREq,
                          mtNeg, mtLogicalNOT, mtBitwiseNOT, mtPreInc, mtPreDec, mtPostInc, mtPostDec,
                          mtNew);

 Const MExpressionDisplay: Array[TMExpressionType] of String =
 ('<nothing>', '<variable>', '<function>', '<tree>', '(', ')', '[', ']', '[]',
  '<bool value>', '<char value>', '<int value>', '<float value>', '<string value>',
  '+', '-', '*', '/', '%', '=', '+=', '-=', '*=', '/=', '%=',
  '<', '>', '==', '<=', '>=', '!=',
  '&&', '||', '&', '|', '^', '<<', '>>', '<<=', '>>=',
  '-', '!', '~', '++', '--', '++', '--',
  'new');

 Type TMExprSet = Set of TMExpressionType;

 Const MBinaryOperators: TMExprSet  = [mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent, mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtXOR, mtSHL, mtSHR, mtSHLEq, mtSHREq];
 Const MUnaryOperators: TMExprSet   = [mtNeg, mtLogicalNOT, mtBitwiseNOT, mtPreInc, mtPreDec, mtPostInc, mtPostDec];
 Const MCompareOperators: TMExprSet = [mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent];
 Const MOperators: TMExprSet        = [mtArrayElement, mtNew, mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent, mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtNeg, mtLogicalNOT, mtBitwiseNOT, mtXOR, mtSHL, mtSHR, mtSHLEq, mtSHREq, mtPreInc, mtPreDec, mtPostInc, mtPostDec];

 Type PMExpression = ^TMExpression;
      TMExpression = Record
                      Left, Right: PMExpression;

                      Typ      : TMExpressionType;
                      Value    : Variant;
                      Token    : TToken_P;
                      Deep     : Integer;
                      ParamList: Array of PMExpression; // for mtFunction

                      ResultOnStack: Boolean;
                     End;
 Type TMExpressionList = Array of TMExpression;

 // TMConstruction
 Type TMConstructionType = (ctJump, ctLabel, ctExpression, ctReturn, ctVoidReturn, ctInlineBytecode, ctFOR, ctFOR_end, ctIF, ctIF_end, ctIF_else, ctWHILE, ctWHILE_end, ct_DO_WHILE, ct_DO_WHILE_end, ctDelete);
 Type PMConstruction = ^TMConstruction;
      TMConstruction = Record
                        Typ   : TMConstructionType;
                        Values: Array of Pointer;
                       End;
 Type TMConstructionList = Array of TMConstruction;

 // TMVariable
 Type PMVariable = ^TMVariable;
      TMVariable = Record
                    RegID  : Integer; // negative values and zero for stack position, positive values for register id
                    RegChar: Char;

                    Name : String;
                    Typ  : TVType;
                    Value: TMExpression;
                    Deep : Integer;

                    isParam: Boolean;
                    isConst: Boolean;

                    Visibility: TMVisibility; // used (as for now) only for global constant; work's the same way as the function visibility
                   End;
 Type TMVariableList = Array of TMVariable;

 // primary types; order is important (as it is the same in the virtual machine)!
 Const TYPE_ANY    = 0;
       TYPE_VOID   = 1;
       TYPE_BOOL   = 2;
       TYPE_CHAR   = 3;
       TYPE_INT    = 4;
       TYPE_FLOAT  = 5;
       TYPE_STRING = 6;

 // operators
 Operator = (A, B: TMType): Boolean;

 // functions
 Function getEmptyType: TMType;

 Implementation

(* TMType = TMType *)
{
 Compares two TMType-d variables (except their names)
}
Operator = (A, B: TMType): Boolean;
Begin
 Result :=
 (A.ArrayDimCount = B.ArrayDimCount) and
 (A.ArrayBase     = B.ArrayBase) and
 (A.InternalID    = B.InternalID) and
 (A.isStrict      = B.isStrict) and
 (A.RegPrefix     = B.RegPrefix);
End;

(* getEmptyType *)
{
 Returns an empty type; i.e. - each field is zeroed, nil-ed or falsed
}
Function getEmptyType: TMType;
Begin
 Result.Name          := '';
 Result.ArrayDimCount := 0;
 Result.ArrayBase     := -1;
 Result.InternalID    := 0;
 Result.isStrict      := False;
 Result.RegPrefix     := #0;
End;

End.
