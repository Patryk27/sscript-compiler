(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit MTypes;

 Interface
 Uses Tokens;

 Type TMIntegerArray = Array of Integer;

 // TMScopeType
 Type TMScopeType = (sFunction, sFOR, sIF, sWHILE, sTryCatch);

 // TMScope
 Type TMScope = Record
                 Typ               : TMScopeType;
                 LoopBegin, LoopEnd: String;
                End;

 // TMExpression
 Type TMExpressionType = (mtNothing, mtVariable, mtFunctionCall, mtMethodCall, mtTypeCast, mtTree, mtType, mtOpeningBracket, mtClosingBracket, mtOpeningBracket2, mtClosingBracket2, mtArrayElement,
                          mtBool, mtChar, mtInt, mtFloat, mtString,
                          mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq,
                          mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent,
                          mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtXOR, mtSHL, mtSHR, mtSHLEq, mtSHREq,
                          mtNeg, mtLogicalNOT, mtBitwiseNOT, mtPreInc, mtPreDec, mtPostInc, mtPostDec,
                          mtNew);

 Const MExpressionDisplay: Array[TMExpressionType] of String =
 ('<nothing>', '<variable>', '<function call>', '<method call>', '<type cast>', '<tree>', '<type>', '(', ')', '[', ']', '[]',
  'bool', 'char', 'int', 'float', 'string',
  '+', '-', '*', '/', '%', '=', '+=', '-=', '*=', '/=', '%=',
  '<', '>', '==', '<=', '>=', '!=',
  '&&', '||', '&', '|', '^', '<<', '>>', '<<=', '>>=',
  '-', '!', '~', '++', '--', '++', '--',
  'new');

 Type TMExprSet = Set of TMExpressionType;

 Const MBinaryOperators: TMExprSet  = [mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent, mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtXOR, mtSHL, mtSHR, mtSHLEq, mtSHREq];
 Const MUnaryOperators: TMExprSet   = [mtNeg, mtLogicalNot, mtBitwiseNot, mtPreInc, mtPreDec, mtPostInc, mtPostDec];
 Const MCompareOperators: TMExprSet = [mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent];
 Const MOperators: TMExprSet        = [mtArrayElement, mtNew, mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq, mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent, mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtNeg, mtLogicalNOT, mtBitwiseNOT, mtXOR, mtSHL, mtSHR, mtSHLEq, mtSHREq, mtPreInc, mtPreDec, mtPostInc, mtPostDec];


 Type PMExpression = ^TMExpression;
      TMExpression = Record
                      Left, Right: PMExpression;

                      Typ      : TMExpressionType;
                      VarName  : String;
                      Value    : Variant;
                      Token    : TToken_P;
//                      Range    : TRange;
                      ParamList: Array of PMExpression; // for mtFunctionCall

                      ResultOnStack: Boolean;

                      Namespaces: TMIntegerArray; // function calls, variables and constants only

                      IdentNamespace, IdentID: Integer;
                      isLocal                : Boolean;

                      InTryCatch: Boolean;
                     End;
 Type TMExpressionList = Array of TMExpression;

 // TMConstruction
 Type TMConstructionType = (ctJump, ctLabel, ctExpression, ctReturn, ctVoidReturn, ctInlineBytecode, ctFOR, ctFOR_end, ctIF, ctIF_end, ctIF_else, ctWHILE, ctWHILE_end, ct_DO_WHILE, ct_DO_WHILE_end, ctDELETE, ctTHROW, ctTRY, ctCATCH, ctCATCH_end);
 Type PMConstruction = ^TMConstruction;
      TMConstruction = Record
                        Typ   : TMConstructionType;
                        Values: Array of Pointer;
                        Token : PToken_P;
                       End;
 Type TMConstructionList = Array of TMConstruction;

 // primary types; order (those numbers) is important, as it is the same in the virtual machine!
 Const PrimaryTypeNames: Array[0..7] of String = ('any', 'null', 'void', 'bool', 'char', 'int', 'float', 'string');
 Const TYPE_ANY_id    = 0;
       TYPE_NULL_id   = 1;
       TYPE_VOID_id   = 2;
       TYPE_BOOL_id   = 3;
       TYPE_CHAR_id   = 4;
       TYPE_INT_id    = 5;
       TYPE_FLOAT_id  = 6;
       TYPE_STRING_id = 7;

 Implementation

End.
