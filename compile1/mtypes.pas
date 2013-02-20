(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit MTypes;

 Interface
 Uses Tokens;

 Type TVType       = Integer; // must be a `signed` type (as some functions from compiler returns eg.`-1` when a variable is not found, and we don't want any integer overflows...)
      TMVisibility = (mvPublic, mvPrivate);

      TMIntegerArray = Array of Integer;

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

 // TMParam
 Type TMParam = Record
                 Name: String;
                 Typ : TVType;
                End;
 Type TMParamList = Array of TMParam;

 // TMType
 Type TMType = Record
                Name     : String;
                RegPrefix: Char;

                InternalID: Byte;

                ArrayBase    : TVType; // array base type (it's in most cases a primary type, like `int` or `char`); it CANNOT be any array-derived type!
                ArrayDimCount: Byte; // array dimension amount

                FuncReturn: TVType;
                FuncParams: TMParamList;

                isStrict, isFunction: Boolean;
               End;

 // TMExpression
 Type TMExpressionType = (mtNothing, mtVariable, mtFunctionCall, mtTypeCast, mtTree, mtOpeningBracket, mtClosingBracket, mtOpeningBracket2, mtClosingBracket2, mtArrayElement,
                          mtBool, mtChar, mtInt, mtFloat, mtString,
                          mtAdd, mtSub, mtMul, mtDiv, mtMod, mtAssign, mtAddEq, mtSubEq, mtMulEq, mtDivEq, mtModEq,
                          mtLower, mtGreater, mtEqual, mtLowerEqual, mtGreaterEqual, mtDifferent,
                          mtLogicalAND, mtLogicalOR, mtBitwiseAND, mtBitwiseOR, mtXOR, mtSHL, mtSHR, mtSHLEq, mtSHREq,
                          mtNeg, mtLogicalNOT, mtBitwiseNOT, mtPreInc, mtPreDec, mtPostInc, mtPostDec,
                          mtNew);

 Const MExpressionDisplay: Array[TMExpressionType] of String =
 ('<nothing>', '<variable>', '<function call>', '<type cast>', '<tree>', '(', ')', '[', ']', '[]',
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

                      Namespaces: TMIntegerArray; // function calls, variables and constants only

                      IdentID, IdentNamespace: Integer;
                      isLocal                : Boolean;
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
                    RegID  : Integer; // negative values and zero for stack position, positive values for register ID (1..4)
                    RegChar: Char;

                    Name : String;
                    Typ  : TVType;
                    Value: TMExpression;
                    Deep : Integer;

                    isParam: Boolean;
                    isConst: Boolean;

                    Visibility: TMVisibility; // as for now, used only for global constants; works the same way as the function visibility

                    mCompiler: Pointer;
                    DeclToken: TToken_P;
                   End;
 Type TMVariableList = Array of TMVariable;

 // TMFunction
 Type PMFunction = ^TMFunction;
      TMFunction = Record
                    Name         : String; // function name
                    MName        : String; // mangled name (used as a label name)
                    ModuleName   : String; // module name in which function has been declared
                    NamespaceName: String;
                    Return       : TVType; // return type
                    ParamList    : TMParamList;

                    LibraryFile: String;

                    ConstructionList: TMConstructionList;
                    VariableList    : TMVariableList;

                    isNaked      : Boolean;
                    isDeclaration: Boolean;
                    Visibility   : TMVisibility;

                    mCompiler: Pointer;
                    DeclToken: TToken_P; // `{` token's line
                   End;

 // TMGlobalDeclaration
 Type TMGlobalDeclarationType = (gdConstant, gdVariable, gdFunction);
 Type TMGlobalDeclaration = Record
                             Typ: TMGlobalDeclarationType;

                             mVariable: TMVariable; // gdConstant, gdVariable
                             mFunction: TMFunction; // gtFunction
                            End;

 // TMNamespace
 Type PMNamespace = ^TMNamespace;
      TMNamespace = Record
                     Name: String;

                     GlobalList: Array of TMGlobalDeclaration; // declaration list

                     Visibility: TMVisibility;

                     mCompiler: Pointer;
                     DeclToken: TToken_P;
                    End;

 Type TMNamespaceArray = Array of TMNamespace;

 // primary types; order (those numbers) is important, as it is the same in the virtual machine!
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
 (A.isFunction    = B.isFunction) and
 (A.RegPrefix     = B.RegPrefix) and
 (A.FuncReturn    = B.FuncReturn) and

 (Length(A.FuncParams) = Length(B.FuncParams));
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
