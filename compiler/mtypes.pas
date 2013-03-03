(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit MTypes;

 Interface
 Uses Tokens;

 Type TMVisibility = (mvPublic, mvPrivate);

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

 // PMType
 Type PMType = ^TMType;

 // TMParam
     TMParam = Record
                 Name: String;
                 Typ : PMType;
                End;
      TMParamList = Array of TMParam;

 // TMType
      TMType = Record
                Name     : String;
                RegPrefix: Char;

                InternalID: Byte;

                ArrayBase    : PMType; // array base type (it's in most cases a primary type, like `int` or `char`); it CANNOT be any array-derived type!
                ArrayDimCount: Byte; // array dimension amount

                FuncReturn: PMType;
                FuncParams: TMParamList;

                isStrict, isFunction: Boolean;

                isUnspecialized: Boolean;
                {
                 Unspecialied types are those, who are only eg."a function".
                 Not "a function returning `int` with parameters (...)" - just some function-type.
                 Like:
                 function<void> do_func(function func)
                 {
                  func();
                 }
                }

                Visibility: TMVisibility;

                mCompiler: Pointer;
                DeclToken: TToken_P;
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
                    Typ  : PMType;
                    Value: PMExpression;
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
                    Return       : PMType; // return type
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
 Type TMGlobalDeclarationType = (gdConstant, gdVariable, gdFunction, gdType);
 Type TMGlobalDeclaration = Record
                             Typ: TMGlobalDeclarationType;

                             mVariable: TMVariable; // gdConstant, gdVariable
                             mFunction: TMFunction; // gtFunction
                             mType    : TMType;

                             isInternal: Boolean; // eg.`null` is internal
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
 Const PrimaryTypeNames: Array[0..7] of String = ('any', 'null', 'void', 'bool', 'char', 'int', 'float', 'string');
 Const TYPE_ANY_id    = 0;
       TYPE_NULL_id   = 1;
       TYPE_VOID_id   = 2;
       TYPE_BOOL_id   = 3;
       TYPE_CHAR_id   = 4;
       TYPE_INT_id    = 5;
       TYPE_FLOAT_id  = 6;
       TYPE_STRING_id = 7;

 // operators
 Operator = (A, B: TMType): Boolean;

 // functions
 Function getEmptyType: TMType;
 Function TYPE_ANY: TMType;
 Function TYPE_NULL: TMType;
 Function TYPE_VOID: TMType;
 Function TYPE_BOOL: TMType;
 Function TYPE_CHAR: TMType;
 Function TYPE_INT: TMType;
 Function TYPE_FLOAT: TMType;
 Function TYPE_STRING: TMType;

 Function TypeInstance(Typ: TMType): PMType;

 Implementation

(* TMType = TMType *)
{
 Compares two TMType-d variables (except their names)
}
Operator = (A, B: TMType): Boolean;
Begin
 Result :=
 (A.ArrayDimCount = B.ArrayDimCount) and
 (A.InternalID    = B.InternalID) and
 (A.isStrict      = B.isStrict) and
 (A.isFunction    = B.isFunction) and
 (A.RegPrefix     = B.RegPrefix);

 if (not Result) Then
  Exit(False);

 if (A.ArrayBase <> nil) and (B.ArrayBase <> nil) Then
  Result := Result and (A.ArrayBase^ = B.ArrayBase^);

 if (A.FuncReturn <> nil) and (B.FuncReturn <> nil) Then
  Result := Result and (A.FuncReturn^ = B.FuncReturn^);

 Result := Result and (Length(A.FuncParams) = Length(B.FuncParams));
End;

(* getEmptyType *)
{
 Returns an empty type; i.e. - each field is zeroed, nil-ed or falsed
}
Function getEmptyType: TMType;
Begin
 With Result do
 Begin
  Name            := '';
  RegPrefix       := #0;
  ArrayDimCount   := 0;
  ArrayBase       := nil;
  InternalID      := 0;
  FuncReturn      := nil;
  isStrict        := False;
  isFunction      := False;
  Visibility      := mvPublic;
  isUnspecialized := False;

  SetLength(FuncParams, 0);
 End;
End;

(* TYPE_ANY *)
Function TYPE_ANY: TMType;
Begin
 Result := getEmptyType;

 With Result do
 Begin
  Name       := 'any';
  RegPrefix  := 'i';
  InternalID := TYPE_ANY_id;
 End;
End;

(* TYPE_NULL *)
Function TYPE_NULL: TMType;
Begin
 Result := getEmptyType;

 With Result do
 Begin
  Name       := 'null';
  RegPrefix  := 'r';
  InternalID := TYPE_NULL_id;
 End;
End;

(* TYPE_VOID *)
Function TYPE_VOID: TMType;
Begin
 Result := getEmptyType;

 With Result do
 Begin
  Name       := 'void';
  RegPrefix  := 'i';
  InternalID := TYPE_VOID_id;
 End;
End;

(* TYPE_BOOL *)
Function TYPE_BOOL: TMType;
Begin
 Result := getEmptyType;

 With Result do
 Begin
  Name       := 'bool';
  RegPrefix  := 'b';
  InternalID := TYPE_BOOL_id;
 End;
End;

(* TYPE_CHAR *)
Function TYPE_CHAR: TMType;
Begin
 Result := getEmptyType;

 With Result do
 Begin
  Name       := 'char';
  RegPrefix  := 'c';
  InternalID := TYPE_CHAR_id;
 End;
End;

(* TYPE_INT *)
Function TYPE_INT: TMType;
Begin
 Result := getEmptyType;

 With Result do
 Begin
  Name       := 'int';
  RegPrefix  := 'i';
  InternalID := TYPE_INT_id;
 End;
End;

(* TYPE_FLOAT *)
Function TYPE_FLOAT: TMType;
Begin
 Result := getEmptyType;

 With Result do
 Begin
  Name       := 'float';
  RegPrefix  := 'f';
  InternalID := TYPE_FLOAT_id;
 End;
End;

(* TYPE_STRING *)
Function TYPE_STRING: TMType;
Begin
 Result := getEmptyType;

 With Result do
 Begin
  Name       := 'string';
  RegPrefix  := 's';
  InternalID := TYPE_STRING_id;

  ArrayBase     := TypeInstance(TYPE_STRING); // why not `TYPE_CHAR`? Well... it's a long story with many monsters, goblins and Access Violations.
  ArrayDimCount := 1;
 End;
End;

(* TypeInstance *)
Function TypeInstance(Typ: TMType): PMType;
Begin
 New(Result);
 Result^ := Typ;
End;

End.
