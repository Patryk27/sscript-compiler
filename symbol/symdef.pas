(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit symdef;

 Interface
 Uses FGL, Expression, FlowGraph, Tokens;

 (* forward declarations *)
 Type TType      = class;
      TVariable  = class;
      TFunction  = class;
      TNamespace = class;
      TSymbol    = class;

 (* auxlialiry declarations *)
 Type TVisibility = (mvPublic, mvPrivate);

 Type TTypeAttributes = Set of (taStrict, taFunction, taEnum, taUnspecialized);
 Type TVariableAttributes = Set of (vaConst, vaEnumItem, vaFuncParam, vaDontAllocate, vaVolatile, vaCatchVar);
 Type TFunctionAttributes = Set of (faNaked);

 Type PRange = ^TRange;
      TRange = Record
                PBegin, PEnd: TToken_P;
               End;

 Type PStackSavedReg = ^TStackSavedReg;
      TStackSavedReg = Record
                        RegChar: Char;
                        RegID  : uint8;
                       End;

 Type TStackSavedRegs = specialize TFPGList<PStackSavedReg>;

 // TNamespaceVisibility
 Type PNamespaceVisibility = ^TNamespaceVisibility;
      TNamespaceVisibility = Record
                              Namespace: TNamespace;
                              Range    : TRange;
                             End;
 Type TNamespaceVisibilityList = specialize TFPGList<PNamespaceVisibility>;

 Type TRefSymbol = class;

 (* lists *)
 Type TNamespaceList = specialize TFPGList<TNamespace>;
      TVariableList  = specialize TFPGList<TVariable>;
      TSymbolList    = specialize TFPGList<TSymbol>;

 (* Type *)
 Type PParam = ^TParam;
      TParam = Record
                Name        : String;
                Typ         : TType;
                DefaultValue: PExpressionNode;
                Attributes  : TVariableAttributes;
                isConst     : Boolean;
                isVar       : Boolean; // `isPassByRef`
               End;
      TParamList = Array of TParam;

   // TType
 Type TType = Class
               Public
               // public fields
                RefSymbol: TRefSymbol;

                RegPrefix : Char;
                InternalID: uint8;

                ArrayBase    : TType; // array base type (it's in most cases a primary type, like `int` or `char`); it CANNOT be any array-derived type!
                ArrayDimCount: uint8; // array dimension count

                FuncReturn: TType;
                FuncParams: TParamList;

                EnumBase    : TType;
                EnumItemList: TVariableList;

                Attributes: TTypeAttributes;

               // public methods
                Constructor Create;

                Function isUnspecialized: Boolean;
                Function isStrict: Boolean;

                Function getBytecodeType: String;
                Function getLowerArray: TType;

                Function isAny: Boolean;
                Function isVoid: Boolean;
                Function isBool: Boolean;
                Function isChar: Boolean;
                Function isInt: Boolean;
                Function isFloat: Boolean;
                Function isString: Boolean;
                Function isNumerical: Boolean;
                Function isArray(const RegardStringAsArray: Boolean=True): Boolean;
                Function isObject: Boolean;
                Function isFunctionPointer: Boolean;
                Function isEnum: Boolean;
                Function isEnumItem: Boolean;

                Function CanBeAssignedTo(T2: TType): Boolean;
                Function CanBeCastedTo(T2: TType): Boolean;

                Function Clone: TType;
                Function asString: String;
               End;

 (* Variable *)
 Type TVariable = Class
                   Public
                   // public fields
                    RefSymbol: TRefSymbol;

                    MemPos: int16; // negative values and zero for stack position, positive values for register ID (1..4)

                    Typ  : TType;
                    Value: PExpressionNode; // if constant

                    isFreed: Boolean; // used for reference-counted variables; by default equal `false`

                    Attributes: TVariableAttributes;

                   // public methods
                    Constructor Create;

                    Function isConst: Boolean;
                    Function isFuncParam: Boolean;
                    Function isVolatile: Boolean;
                    Function isCatchVar: Boolean;
                    Function DontAllocate: Boolean;

                    Function getAllocationPos: String;
                   End;
 Type TMVariableList = Array of TVariable;

 (* Function *)
 Type TFunction = Class
                   Public
                   // public fields
                    RefSymbol: TRefSymbol;
                    RefVar   : TVariable;

                    ModuleName   : String; // module name in which function has been declared
                    MangledName  : String; // function mangled (label) name
                    NamespaceName: String; // namespace name in which function has been declared
                    LibraryFile  : String; // only for library-imported functions - library file name

                    Return: TType; // return type

                    ParamList : TParamList; // parameter list
                    SymbolList: TSymbolList; // local symbol list
                    FlowGraph : TCFGraph; // control flowgraph

                    StackSize: uint8;
                    StackRegs: TStackSavedRegs;

                    Attributes: TFunctionAttributes;

                   // public methods
                    Constructor Create;

                    Function isNaked: Boolean;

                    Function findSymbol(const SymName: String): TSymbol;
                    Function findSymbol(const SymName: String; const SymScope: TToken_P): TSymbol;
                   End;

 (* Namespace *)
 Type TNamespace = Class
                    Public
                    // methods
                     Constructor Create;

                    // fields
                    Var
                     RefSymbol : TRefSymbol;
                     SymbolList: TSymbolList; // global symbol list

                     Function findSymbol(const SymName: String): TSymbol;
                     Function findSymbol(const SymName: String; const SymScope: TToken_P): TSymbol;
                     Function findFunction(const FuncName: String): TFunction;
                    End;

 (* RefSymbol *)
 Type TRefSymbol = Class
                    Public
                     // public methods
                     Constructor Create;
                     Function Clone: TRefSymbol;
                     Procedure CopyTo(const Symbol: TRefSymbol);

                     Function isLocal: Boolean;
                     Function isGlobal: Boolean;

                     // public fields
                     Var
                      Name : String; // symbol name
                      Range: TRange; // accessability range

                      DeclNamespace: TNamespace; // namespace in which identifier has been declared
                      DeclFunction : TFunction; // function in which identifier has been declared

                      Visibility: TVisibility; // visibility
                      mCompiler : Pointer; // compiler in which symbol has been declared
                      DeclToken : PToken_P; // declaration token pointer

                      isInternal: Boolean; // eg.`null` is internal
                     End;

 (* Symbol *)
 Type TSymbolType = (stNamespace,  stFunction, stVariable, stConstant, stType);
 Type TSymbol = Class (TRefSymbol)
                 Public
                 // methods
                  Constructor Create(const SymbolType: TSymbolType; const CreateInstance: Boolean=True);
                  Constructor Create(const SymbolType: TSymbolType; const Instance: Pointer);
                  Constructor Create(const CloneOf: TSymbol);

                 // fields
                 Var
                  Typ: TSymbolType;

                  mNamespace: TNamespace;
                  mVariable : TVariable;
                  mFunction : TFunction;
                  mType     : TType;
                 End;

 // operators
 Function type_equal(A, B: TType): Boolean;
 Operator in (Token: TToken_P; Range: TRange): Boolean;

 // functions
 Function CreateFunctionMangledName(const Func: TFunction; FuncName: String; SimplifiedName: Boolean): String;

 Function TYPE_ANY: TType;
 Function TYPE_VOID: TType;
 Function TYPE_BOOL: TType;
 Function TYPE_CHAR: TType;
 Function TYPE_INT: TType;
 Function TYPE_FLOAT: TType;
 Function TYPE_STRING: TType;

 Implementation
Uses CompilerUnit, SSCompiler, ExpressionCompiler, Messages, SysUtils;

(* type_equal *)
{
 Compares two TTypes (except their names)
}
Function type_equal(A, B: TType): Boolean;
Var I: Integer;
Begin
 if (A = nil) or (B = nil) Then
  Exit(False);

 Result :=
 (A.ArrayDimCount = B.ArrayDimCount) and
 (A.InternalID    = B.InternalID) and
// (A.Attributes    = B.Attributes) and
 (A.RegPrefix     = B.RegPrefix);

 if (not Result) Then
  Exit;

 // array base
 if (A.ArrayBase <> nil) and (B.ArrayBase <> nil) Then
  Result := Result and type_equal(A.ArrayBase, B.ArrayBase);

 // func return
 if (A.FuncReturn <> nil) and (B.FuncReturn <> nil) Then
  Result := Result and type_equal(A.FuncReturn, B.FuncReturn);

 // func parameter list
 Result := Result and (Length(A.FuncParams) = Length(B.FuncParams));

 // enum base
 if (A.EnumBase <> nil) and (B.EnumBase <> nil) Then
  Result := Result and type_equal(A.EnumBase, B.EnumBase);

 if ((A.EnumBase = nil) and (B.EnumBase <> nil)) or
    ((A.EnumBase <> nil) and (B.EnumBase = nil)) Then
     Exit(False);

 // enum item list
 Result := Result and (A.EnumItemList.Count = B.EnumItemList.Count);
 if (not Result) Then
  Exit;

 For I := 0 To A.EnumItemList.Count-1 Do
  Result := Result and (A.EnumItemList[I] = B.EnumItemList[I]);
End;

(* `TToken_P` in `TRange` *)
Operator in (Token: TToken_P; Range: TRange): Boolean;
Begin
 Result := (Token.Position >= Range.PBegin.Position) and (Token.Position <= Range.PEnd.Position);
End;

(* CreateFunctionMangledName *)
{
 Creates a mangled name of function passed in parameter.
}
Function CreateFunctionMangledName(const Func: TFunction; FuncName: String; SimplifiedName: Boolean): String;
Var P: TParam;
Begin
 With Func do
 Begin
  if (SimplifiedName) Then
   Exit('__function_'+NamespaceName+'_'+FuncName);

  Result := '__function_'+NamespaceName+'_'+FuncName+'_'+ModuleName+'_'+Return.getBytecodeType;
  For P in ParamList Do
   Result += P.Typ.getBytecodeType+'_';
 End;
End;

(* TYPE_ANY *)
Function TYPE_ANY: TType;
Begin
 Result := TType.Create;

 With Result do
 Begin
  RefSymbol.Name := 'any';
  RegPrefix      := 'i';
  InternalID     := TYPE_ANY_id;
 End;
End;

(* TYPE_VOID *)
Function TYPE_VOID: TType;
Begin
 Result := TType.Create;

 With Result do
 Begin
  RefSymbol.Name := 'void';
  RegPrefix      := 'i';
  InternalID     := TYPE_VOID_id;
 End;
End;

(* TYPE_BOOL *)
Function TYPE_BOOL: TType;
Begin
 Result := TType.Create;

 With Result do
 Begin
  RefSymbol.Name := 'bool';
  RegPrefix      := 'b';
  InternalID     := TYPE_BOOL_id;
 End;
End;

(* TYPE_CHAR *)
Function TYPE_CHAR: TType;
Begin
 Result := TType.Create;

 With Result do
 Begin
  RefSymbol.Name := 'char';
  RegPrefix      := 'c';
  InternalID     := TYPE_CHAR_id;
 End;
End;

(* TYPE_INT *)
Function TYPE_INT: TType;
Begin
 Result := TType.Create;

 With Result do
 Begin
  RefSymbol.Name := 'int';
  RegPrefix      := 'i';
  InternalID     := TYPE_INT_id;
 End;
End;

(* TYPE_FLOAT *)
Function TYPE_FLOAT: TType;
Begin
 Result := TType.Create;

 With Result do
 Begin
  RefSymbol.Name := 'float';
  RegPrefix      := 'f';
  InternalID     := TYPE_FLOAT_id;
 End;
End;

(* TYPE_STRING *)
Function TYPE_STRING: TType;
Begin
 Result := TType.Create;

 With Result do
 Begin
  RefSymbol.Name := 'string';
  RegPrefix      := 's';
  InternalID     := TYPE_STRING_id;

  ArrayBase     := TYPE_CHAR;
  ArrayDimCount := 1;
 End;
End;


(* ---------- TType ---------- *)

(* TType.Create *)
{
 Constructor for TType
}
Constructor TType.Create;
Begin
 RefSymbol := TRefSymbol.Create;

 RegPrefix     := 'i';
 ArrayDimCount := 0;
 ArrayBase     := nil;
 InternalID    := 0;
 FuncReturn    := nil;
 EnumBase      := nil;
 EnumItemList  := TVariableList.Create;
 Attributes    := [];

 SetLength(FuncParams, 0);
End;

(* TType.isUnspecialized *)
{
 Returns `true`, when type is marked as `unspecialized`
}
Function TType.isUnspecialized: Boolean;
Begin
 Result := (taUnspecialized in Attributes);
End;

(* TType.isStrict *)
{
 Returns `true`, when type is marked as `strict`
}
Function TType.isStrict: Boolean;
Begin
 Result := (taStrict in Attributes);
End;

(* TType.getBytecodeType *)
{
 Returns type declaration, that can be used as a label's name.
 Eg.instead of "int[]" returns "2darray_int_".
}
Function TType.getBytecodeType: String;
Var I: Integer;
Begin
 if (self = nil) Then
 Begin
  DevLog(dvError, 'TType.getBytecodeType', 'self = nil; that was not supposed to happen. Returned an empty string.');
  Exit('');
 End;

 Result := '';

 { is function? }
 if (taFunction in Attributes) Then
 Begin
  if (taUnspecialized in Attributes) Then
   Result := 'unspecialized_function_' Else
  Begin
   Result := 'function_'+FuncReturn.getBytecodeType+'_';

   For I := Low(FuncParams) To High(FuncParams) Do
    Result += FuncParams[I].Typ.getBytecodeType+'_';
  End;

  if (ArrayDimCount > 0) Then
   Result += IntToStr(ArrayDimCount-Byte(isString))+'darray_'+ArrayBase.getBytecodeType+'_';

  Exit;
 End;

 { is primary? }
 if (ArrayDimCount = 0) or ((ArrayDimCount = 1) and (InternalID = TYPE_STRING_id)) Then
 Begin
  Result += PrimaryTypeNames[InternalID]+'_';
 End Else
 Begin
  { is array? }
  Result += IntToStr(ArrayDimCount-Byte(isString))+'darray_'+ArrayBase.getBytecodeType+'_';
  Exit;
 End;
End;

(* TType.getLowerArray *)
{
 Returns 'lower' type of array type.
 When type is 'int[][]', it returns 'int[]',
 When type is 'string' it returns 'char', and so on.
}
Function TType.getLowerArray: TType;
Begin
 if (not isArray) Then
  raise Exception.Create('TType.getLowerArray() called on non-array type');

 if (ArrayDimCount = 1) Then
 Begin
  Result := ArrayBase;
 End Else
 Begin
  Result               := TType.Create;
  Result.ArrayDimCount := ArrayDimCount-1;
  Result.RefSymbol     := RefSymbol;
  Result.Attributes    := Attributes;

  if (isString) Then
  Begin
   Result.ArrayBase  := ArrayBase.ArrayBase;
   Result.RegPrefix  := ArrayBase.RegPrefix;
   Result.InternalID := ArrayBase.InternalID;
  End Else
  Begin
   Result.ArrayBase  := ArrayBase;
   Result.RegPrefix  := RegPrefix;
   Result.InternalID := InternalID;
  End;
 End;
End;

(* TType.isAny *)
{
 Returns `true` when type passed in parameter is `any`.
}
Function TType.isAny: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(InternalID = TYPE_ANY_id);
End;

(* TType.isVoid *)
{
 Returns `true` when type passed in parameter is `void`.
}
Function TType.isVoid: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(InternalID = TYPE_VOID_id);
End;

(* TType.isBool *)
{
 Returns `true` when type passed in parameter is `bool`.
}
Function TType.isBool: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(InternalID = TYPE_BOOL_id);
End;

(* TType.isChar *)
{
 Returns `true` when type passed in parameter is `char`.
}
Function TType.isChar: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(InternalID = TYPE_CHAR_id);
End;

(* TType.isInt *)
{
 Returns `true` when type passed in parameter is `int` or a pointer.
}
Function TType.isInt: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit((InternalID = TYPE_INT_id) or (taFunction in Attributes));
End;

(* TType.isFloat *)
{
 Returns `true` when type passed in parameter is `float`.
}
Function TType.isFloat: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(InternalID = TYPE_FLOAT_id);
End;

(* TType.isString *)
{
 Returns `true` when type passed in parameter is `string`.
}
Function TType.isString: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(InternalID = TYPE_STRING_id);
End;

(* TType.isNumerical *)
{
 Returns `true` when type passed in parameter is a numerical type (int, float, char and pointers) or numerical-derived; in other case, returns `false`.
}
Function TType.isNumerical: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(isInt or isFloat or isChar or isFunctionPointer);
End;

(* TType.isArray *)
{
 Returns `true` when type passed in parameter is an array; in other case, returns `false`.
}
Function TType.isArray(const RegardStringAsArray: Boolean=True): Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 if ((isString) and (ArrayDimCount = 1)) Then
  Exit(RegardStringAsArray);

 Exit(ArrayDimCount > 0);
End;

(* TType.isObject *)
{
 Returns `true` when type passed in parameter is an object.
}
Function TType.isObject: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Result := isArray(False); // for now, only arrays are some-kind-of-objects
End;

(* TType.isFunctionPointer *)
{
 Returns `true` when type passed in parameter is a function pointer.
}
Function TType.isFunctionPointer: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(taFunction in Attributes);
End;

(* TType.isEnum *)
{
 Returns `true` when type passed in parameter is an enumeration type (enum).
}
Function TType.isEnum: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(taEnum in Attributes);
End;

(* TType.isEnumItem *)
{
 Returns `true` when type passed in parameter is an enumeration type's item.
}
Function TType.isEnumItem: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(EnumBase <> nil);
End;

(* TType.Clone *)
{
 Returns a clone of self.
}
Function TType.Clone: TType;
Var I: Integer;
Begin
 if (self = nil) Then
  Exit(nil);

 Result := TType.Create;

 Result.RefSymbol := RefSymbol.Clone;

 Result.RegPrefix  := RegPrefix;
 Result.InternalID := InternalID;

 Result.ArrayDimCount := ArrayDimCount;

 Result.FuncReturn := FuncReturn.Clone;
 SetLength(Result.FuncParams, Length(FuncParams));
 For I := Low(FuncParams) To High(FuncParams) Do
 Begin
  Result.FuncParams[I]     := FuncParams[I];
  Result.FuncParams[I].Typ := FuncParams[I].Typ.Clone;
 End;

 Result.EnumBase     := EnumBase.Clone;
 Result.EnumItemList := EnumItemList; // @Note: do NOT clone enum items (see `type_equal` routine)

 Result.Attributes := Attributes;

 if (ArrayBase = self) Then
 Begin
  DevLog(dvError, 'TType.Clone', 'ArrayBase = self; cannot do the entire type cloning');
  Result.ArrayBase := ArrayBase;
  Exit;
 End;

 if (ArrayBase = nil) Then
  Result.ArrayBase := nil Else
  Result.ArrayBase := ArrayBase.Clone;
End;

(* TType.asString *)
{
 Returns type declaration.
 Eg.`int[]` or `function<void>(string)`.
}
Function TType.asString: String;
Var I: Integer;
Begin
 if (self = nil) Then // erroneous (invalid) type
  Exit('erroneous type');

 Result := '';

 { is function? }
 if (taFunction in Attributes) Then
 Begin
  if (taUnspecialized in Attributes) Then
   Result += 'unspecialized function' Else
  Begin
   Result += 'function<'+FuncReturn.asString+'>(';

   For I := Low(FuncParams) To High(FuncParams) Do
   Begin
    if (FuncParams[I].isConst) Then
     Result += 'const ';

    if (FuncParams[I].isVar) Then
     Result += 'var ';

    Result += FuncParams[I].Typ.asString;

    if (FuncParams[I].DefaultValue <> nil) Then
     Result += ' = '+getValueFromExpression(FuncParams[I].DefaultValue, True);

    if (I <> High(FuncParams)) Then
     Result += ', ';
   End;

   Result += ')';
  End;

  For I := 1 To ArrayDimCount Do
   Result += '[]';

  Exit;
 End Else

 { is enum? }
 if (taEnum in Attributes) Then
 Begin
  Result := 'enum-item of '+RefSymbol.Name;
 End Else

 { is enum item? }
 if (EnumBase <> nil) Then
 Begin
  Result := 'enum-item of '+EnumBase.RefSymbol.Name;
 End Else

 { is primary? }
 if (ArrayDimCount = 0) Then
 Begin
  if (isString) Then
   Exit('char');

  if (InternalID > High(PrimaryTypeNames)) Then
  Begin
   DevLog(dvError, 'TType.asString', 'InternalID > High(PrimaryTypeNames) ['+IntToStr(InternalID)+' > '+IntToStr(High(PrimaryTypeNames))+']; returned `erroneous type`');
   Exit('erroneous type');
  End;

  Exit(PrimaryTypeNames[InternalID]);
 End Else
 Begin
  { is array? }
  if (isString) or (ArrayBase.isString) Then
  Begin
   I      := ArrayDimCount-1;
   Result += 'string';
  End Else
  Begin
   if (ArrayBase.isArray) Then
    TCompiler(RefSymbol.mCompiler).CompileError(eInternalError, ['ArrayBase.isArray() == true']);

   I      := ArrayDimCount;
   Result += ArrayBase.asString;
  End;

  For I := 1 To I Do
   Result += '[]';
 End;
End;

(* TType.CanBeAssignedTo *)
{
 Returns `true` if this type can be assigned to type passed in parameter
}
Function TType.CanBeAssignedTo(T2: TType): Boolean;
Var I: Integer;
Begin
 Result := True;

 if (self = nil) or (T2 = nil) Then
 Begin
  DevLog(dvWarning, 'TType.CanBeAssignedTo', 'comparing erroneous types; returned `true`');
  Exit(True);
 End;

 { strict types }
 if (self.isStrict) or (T2.isStrict) Then
  Exit(type_equal(self, T2));

 { 'any' types }
 if (self.InternalID = TYPE_ANY_id) or (T2.InternalID = TYPE_ANY_id) Then // any or any => true
  Exit(True);

 { compare function-pointers }
 if (self.isFunctionPointer and T2.isFunctionPointer) Then
 Begin
  if (self.isUnspecialized) or (T2.isUnspecialized) Then
   Exit(True);

  Result := type_equal(self.FuncReturn, T2.FuncReturn) and
            (Length(self.FuncParams) = Length(T2.FuncParams));

  if (not Result) Then
   Exit(False);

  For I := Low(self.FuncParams) To High(T2.FuncParams) Do
   if (not (type_equal(self.FuncParams[I].Typ, T2.FuncParams[I].Typ))) or
      (self.FuncParams[I].isVar <> T2.FuncParams[I].isVar) or
      (self.FuncParams[I].isConst <> T2.FuncParams[I].isConst) Then
    Exit(False);

  Exit(True);
 End Else

 { comparing function-pnt with non-func-pnt }
 if (self.isFunctionPointer and (not T2.isFunctionPointer)) Then
 Begin
  Exit(T2.isInt);
 End Else

 { comparing strings }
 if (self.isString and T2.isString) Then
 Begin
  Exit(self.ArrayDimCount = T2.ArrayDimCount); // string to string => true
 End Else

 { comparing string with non-string (except `char` to `string`) always returns false }
 if (self.isString and (not T2.isString)) or
    ((not self.isString) and (T2.isString)) Then
 Begin
  if (self.isChar and (self.ArrayDimCount = 0)) and (T2.isString) Then
   Exit(True);

  Exit(False);
 End Else

 { comparing arrays }
 if (self.isArray and T2.isArray) Then
 Begin
  Exit(
       (self.ArrayBase.InternalID = T2.ArrayBase.InternalID) and // arrays' base types must be correct
       (self.ArrayDimCount = T2.ArrayDimCount) // and also their dimensions amount must be the same
      );
 End Else

 { comparing array with non-array almost always returns false except one case: int (eg.null) -> array }
 if (self.isArray and (not T2.isArray)) or
    (T2.isArray and (not self.isArray)) Then
 Begin
  Exit(self.isInt and T2.isArray(False));
 End Else

 { comparing enums }
 if (self.isEnum and T2.isEnum) Then
 Begin
  Exit(type_equal(self, T2));
 End Else

 { comparing enum with not-enum always returns unless either one of them is enum-item }
 if (self.isEnum and (not T2.isEnum)) or
    (T2.isEnum and (not self.isEnum)) Then
 Begin
  if (self.isEnumItem) Then
   Exit(type_equal(self.EnumBase, T2)) Else

  if (T2.isEnumItem) Then
   Exit(type_equal(self, T2.EnumBase)) Else

   Exit(False);
 End Else

 { comparing enum items }
 if (self.isEnumItem and T2.isEnumItem) Then
 Begin
  Exit(type_equal(self.EnumBase, T2.EnumBase));
 End Else

 { comparing enum-item with not enum-item returns false unless either one of them is enum }
 if (self.isEnumItem and (not T2.isEnumItem)) or
    (T2.isEnumItem and (not self.isEnumItem)) Then
 Begin
  if (self.isEnum) Then
   Exit(type_equal(self, T2.EnumBase)) Else

  if (T2.isEnum) Then
   Exit(type_equal(self.EnumBase, T2)) Else

   Exit(False);
 End Else

 { compare-table for simple (primary) types }
 Begin
  if (self.isVoid) and (not T2.isVoid) Then // void to non void => false
   Exit(False);

  if (self.isInt) and (T2.isFloat) Then // int to float => true
   Exit(True);

  if (self.isInt) and (T2.isChar) Then // int to char => true
   Exit(True);

  if (self.isChar) and (T2.isInt) Then // char to int => true
   Exit(True);

  if (self.isInt) and (T2.isBool) Then // int to bool => true
   Exit(True);

  if (self.isInt) and (T2.isFunctionPointer or T2.isObject) Then // int to pointer/object => true
   Exit(True);
 End;

 { compare types }
 Exit(type_equal(self, T2));
End;

(* TType.CanBeCastedTo *)
Function TType.CanBeCastedTo(T2: TType): Boolean;
Begin
 if (self = nil) Then
 Begin
  DevLog(dvWarning, 'TType.CanBeCastedTo', 'self = nil; returned `false`');
  Exit(False);
 End;

 if (T2 = nil) Then
 Begin
  DevLog(dvWarning, 'TType.CanBeCastedTo', 'T2 = nil; returned `false`');
  Exit(False);
 End;

 if (type_equal(self, T2)) Then // cast is always valid if types are the same
  Exit(True);

 if (self.isFunctionPointer) Then // `function pointer` can be casted to: int
  Exit(T2.isInt);

 if (self.isBool) Then // `bool` can be casted to: char, int
  Exit(T2.isChar or T2.isInt);

 if (self.isChar) Then // `char` can be casted to: bool, int, string
  Exit(T2.isBool or T2.isInt or T2.isString);

 if (self.isInt) Then // `int` can be casted to: bool, char, float, pointer, object, enum, enum item
  Exit(T2.isBool or T2.isChar or T2.isFloat or T2.isFunctionPointer or T2.isObject or T2.isEnum or T2.isEnumItem);

 if (self.isFloat) Then // `float` can be casted to: bool, int
  Exit(T2.isBool or T2.isInt);

 if (self.isString) Then // `string` cannot be casted to anything
  Exit(False);

 if (self.isVoid) or (T2.isVoid) Then // void to anything => false
  Exit(False);

 if (self.isEnum) or (self.isEnumItem) Then // `enum` can be casted to: int
  Exit(T2.isInt);

 Exit(False);
End;

(* ---------- TVariable ---------- *)

(* TVariable.Create *)
Constructor TVariable.Create;
Begin
 RefSymbol := TRefSymbol.Create;

 MemPos := 0;
 Typ    := nil;
 Value  := nil;

 isFreed := False;

 Attributes := [];
End;

(* TVariable.isConst *)
Function TVariable.isConst: Boolean;
Begin
 Result := vaConst in Attributes;
End;

(* TVariable.isFuncParam *)
Function TVariable.isFuncParam: Boolean;
Begin
 Result := vaFuncParam in Attributes;
End;

(* TVariable.isVolatile *)
Function TVariable.isVolatile: Boolean;
Begin
 Result := vaVolatile in Attributes;
End;

(* TVariable.isCatchVar *)
Function TVariable.isCatchVar: Boolean;
Begin
 Result := vaCatchVar in Attributes;
End;

(* TVariable.DontAllocate *)
Function TVariable.DontAllocate: Boolean;
Begin
 Result := vaDontAllocate in Attributes;
End;

(* TVariable.getAllocationPos *)
{
 Returns variable's position on stack or register; eg.`[-1]` or `ei3`
}
Function TVariable.getAllocationPos: String;
Begin
 if (MemPos <= 0) Then
  Result := '['+IntToStr(MemPos)+']' Else
  Result := 'e'+Typ.RegPrefix+IntToStr(MemPos);
End;

(* ---------- TFunction ---------- *)

(* TFunction.Create *)
Constructor TFunction.Create;
Begin
 RefSymbol := TRefSymbol.Create;
 RefVar    := nil;

 ModuleName    := '';
 NamespaceName := '';

 Return := nil;

 SetLength(ParamList, 0);

 FlowGraph  := TCFGraph.Create;
 SymbolList := TSymbolList.Create;

 StackSize := 0;
 StackRegs := TStackSavedRegs.Create;

 Attributes := [];
End;

(* TFunction.isNaked *)
{
 Returns true if function has attribute `faNaked`
}
Function TFunction.isNaked: Boolean;
Begin
 Result := (faNaked in Attributes);
End;

(* TFunction.findSymbol *)
{
 Returns symbol with specified name or `nil` when such couldn't have been found.
}
Function TFunction.findSymbol(const SymName: String): TSymbol;
Begin
 For Result in SymbolList Do
  if (Result.Name = SymName) Then
   Exit;

 Exit(nil);
End;

(* TFunction.findSymbol *)
{
 Returns symbol with specified name and scope or `nil` when such couldn't have been found.
}
Function TFunction.findSymbol(const SymName: String; const SymScope: TToken_P): TSymbol;
Begin
 For Result in SymbolList Do
  if (Result.Name = SymName) and (SymScope in Result.Range) Then
   Exit;

 Exit(nil);
End;

(* ---------- TNamespace ---------- *)

(* TNamespace.Create *)
Constructor TNamespace.Create;
Begin
 RefSymbol := TRefSymbol.Create;

 SymbolList := TSymbolList.Create;
End;

(* TNamespace.findSymbol *)
{
 Returns symbol with specified name or `nil` when such couldn't have been found.
}
Function TNamespace.findSymbol(const SymName: String): TSymbol;
Begin
 For Result in SymbolList Do
  if (Result.Name = SymName) Then
   Exit;

 Exit(nil);
End;

(* TNamespace.findSymbol *)
{
 Returns symbol with specified name and scope or `nil` when such couldn't have been found.
}
Function TNamespace.findSymbol(const SymName: String; const SymScope: TToken_P): TSymbol;
Begin
 For Result in SymbolList Do
  if (Result.Name = SymName) and (SymScope in Result.Range) Then
   Exit;

 Exit(nil);
End;

(* TNamespace.findFunction *)
{
 Returns function with specified name or `nil` when such couldn't have been found.
}
Function TNamespace.findFunction(const FuncName: String): TFunction;
Var Symbol: TSymbol;
Begin
 Symbol := findSymbol(FuncName);

 if (Symbol <> nil) and (Symbol.Typ = stFunction) Then
  Result := Symbol.mFunction Else
  Result := nil;
End;

(* ---------- TRefSymbol ---------- *)

(* TRefSymbol.Create *)
Constructor TRefSymbol.Create;
Begin
 Name                  := '';
 Range.PBegin.Position := 0;
 Range.PEnd.Position   := High(Integer);

 Visibility := mvPrivate;
 mCompiler  := nil;
 DeclToken  := nil;

 isInternal := False;
End;

(* TRefSymbol.Clone *)
Function TRefSymbol.Clone: TRefSymbol;
Begin
 Result := TRefSymbol.Create;
 CopyTo(Result);
End;

(* TRefSymbol.CopyTo *)
Procedure TRefSymbol.CopyTo(const Symbol: TRefSymbol);
Begin
 if (Symbol = nil) Then
  raise Exception.Create('Invalid method call! `Symbol = nil`');

 if (self = nil) Then
  raise Exception.Create('Invalid method call! `self = nil`');

 Symbol.Name          := Name;
 Symbol.Range         := Range;
 Symbol.Visibility    := Visibility;
 Symbol.mCompiler     := mCompiler;
 Symbol.DeclToken     := DeclToken;
 Symbol.DeclNamespace := DeclNamespace;
 Symbol.DeclFunction  := DeclFunction;
 Symbol.isInternal    := isInternal;
End;

(* TRefSymbol.isLocal *)
{ Returns 'true' if symbol's been declared in local scope (inside a function) }
Function TRefSymbol.isLocal: Boolean;
Begin
 Result := (DeclFunction <> nil);
End;

(* TRefSymbol.isGlobal *)
{ Returns 'true' if symbol's been declared outside a function }
Function TRefSymbol.isGlobal: Boolean;
Begin
 Result := (DeclFunction = nil);
End;

(* ---------- TSymbol ---------- *)

(* TSymbol.Create *)
Constructor TSymbol.Create(const SymbolType: TSymbolType; const CreateInstance: Boolean=True);
Begin
 inherited Create;

 Typ := SymbolType;

 mVariable := nil;
 mFunction := nil;
 mType     := nil;

 if (CreateInstance) Then
  Case Typ of
   stConstant, stVariable:
   Begin
    mVariable           := TVariable.Create;
    mVariable.RefSymbol := self;
   End;

   stFunction:
   Begin
    mFunction           := TFunction.Create;
    mFunction.RefSymbol := self;
   End;

   stType:
   Begin
    mType           := TType.Create;
    mType.RefSymbol := self;
   End;
  End;
End;

(* TSymbol.Create *)
Constructor TSymbol.Create(const SymbolType: TSymbolType; const Instance: Pointer);
Begin
 Create(SymbolType, False);

 Case Typ of
  stConstant, stVariable:
  Begin
   mVariable := TVariable(Instance);
   mVariable.RefSymbol.CopyTo(self);
  End;

  stFunction:
  Begin
   mFunction := TFunction(Instance);
   mFunction.RefSymbol.CopyTo(self);
  End;

  stType:
  Begin
   mType := TType(Instance);
   mType.RefSymbol.CopyTo(self);
  End;
 End;
End;

(* TSymbol.Create *)
Constructor TSymbol.Create(const CloneOf: TSymbol);
Begin
 CloneOf.CopyTo(self);

 Typ       := CloneOf.Typ;
 mVariable := CloneOf.mVariable;
 mFunction := CloneOf.mFunction;
 mType     := CloneOf.mType;

 if (mVariable <> nil) Then
  mVariable.RefSymbol := mVariable.RefSymbol.Clone;

 if (mFunction <> nil) Then
  mFunction.RefSymbol := mFunction.RefSymbol.Clone;

 if (mType <> nil) Then
  mType.RefSymbol := mType.RefSymbol.Clone;
End;
End.
