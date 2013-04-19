(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit symdef;

 Interface
 Uses MTypes, Tokens, FGL;

 Type TVisibility = (mvPublic, mvPrivate);

 Type TTypeAttributes = Set of (taStrict, taFunction, taUnspecialized);
 Type TVariableAttributes = Set of (vaConst, vaFuncParam, vaDontAllocate);
 Type TFunctionAttributes = Set of (faNaked);

 Type TRange = Record
                PBegin, PEnd: Int64;
               End;

 (* lists *)
 Type TNamespace     = class;
      TNamespaceList = specialize TFPGList<TNamespace>;

 Type TLocalSymbol     = class;
      TLocalSymbolList = specialize TFPGList<TLocalSymbol>;

 Type TGlobalSymbol     = class;
      TGlobalSymbolList = specialize TFPGList<TGlobalSymbol>;

 (* Symbol *)
 Type TSymbol = Class
                 Public
                  Name : String; // symbol name
                  Range: TRange; // accessability range

                  Visibility: TVisibility; // visibility
                  mCompiler : Pointer; // compiler in which symbol has been declared
                  DeclToken : PToken_P; // declaration token pointer

                  isInternal: Boolean; // eg.`null` is internal

                 // public methods
                  Constructor Create;
                  Function Clone: TSymbol;
                  Procedure CopyTo(const Symbol: TSymbol);
                 End;

 (* Type *)
 Type TType = class;

 Type TParam = Record
                Name      : String;
                Typ       : TType;
                Attributes: TVariableAttributes;
               End;
      TParamList = Array of TParam;

   // TType
 Type TType = Class
               Public
               // public fields
                RefSymbol: TSymbol;

                RegPrefix : Char;
                InternalID: Byte;

                ArrayBase    : TType; // array base type (it's in most cases a primary type, like `int` or `char`); it CANNOT be any array-derived type!
                ArrayDimCount: Byte; // array dimension count

                FuncReturn: TType;
                FuncParams: TParamList;

                Attributes: TTypeAttributes;

               // public methods
                Constructor Create;

                Function isUnspecialized: Boolean;
                Function isStrict: Boolean;

                Function getBytecodeType: String;

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

                Function CanBeAssignedTo(T2: TType): Boolean;
                Function CanBeCastedTo(T2: TType): Boolean;

                Function Clone: TType;
                Function asString: String;
               End;

 (* Variable *)
 Type TVariable = Class
                   Public
                   // public fields
                    RefSymbol: TSymbol;

                    MemPos: Integer; // negative values and zero for stack position, positive values for register ID (1..4)

                    Typ  : TType;
                    Value: PMExpression;

                    Attributes: TVariableAttributes;

                   // public methods
                    Constructor Create;

                    Function isConst: Boolean;
                    Function isFuncParam: Boolean;
                    Function DontAllocate: Boolean;

                    Function getBytecodePos: String;
                   End;
 Type TMVariableList = Array of TVariable;

 (* Function *)
 Type TFunction = Class
                   Public
                   // public fields
                    RefSymbol: TSymbol;

                    ModuleName   : String; // module name in which function has been declared
                    MangledName  : String; // function mangled (label) name
                    NamespaceName: String; // namespace name in which function has been declared
                    LibraryFile  : String; // only for library-imported functions - library file name

                    Return: TType; // return type

                    ParamList       : TParamList; // parameter list
                    SymbolList      : TLocalSymbolList; // local symbol list
                    ConstructionList: TMConstructionList; // construction list

                    Attributes: TFunctionAttributes;

                   // public methods
                    Constructor Create;

                    Function isNaked: Boolean;
                   End;

 (* Local symbol *)
 Type TLocalSymbolType = (lsConstant, lsVariable, lsType);
 Type TLocalSymbol = Class (TSymbol)
                      Public
                      // methods
                       Constructor Create(const SymbolType: TLocalSymbolType; const CreateInstance: Boolean=True);
                       Constructor Create(const SymbolType: TLocalSymbolType; const Instance: Pointer);

                      // fields
                      Var
                       Typ: TLocalSymbolType;

                       mVariable: TVariable;
                       mType    : TType;
                      End;

 (* Global symbol *)
 Type TGlobalSymbolType = (gsConstant, gsVariable, gsFunction, gsType);
 Type TGlobalSymbol = Class (TSymbol)
                       Public
                       // methods
                        Constructor Create(const SymbolType: TGlobalSymbolType; const CreateInstance: Boolean=True);
                        Constructor Create(const SymbolType: TGlobalSymbolType; const Instance: Pointer);
                        Constructor Create(const CloneOf: TGlobalSymbol);

                       // fields
                       Var
                        Typ: TGlobalSymbolType;

                        mVariable: TVariable; // gdConstant, gdVariable
                        mFunction: TFunction; // gtFunction
                        mType    : TType;
                       End;

 (* Namespace *)
 Type TNamespace = Class (TSymbol)
                    Public
                    // methods
                     Constructor Create;

                    // fields
                    Var
                     SymbolList: TGlobalSymbolList; // global symbol list
                    End;

 // operators
 Function type_equal(A, B: TType): Boolean;

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
Uses CompilerUnit, Compile1, Messages, SysUtils;

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

(* type_equal *)
{
 Compares two TTypes (except their names)
}
Function type_equal(A, B: TType): Boolean;
Begin
 if (A = nil) or (B = nil) Then
  Exit(False);

 Result :=
 (A.ArrayDimCount = B.ArrayDimCount) and
 (A.InternalID    = B.InternalID) and
 (A.Attributes    = B.Attributes) and
 (A.RegPrefix     = B.RegPrefix);

 if (not Result) Then
  Exit(False);

 if (A.ArrayBase <> nil) and (B.ArrayBase <> nil) Then
  Result := Result and type_equal(A.ArrayBase, B.ArrayBase);

 if (A.FuncReturn <> nil) and (B.FuncReturn <> nil) Then
  Result := Result and type_equal(A.FuncReturn, B.FuncReturn);

 Result := Result and (Length(A.FuncParams) = Length(B.FuncParams));
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

(* ---------- TSymbol ---------- *)

(* TSymbol.Create *)
Constructor TSymbol.Create;
Begin
 Name         := '';
 Range.PBegin := 0;
 Range.PEnd   := 0;

 Visibility := mvPrivate;
 mCompiler  := nil;
 DeclToken  := nil;
 isInternal := False;
End;

(* TSymbol.Clone *)
Function TSymbol.Clone: TSymbol;
Begin
 Result := TSymbol.Create;

 Result.Name       := Name;
 Result.Range      := Range;
 Result.Visibility := Visibility;
 Result.mCompiler  := mCompiler;
 Result.DeclToken  := DeclToken;
 Result.isInternal := isInternal;
End;

(* TSymbol.CopyTo *)
Procedure TSymbol.CopyTo(const Symbol: TSymbol);
Begin
 if (Symbol = nil) Then
  raise Exception.Create('Invalid method call! `Symbol = nil`');

 Symbol.Name       := Name;
 Symbol.Range      := Range;
 Symbol.Visibility := Visibility;
 Symbol.mCompiler  := mCompiler;
 Symbol.DeclToken  := DeclToken;
 Symbol.isInternal := isInternal;
End;

(* ---------- TType ---------- *)

(* TType.Create *)
{
 Constructor for TType
}
Constructor TType.Create;
Begin
 RefSymbol := TSymbol.Create;

 RegPrefix     := #0;
 ArrayDimCount := 0;
 ArrayBase     := nil;
 InternalID    := 0;
 FuncReturn    := nil;
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
  DevLog('Error: TType.getBytecodeType() -> self = nil; that was not supposed to happen. Returned an empty string.');
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

(* TType.isVoid *)
{
 Returns `true` when type passed in parameter is `void`.
}
Function TType.isVoid: Boolean;
Begin
 if (self = nil) Then
  Exit(False);

 Exit(InternalID in [TYPE_VOID_id]);
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

(* TType.Clone *)
{
 Returns a clone of self.
}
Function TType.Clone: TType;
Var I: Integer;
Begin
 Result := TType.Create;

 if (self = nil) Then
 Begin
  //DevLog('Info: TType.Clone() called with `self = nil`; returned an empty type');
  Exit;
 End;

 Result.RefSymbol := RefSymbol.Clone;

 Result.RegPrefix  := RegPrefix;
 Result.InternalID := InternalID;

 Result.ArrayDimCount := ArrayDimCount;

 Result.FuncReturn := FuncReturn.Clone;
 SetLength(Result.FuncParams, Length(FuncParams));
 For I := Low(FuncParams) To High(FuncParams) Do
  Result.FuncParams[I].Typ := FuncParams[I].Typ.Clone;

 Result.Attributes := Attributes;

 if (ArrayBase = self) Then
 Begin
  DevLog('Error: TType.Clone() -> ArrayBase = self; cannot do the entire type cloning');
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
 if (self = nil) Then
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
    Result += FuncParams[I].Typ.asString;
    if (I <> High(FuncParams)) Then
     Result += ', ';
   End;

   Result += ')';
  End;

  For I := 1 To ArrayDimCount Do
   Result += '[]';

  Exit;
 End;

 { is primary? }
 if (ArrayDimCount = 0) Then
 Begin
  if (isString) Then
   Exit('char');

  if (InternalID > High(PrimaryTypeNames)) Then
  Begin
   DevLog('Error: InternalID > High(PrimaryTypeNames) ('+IntToStr(InternalID)+' > '+IntToStr(High(PrimaryTypeNames))+'); returned `erroneous type`');
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
  {$IFDEF COMPARE_ERRONEOUS_TYPE}
   DevLog('Comparing erroneous types; returned `true`');
   Exit(True);
  {$ELSE}
   DevLog('Comparing erroneous types; returned `false`');
   Exit(False);
  {$ENDIF}
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
   if (not type_equal(self.FuncParams[I].Typ, T2.FuncParams[I].Typ)) Then
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

 { comparing array with non-array always returns false }
 if (self.isArray and (not T2.isArray)) or
    (T2.isArray and (not self.isArray)) Then
 Begin
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

  if (self.isInt) and (T2.isBool) Then // int to bool => true (it's supported by the VM)
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
  DevLog('Warning: TType.CanBeCastedTo() -> self = nil; returned `false`');
  Exit(False);
 End;

 if (T2 = nil) Then
 Begin
  DevLog('Warning: TType.CanBeCastedTo() -> T2 = nil; returned `false`');
  Exit(False);
 End;

 if (type_equal(self, T2)) Then
  Exit(True);

 if (self.isFunctionPointer) Then // `function pointer` can be casted to: int
  Exit(T2.isInt);

 if (self.isBool) Then // `bool` can be casted to: char, int
  Exit(T2.isChar or T2.isInt);

 if (self.isChar) Then // `char` can be casted to: bool, int, string
  Exit(T2.isBool or T2.isInt or T2.isString);

 if (self.isInt) Then // `int` can be casted to: bool, char, float, pointer, object
  Exit(T2.isBool or T2.isChar or T2.isFloat or T2.isFunctionPointer or T2.isObject);

 if (self.isFloat) Then // `float` can be casted to: bool, int
  Exit(T2.isBool or T2.isInt);

 if (self.isString) Then // `string` cannot be casted to anything
  Exit(False);

 if (self.isVoid) or (T2.isVoid) Then // void to anything => false
  Exit(False);

 Exit(False);
End;

(* ---------- TVariable ---------- *)

(* TVariable.Create *)
Constructor TVariable.Create;
Begin
 RefSymbol := TSymbol.Create;

 MemPos := 0;
 Typ    := nil;
 Value  := nil;

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

(* TVariable.DontAllocate *)
Function TVariable.DontAllocate: Boolean;
Begin
 Result := vaDontAllocate in Attributes;
End;

(* TVariable.getBytecodePos *)
{
 Returns variable's position on stack or register; eg.`[-1]` or `ei3`
}
Function TVariable.getBytecodePos: String;
Begin
 if (MemPos <= 0) Then
  Result := '['+IntToStr(MemPos)+']' Else
  Result := 'e'+Typ.RegPrefix+IntToStr(MemPos);
End;

(* ---------- TFunction ---------- *)

(* TFunction.Create *)
Constructor TFunction.Create;
Begin
 RefSymbol := TSymbol.Create;

 ModuleName    := '';
 NamespaceName := '';

 Return := nil;

 SetLength(ParamList, 0);
 SetLength(ConstructionList, 0);

 SymbolList := TLocalSymbolList.Create;

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

(* ---------- TLocalSymbol ---------- *)

(* TLocalSymbol.Create *)
Constructor TLocalSymbol.Create(const SymbolType: TLocalSymbolType; const CreateInstance: Boolean=True);
Begin
 Typ := SymbolType;

 mVariable := nil;
 mType     := nil;

 if (CreateInstance) Then
  Case Typ of
   lsConstant, lsVariable:
   Begin
    mVariable           := TVariable.Create;
    mVariable.RefSymbol := self;
   End;

   lsType:
   Begin
    mType           := TType.Create;
    mType.RefSymbol := self;
   End;
  End;
End;

(* TLocalSymbol.Create *)
Constructor TLocalSymbol.Create(const SymbolType: TLocalSymbolType; const Instance: Pointer);
Begin
 Create(SymbolType, False);

 Case Typ of
  lsConstant, lsVariable:
  Begin
   mVariable := TVariable(Instance);
   mVariable.RefSymbol.CopyTo(self);
  End;

  lsType:
  Begin
   mType := TType(Instance);
   mType.RefSymbol.CopyTo(self);
  End;
 End;
End;

(* ---------- TGlobalSymbol ---------- *)

(* TGlobalSymbol.Create *)
Constructor TGlobalSymbol.Create(const SymbolType: TGlobalSymbolType; const CreateInstance: Boolean=True);
Begin
 Typ := SymbolType;

 mVariable := nil;
 mFunction := nil;
 mType     := nil;

 if (CreateInstance) Then
  Case Typ of
   gsConstant, gsVariable:
   Begin
    mVariable           := TVariable.Create;
    mVariable.RefSymbol := self;
   End;

   gsFunction:
   Begin
    mFunction           := TFunction.Create;
    mFunction.RefSymbol := self;
   End;

   gsType:
   Begin
    mType           := TType.Create;
    mType.RefSymbol := self;
   End;
  End;
End;

(* TGlobalSymbol.Create *)
Constructor TGlobalSymbol.Create(const SymbolType: TGlobalSymbolType; const Instance: Pointer);
Begin
 Create(SymbolType, False);

 Case Typ of
  gsConstant, gsVariable:
  Begin
   mVariable := TVariable(Instance);
   mVariable.RefSymbol.CopyTo(self);
  End;

  gsFunction:
  Begin
   mFunction := TFunction(Instance);
   mFunction.RefSymbol.CopyTo(self);
  End;

  gsType:
  Begin
   mType := TType(Instance);
   mType.RefSymbol.CopyTo(self);
  End;
 End;
End;

(* TGlobalSymbol.Create *)
Constructor TGlobalSymbol.Create(const CloneOf: TGlobalSymbol);
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

(* ---------- TNamespace ---------- *)

(* TNamespace.Create *)
Constructor TNamespace.Create;
Begin
 SymbolList := TGlobalSymbolList.Create;
End;
End.
