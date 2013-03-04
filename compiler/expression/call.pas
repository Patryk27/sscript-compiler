Procedure ParseCall;
Var IdentID, Namespace: Integer;

{$I array_length.pas}

{ CastCall }
Procedure CastCall;
Var TypeID       : PMType;
    Param        : Integer;
    fParamList   : TMParamList;
    Unspecialized: Boolean;
Begin
 TypeID := Parse(Left, 1, 'r');
 RePop(Left, TypeID, 1);

 With Compiler do
  if (not isTypeFunctionPointer(TypeID)) Then
  Begin
   Error(eWrongType, [getTypeDeclaration(TypeID), 'function pointer']);
   Exit;
  End;

 fParamList    := TypeID^.FuncParams;
 Result        := TypeID^.FuncReturn;
 Unspecialized := TypeID^.isUnspecialized;

 if (not Unspecialized) Then
 Begin
  // check param count
  if (Length(Expr^.ParamList) <> Length(fParamList)) Then
  Begin
   Error(eWrongParamCount, ['anonymous cast-function', Length(fParamList), Length(Expr^.ParamList)]);
   Exit;
  End;
 End Else
  SetLength(fParamList, Length(Expr^.ParamList));

 // push parameters onto the stack
 For Param := Low(fParamList) To High(fParamList) Do
 Begin
  TypeID := Parse(Expr^.ParamList[Param]);

  if (not Unspecialized) Then
   With Compiler do
    if (not CompareTypes(fParamList[High(fParamList)-Param].Typ, TypeID)) Then
     Error(Expr^.ParamList[Param]^.Token, eWrongTypeInCall, ['anonymous cast-function', High(fParamList)-Param+1, getTypeDeclaration(TypeID), getTypeDeclaration(fParamList[High(fParamList)-Param].Typ)]);
 End;

 Dec(PushedValues, Length(fParamList));

 // call function-pointer
 Compiler.PutOpcode(o_acall, ['er1']);
End;

{ LocalVarCall }
Procedure LocalVarCall;
Var TypeID       : PMType;
    Param        : Integer;
    Variable     : TRVariable;
    fParamList   : TMParamList;
    Unspecialized: Boolean;
Begin
 Variable := getVariable(Expr^.Left);

 if (Variable.ID = -1) Then
  Exit;

 TypeID := __variable_getvalue_reg(Variable, 1, 'r');

 With Compiler do
  if (not isTypeFunctionPointer(TypeID)) Then
  Begin
   Error(eWrongType, [getTypeDeclaration(TypeID), 'function pointer']);
   Exit;
  End;

 fParamList    := TypeID^.FuncParams;
 Result        := TypeID^.FuncReturn;
 Unspecialized := TypeID^.isUnspecialized;

 if (not Unspecialized) Then
 Begin
  // check param count
  if (Length(Expr^.ParamList) <> Length(fParamList)) Then
  Begin
   Error(eWrongParamCount, [Variable.Name, Length(fParamList), Length(Expr^.ParamList)]);
   Exit;
  End;
 End Else
  SetLength(fParamList, Length(Expr^.ParamList));

 // push parameters onto the stack
 For Param := Low(fParamList) To High(fParamList) Do
 Begin
  TypeID := Parse(Expr^.ParamList[Param]);

  if (not Unspecialized) Then
   With Compiler do
    if (not CompareTypes(fParamList[High(fParamList)-Param].Typ, TypeID)) Then
     Error(Expr^.ParamList[Param]^.Token, eWrongTypeInCall, [Variable.Name, High(fParamList)-Param+1, getTypeDeclaration(TypeID), getTypeDeclaration(fParamList[High(fParamList)-Param].Typ)]);
 End;

 Dec(PushedValues, Length(fParamList));

 // call function-pointer
 Compiler.PutOpcode(o_acall, ['er1']);
End;

{ GlobalFuncCall }
Procedure GlobalFuncCall;
Var TypeID: PMType;
    Param : Integer;
Begin
 With Compiler.NamespaceList[Namespace].GlobalList[IdentID].mFunction do
 Begin
  // check param count
  if (Length(Expr^.ParamList) <> Length(ParamList)) Then
  Begin
   Error(eWrongParamCount, [Name, Length(ParamList), Length(Expr^.ParamList)]);
   Exit;
  End;

  // push parameters onto the stack
  For Param := Low(ParamList) To High(ParamList) Do
  Begin
   TypeID := Parse(Expr^.ParamList[Param]);

   With Compiler do
    if (not CompareTypes(ParamList[High(ParamList)-Param].Typ, TypeID)) Then
     Error(Expr^.ParamList[Param]^.Token, eWrongTypeInCall, [Name, High(ParamList)-Param+1, getTypeDeclaration(TypeID), getTypeDeclaration(ParamList[High(ParamList)-Param].Typ)]);
  End;

  Dec(PushedValues, Length(ParamList));

  // call function
  Compiler.PutOpcode(o_call, [':'+MName]);

  Result := Return;
 End;
End;

Begin
 IdentID    := Expr^.IdentID;
 Namespace  := Expr^.IdentNamespace;

 if (Expr^.IdentID = -1) Then // function not found or cast-call
 Begin
  if (VarToStr(Expr^.Value) = 'cast-call') Then // cast-call
  Begin
   CastCall;
   Exit;
  End;

  // is it any of internal functions?
  Case VarToStr(Left^.Value) of
   { @Note: when changing internal functions, modify also @TInterpreter.MakeTree.CreateNodeFromStack }

   'array_length': __array_length;
  End;

  Exit; // error message has been shown when building the expression tree
 End;

 { local variable call }
 if (Expr^.isLocal) Then
  LocalVarCall Else

 { global function call }
 if (Compiler.NamespaceList[Namespace].GlobalList[IdentID].Typ = gdFunction) Then
  GlobalFuncCall Else

 { global variable call }
  Error(eInternalError, ['@TODO']);
End;
