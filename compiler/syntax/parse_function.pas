(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_FUNCTION;

 Interface
 Uses MTypes, Tokens, Variants, TypInfo;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, Messages, Opcodes, ExpressionCompiler, symdef, SysUtils, CompilerUnit;

(* Parse *)
{
 Parses and compiles functions.
}
Procedure Parse(Compiler: Pointer);
Type TVarRecArray = Array of TVarRec;
     PVarRecArray = ^TVarRecArray;
Var Func : TFunction; // our new function

    CList: TMConstructionList;
    c_ID : Integer;

    NamedParams  : (npUnknown, npYes, npNo) = npUnknown;
    FuncNameLabel: String;

  {$I gen_bytecode.pas}

  // NewConst (used to create internal constants, if enabled)
  Procedure NewConst(const cName: String; cTyp: TType; cValue: PMExpression);
  Var Variable: TVariable;
  Begin
   With TCompiler(Compiler) do
   Begin
    Variable       := TVariable.Create;
    Variable.Typ   := cTyp;
    Variable.Value := cValue;

    With Variable.RefSymbol do
    Begin
     Name      := cName;
     mCompiler := Compiler;
     DeclToken := Parser.next_pnt;
    End;

    Variable.Attributes += [vaConst, vaDontAllocate];

    if (findLocalVariable(cName) = -1) Then // don't duplicate
     With getCurrentFunction do
     Begin
      SymbolList.Add(TLocalSymbol.Create(lsConstant, False));
      SymbolList.Last.mVariable := Variable;
     End;
   End;
  End;

  // ReadParamList
  Procedure ReadParamList;
  Var I: Integer;
  Label NextParam;
  Begin
   SetLength(Func.ParamList, 0);

   With TCompiler(Compiler), Parser, Func do
   Begin
    eat(_BRACKET1_OP); // (

    if (next_t <> _BRACKET1_CL) Then // special case: empty parameter list (`()`)
     While (true) Do
     Begin
      SetLength(ParamList, Length(ParamList)+1); // resize the param array
      With ParamList[High(ParamList)] do // read parameter
      Begin
       if (next_t = _CONST) Then // is a const-param?
       Begin
        read;
        Attributes += [vaConst];
       End;

       if (next_t = _VAR) Then // is a var-param?
        CompileError(eUnimplemented, ['var-params']);

       Typ := read_type; // [param type]

       if (Typ.isVoid) Then // error: void-typed param
        CompileError(eVoidParam, [Name]);

       if (next_t in [_COMMA, _BRACKET1_CL]) Then
       Begin
        if (NamedParams = npYes) Then
         CompileError(next, eExpectedIdentifier, [next.Value]) Else
         Begin
          NamedParams := npNo;
          goto NextParam;
         End;
       End;

       if (NamedParams = npNo) Then
        CompileError(next, eExpected, [',', next.Value]);

       Name        := read_ident; // [param name]
       NamedParams := npYes;

       { check for duplicates }
       For I := Low(ParamList) To High(ParamList)-1 Do
        if (ParamList[I].Name = Name) Then
        Begin
         CompileError(eRedeclaration, [Name]); // error: parameter has been redeclared
         Break;
        End;
      End;

     NextParam:
      if (next_t = _BRACKET1_CL) Then
       Break Else
       eat(_COMMA); // parameters are separated by comma
     End;

    eat(_BRACKET1_CL); // read remaining parenthesis
   End;
  End;

  // ReadAttributes
  Procedure ReadAttributes;
  Var Token        : TToken_P;
      Option, Value: String;
  Begin
   With TCompiler(Compiler), Parser do
   Begin
    Func.Attributes := [];

    While (true) Do
    Begin
     Token := read;
     Case Token.Token of
      _NAKED: Include(Func.Attributes, faNaked);

      _BRACKET2_OP:
      Begin
       Option := read_ident;
       eat(_EQUAL);
       Value := read_string;

       Case Option of
        'library': Func.LibraryFile := Value;
        'namespace': Func.NamespaceName := Value;
        'name': FuncNameLabel := Value;
        'label': Func.MangledName := Value;

        else
         CompileError(eUnknownAttribute, [Option]);
       End;

       eat(_BRACKET2_CL);
      End;

      else
       Break;
     End;
    End;
    Dec(TokenPos);
   End;
  End;

// main function block
Var I, SavedRegs: Integer;
    VarsOnStack : LongWord = 0;

    FuncID, NamespaceID: Integer;
    Symbol             : TLocalSymbol;

    Namespaces: Array of Integer;
Begin
With TCompiler(Compiler), Parser do
Begin
 // make a backup of current namespaces (we'll restore them when finish compiling this namespace)
 SetLength(Namespaces, Length(SelectedNamespaces));
 For I := Low(Namespaces) To High(Namespaces) Do
  Namespaces[I] := SelectedNamespaces[I];

 (* if first pass *)
 if (CompilePass = cp1) Then
 Begin
  Func                     := TFunction.Create;
  CurrentFunction          := Func;
  Func.RefSymbol.mCompiler := Compiler;
  Func.RefSymbol.DeclToken := next_pnt(-1); // _FUNCTION
  Func.NamespaceName       := getCurrentNamespace.Name;

  { read function return type }
  eat(_LOWER); // <
  Func.Return := read_type; // [type]
  eat(_GREATER); // >

  { read function name }
  Func.RefSymbol.Name       := read_ident; // [identifier]
  Func.RefSymbol.Visibility := getVisibility;
  Func.ModuleName           := ModuleName;

  FuncNameLabel := Func.RefSymbol.Name;

  RedeclarationCheck(Func.RefSymbol.Name); // check for redeclaration

  { read parameter list }
  ReadParamList;

  { read special attributes }
  ReadAttributes;

  { generate label name }
  if (Func.MangledName = '') Then
  Begin
   if (Func.LibraryFile <> '') or ((TCompiler(Compiler).Parent.CompileMode = cmLibrary) and (Pointer(TCompiler(Compiler).Parent) = Compiler)) Then
    Func.MangledName := CreateFunctionMangledName(Func, FuncNameLabel, True) Else
    Func.MangledName := CreateFunctionMangledName(Func, FuncNameLabel, False); // create function mangled name
  End;

  // check for redeclaration by label name
  findFunctionByLabel(Func.MangledName, FuncID, NamespaceID);

  if (FuncID <> -1) Then
  Begin
   CompileError(eRedeclaration, [Func.MangledName]);

   With NamespaceList[NamespaceID].SymbolList[FuncID] do
    TCompiler(mCompiler).CompileError(DeclToken, ePrevDeclared, []);
  End;

  { add this function into the symbol list }
  With getCurrentNamespace do
  Begin
   SymbolList.Add(TGlobalSymbol.Create(gsFunction, Func));

   With SymbolList.Last do
   Begin
    mVariable                := TVariable.Create;
    mVariable.RefSymbol.Name := Func.RefSymbol.Name;
    mVariable.Typ            := NewTypeFromFunction(Func);
    mVariable.Value          := MakeIntExpression('@'+Func.MangledName);

    mVariable.Attributes += [vaConst, vaDontAllocate]; // const, don't allocate
   End;
  End;

  if (Func.LibraryFile <> '') Then // if function is external, don't create any bytecode
  Begin
   semicolon;
   Exit;
  End;

  if (NamedParams = npNo) and (next_t <> _SEMICOLON) Then
   CompileError(next, eExpected, [';', next.Value]);

  { add parameters }
  With Func do
   For I := Low(ParamList) To High(ParamList) Do
    __variable_create(ParamList[I].Name, ParamList[I].Typ, -I-2, [vaFuncParam, vaDontAllocate]+ParamList[I].Attributes);

  { add special constants (if `--internal-const` enabled) }
  if (getBoolOption(opt_internal_const)) Then
  Begin
   NewConst('__self', TYPE_STRING, MakeStringExpression(Func.RefSymbol.Name));
  End;

  SkipCodeBlock;
  Exit;
 End Else

 (* if second pass *)
 if (CompilePass = cp2) Then
 Begin
  skip_parenthesis; // return type
  Func := getCurrentNamespace.SymbolList[findFunction(read_ident)].mFunction;
  skip_parenthesis; // param list
  While not (next_t in [_BRACKET3_OP, _SEMICOLON]) do
   read;

  CurrentFunction := Func;

  if (Func.LibraryFile <> '') Then { if is an imported function }
  Begin
   semicolon;
   Exit;
  End;

  { new label (function begin) }
  PutLabel(Func.MangledName)^.isPublic := True;

  { new scope (because we're in function now) }
  NewScope(sFunction);

  { ====== parse function's body ====== }
  ParseCodeBlock;

  PutOpcode(o_loc_func, ['"'+Func.RefSymbol.Name+'"']);

  (* now, we have a full construction list used in this function; so - let's generate bytecode! :) *)
  CList := getCurrentFunction.ConstructionList;

  { allocate local stack variables }
  VarsOnStack := 0;

  if (not Func.isNaked) Then
  Begin
   With getCurrentFunction do
    For Symbol in SymbolList Do // each symbol
     if (Symbol.Typ = lsVariable) Then // if variable
      With Symbol.mVariable do
       if (MemPos <= 0) and (not DontAllocate) Then
        Inc(VarsOnStack); // next variable to allocate

   PutOpcode(o_add, ['stp', VarsOnStack+1]); // `+1`, because `stack[stp]` is the caller's IP (instruction pointer)
  End;

  { if register is occupied by variable, we need to at first save this register's value (and restore it at the end of the function) }
  SavedRegs := 0;
  With getCurrentFunction do
  Begin
   For Symbol in SymbolList Do // each symbol
    if (Symbol.Typ = lsVariable) Then // if variable
     With Symbol.mVariable do
      if (MemPos > 0) Then // if allocated in register
      Begin
       PutOpcode(o_push, ['e'+Typ.RegPrefix+IntToStr(MemPos)]);
       Inc(SavedRegs);
      End;

   For Symbol in SymbolList Do // each symbol
    if (Symbol.Typ = lsVariable) Then // if variable
     With Symbol.mVariable do
      if (MemPos <= 0) Then // if allocated on the stack
      Begin
       MemPos -= SavedRegs;

       if (isFuncParam) Then
        MemPos -= VarsOnStack;
      End;
  End;

  { new label (main function's body; nothing should jump here, it's just facilitation for optimizer) }
  PutLabel(Func.MangledName+'_body');

  { compile constructions }
  c_ID := 0;
  Repeat
   ParseConstruction(c_ID);
   Inc(c_ID);
  Until (c_ID > High(CList));

  { function end code }
  PutLabel(Func.MangledName+'_end');

  With getCurrentFunction do
  Begin
   For I := SymbolList.Count-1 Downto 0 Do
   Begin
    Symbol := SymbolList[I];
    if (Symbol.Typ = lsVariable) Then // if variable
     With Symbol.mVariable do
      if (MemPos > 0) Then // if allocated in register
       PutOpcode(o_pop, ['e'+Typ.RegPrefix+IntToStr(MemPos)]);
   End;
  End;

  if (not Func.isNaked) Then
   PutOpcode(o_sub, ['stp', VarsOnStack+1]);

  PutOpcode(o_ret);
  // </>

  RemoveScope; // ... and - as we finished compiling this function - remove scope
 End;

 SetLength(SelectedNamespaces, Length(Namespaces));
 For I := Low(Namespaces) To High(Namespaces) Do
  SelectedNamespaces[I] := Namespaces[I];
End;
End;
End.
