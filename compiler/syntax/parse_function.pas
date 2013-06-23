(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_FUNCTION;

 Interface
 Uses Expression, Tokens, Variants, TypInfo;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses SysUtils, Classes, FGL, Compile1, Messages, Opcodes, ExpressionCompiler, symdef, cfgraph, CompilerUnit;

Type PVar = ^TVar;
     TVar = Record
             mVar    : TVariable;
             UseCount: uint32;
            End;

// SortVarRec
Function SortVarRec(const A, B: PVar): Integer;
Begin
 if (A^.UseCount > B^.UseCount) Then
  Result := -1 Else
 if (A^.UseCount < B^.UseCount) Then
  Result := 1 Else
  Result := 0;
End;

(* Parse *)
{
 Parses and compiles functions.
}
Procedure Parse(Compiler: Pointer);
Var Func : TFunction; // our new function

    NamedParams        : (npNotSetYet, npYes, npNo) = npNotSetYet;
    RequireDefaultValue: Boolean = False;
    DefaultValueType   : TType;
    FuncNameLabel      : String;

    VarsOnStack: uint16 = 0; // number of variables allocated on the stack

    RemovedNodes: TCFGNodeList; // list of removed nodes

  {$I gen_bytecode.pas}

  // NewConst (used to create internal constants, if enabled)
  Procedure NewConst(const cName: String; cTyp: TType; cValue: PExpression);
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
  Label SkipParamName;
  Begin
   SetLength(Func.ParamList, 0); // every function has no parameters by the default

   With TCompiler(Compiler), Parser, Func do
   Begin
    eat(_BRACKET1_OP); // (

    if (next_t <> _BRACKET1_CL) Then // special case: empty parameter list (`()`)
     While (true) Do
     Begin
      SetLength(ParamList, Length(ParamList)+1); // resize the param array

      With ParamList[High(ParamList)] do // read parameter
      Begin
       if (next_t = _CONST) { const } Then // is a const-param?
       Begin
        read;
        Attributes += [vaConst];
        isConst := True;
       End Else

       if (next_t = _VAR) { var } Then // is a var-param?
       Begin
        read;
        isVar := True;
       End;

       Typ := read_type; // [param type]

       if (Typ.isVoid) Then // error: void-typed param
        CompileError(eVoidParam, [Name]);

       if (next_t in [_EQUAL, _COMMA, _BRACKET1_CL]) Then // if `=`, `,` or `)` is next...
       Begin
        if (NamedParams = npYes) Then
         CompileError(next, eExpectedIdentifier, [next.Value]) Else
         Begin
          NamedParams := npNo;
          goto SkipParamName;
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

      SkipParamName:
       if (next_t = _EQUAL) Then // default parameter value specified
       Begin
        eat(_EQUAL);
        DefaultValue     := read_constant_expr;
        DefaultValueType := getTypeFromExpr(DefaultValue^);
        Dec(TokenPos);

        if (not DefaultValueType.CanBeAssignedTo(Typ)) Then
         CompileError(eWrongType, [DefaultValueType.asString, Typ.asString]);

        RequireDefaultValue := True;
       End Else
        if (RequireDefaultValue) Then
         CompileError(eDefaultValueRequired, [Name]) Else
         DefaultValue := nil;
      End;

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
        'library'  : Func.LibraryFile := Value;
        'namespace': Func.NamespaceName := Value;
        'name'     : FuncNameLabel := Value;
        'label'    : Func.MangledName := Value;

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
Var I: Integer;

    FuncID, NamespaceID: Integer;

    Namespaces: Array of Integer;

    TmpNode: TCFGNode;
Begin
With TCompiler(Compiler), Parser do
Begin
 // make a backup of current namespaces (we'll restore them when finish compiling this namespace)
 SetLength(Namespaces, Length(SelectedNamespaces));
 For I := Low(Namespaces) To High(Namespaces) Do
  Namespaces[I] := SelectedNamespaces[I];

 (* if first pass *)
 if (CompilePass = _cp1) Then
 Begin
  skip_parenthesis; // return type
  skip_parenthesis; // param list
  While not (next_t in [_BRACKET3_OP, _SEMICOLON]) do
   read;

  SkipCodeBlock;
  Exit;
 End Else

 (* if second pass *)
 if (CompilePass = _cp2) Then
 Begin
  Func                     := TFunction.Create;
  CurrentFunction          := Func;
  Func.RefSymbol.mCompiler := Compiler;
  Func.RefSymbol.DeclToken := next_pnt(-1); // _FUNCTION
  Func.NamespaceName       := getCurrentNamespace.Name;

  { read function return type }
  eat(_LOWER);
  Func.Return := read_type;
  eat(_GREATER);

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

 (* if third pass *)
 if (CompilePass = _cp3) Then
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
  setNewRootNode(nil, False);

  ParseCodeBlock; // parse!

  Func.FlowGraph.Root := getCurrentRoot;
  Func.FlowGraph.Last := getCurrentNode;

  (* now, we have the full control flow graph of this function; so - let's do many magic things and eventually generate bytecode! :) *)

  VisitedNodes := TCFGNodeList.Create;
  RemovedNodes := TCFGNodeList.Create;
  Try
   DevLog(dvInfo, 'Parse', 'Validing graph for function `'+Func.RefSymbol.Name+'`...');
   ValidateGraph;

  //if (Func.RefSymbol.Name = 'main') Then // @TODO
   //DrawGraph(Func.FlowGraph); // `not_optimized/main.dot`

   if (getBoolOption(opt__constant_folding)) Then
   Begin
    DevLog(dvInfo, 'Parse', 'Optimizing expressions...');
    OptimizeExpressions;
   End;

   if (getBoolOption(opt__optimize_branches)) Then
   Begin
    DevLog(dvInfo, 'Parse', 'Optimizing branches...');
    OptimizeBranches;
   End;

   if (getBoolOption(opt__remove_dead)) Then
   Begin
    DevLog(dvInfo, 'Parse', 'Removing unused variables...');
    RemoveUnusedVariables;
   End;

   DevLog(dvInfo, 'Parse', 'Allocating variables...');
   AllocateVariables(getBoolOption(opt__register_alloc));

   DevLog(dvInfo, 'Parse', 'Generating bytecode...');
   AddPrologCode;
   GenerateBytecode(Func.FlowGraph.Root);
   AddEpilogCode;
  Finally
   DoNotGenerateCode := True;
   For TmpNode in RemovedNodes Do // compile (but not save generated bytecode) removed nodes
    Generate(TmpNode);
   DoNotGenerateCode := False;

   (* @Note:
    We're compiling removed nodes, to avoid situations like this:

    if (true)
     main(); else
     this_would_not_be_compiled_and_thus_no_error_would_have_been_raised*2;
   *)

   RemovedNodes.Free;
   VisitedNodes.Free;
  End;

  DevLog(dvInfo, 'Parse', 'Function '''+Func.RefSymbol.Name+''' has been compiled!');
  DevLog;

  //if (Func.RefSymbol.Name = 'main') Then // @TODO
  // DrawGraph(Func.FlowGraph); // `optimized/main.dot`

  RemoveScope; // ... and - as we finished compiling this function - remove scope
 End;

 SetLength(SelectedNamespaces, Length(Namespaces));
 For I := Low(Namespaces) To High(Namespaces) Do
  SelectedNamespaces[I] := Namespaces[I];
End;
End;
End.
