(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_FUNCTION;

 Interface
 Uses Expression, Tokens, Variants, TypInfo;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses SysUtils, Classes, FGL, SSCompiler, Messages, Opcodes, ExpressionCompiler, symdef, FlowGraph, CompilerUnit;

Type PVar = ^TVar;
     TVar = Record
             mVar   : TVariable;
             CFGCost: uint32;
            End;

// SortVarRec
Function SortVarRec(const A, B: PVar): Integer;
Begin
 if (A^.CFGCost > B^.CFGCost) Then
  Result := -1 Else
 if (A^.CFGCost < B^.CFGCost) Then
  Result := 1 Else
  Result := 0;
End;

(* Parse *)
{
 Parses and compiles functions.
}
Procedure Parse(Compiler: Pointer);
Var Func: TFunction; // our new function

    NamedParams        : (npNotSetYet, npYes, npNo) = npNotSetYet;
    RequireDefaultValue: Boolean = False;
    DefaultValueType   : TType;

    VisitedNodes: TCFGNodeList;
    RemovedNodes: TCFGNodeList; // list of removed nodes

  {$I tree_ssa.pas}
  {$I bytecode_generator.pas}

  { NewConst } // (used to create internal constants, if enabled)
  Procedure NewConst(const cName: String; cTyp: TType; cValue: PExpressionNode);
  Var Variable: TVariable;
  Begin
   With TCompiler(Compiler) do
   Begin
    Variable       := TVariable.Create;
    Variable.Typ   := cTyp;
    Variable.Value := cValue;

    With Variable.RefSymbol do
    Begin
     Name          := cName;
     mCompiler     := Compiler;
     DeclToken     := Parser.next_pnt;
     DeclFunction  := getCurrentFunction;
     DeclNamespace := getCurrentNamespace;
    End;

    Variable.Attributes += [vaConst, vaDontAllocate];

    if (getCurrentFunction.findSymbol(cName) = nil) Then // don't duplicate
     With getCurrentFunction do
     Begin
      SymbolList.Add(TSymbol.Create(stConstant, False));
      SymbolList.Last.mVariable := Variable;
     End;
   End;
  End;

  { ReadParamList }
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
        DefaultValueType := getTypeFromExpression(DefaultValue);
        Dec(TokenPos);

        if (not DefaultValueType.CanBeAssignedTo(Typ)) Then
         CompileError(eWrongType, [DefaultValueType.asString, Typ.asString]);

        RequireDefaultValue := True;
       End Else
        if (RequireDefaultValue) Then
         CompileError(eDefaultParamValueRequired, [Name]) Else
         DefaultValue := nil;
      End;

      if (next_t = _BRACKET1_CL) Then
       Break Else
       eat(_COMMA); // parameters are separated by comma
     End;

    eat(_BRACKET1_CL); // read remaining parenthesis
   End;
  End;

  { ReadAttributes }
  Procedure ReadAttributes;
  Var Token: TToken_P;
  Begin
   With TCompiler(Compiler), Parser do
   Begin
    Func.Attributes := [];

    While (true) Do
    Begin
     Token := read;
     Case Token.Token of
      _NAKED: Include(Func.Attributes, faNaked);

      else
       Break;
     End;
    End;
    Dec(TokenPos);
   End;
  End;

// main function block
Var I           : Integer;
    TmpNode     : TCFGNode;
    TmpType     : TType;
    TmpSymbol   : TSymbol;
    TmpNamespace: TNamespace;
    TmpIdent    : String;
Begin
With TCompiler(Compiler), Parser do
Begin
 (* if first pass *)
 if (CompilePass = _cp1) Then
 Begin
  if (PreviousInstance <> nil) Then
  Begin
   (*
    @Note: the situation is a bit messy here.

    Let's say, that we have 3 files:

    -> main.ss
      @("a.ss")

      function<int> main()
      {
       return null;
      }

    -> a.ss
      @("b.ss")

      function<void> a()
      {
       b();
      }

    -> b.ss
      @("a.ss")

      function<void> b()
      {
       a();
      }

    As you can see, here we have circular reference between files `a.ss` and `b.ss`, and also one between functions `a` and `b`.
    To solve it (as we must support circular references), compiler has two compilation passes (see `doc/multi-pass compilation.txt`).
    As we are here, we're in the first pass: function's header scanning.
    And also one more condition is met: the 'include stack' looks like this:

    main.ss -> a.ss -> b.ss -> a.ss |< we are here
                 ^                ^
                 |                --------------------------
                 |                                         |
                 --------------------------------          |
    The "PreviousInstance" pointer points there ^          |
    The current compiler instance ("Compiler") points here ^

    Since this function had been already parsed in the "PreviousInstance", we have to use that symbol pointer.
    Why we have to?
    Becase when a function is called, it may (and most likely will) not have assigned its "TFunction.MangledName" assigned yet (it's empty), thus
    call would look like this: `call(:)`, oops: no label name, because it's unknown yet! And as the name is generated in the second pass, for our
    instance such would never be generated! (because at least double included files are only first-pass parsed)
    To prevent it we use a simple trick: instead of calling label's name, there's used construction:
    => call(:$function.FUNCTION_CLASS_POINTER)
    Eg.
    => call(:$function.23123095) <- this number is an address (pointer) to the "TFunction" class of the callee function.
    The "$function.FUNCTION_CLASS_POINTER" is a temporary label name resolved inside the bytecode compiler, where everything has been already
    parsed and is known.

    I think it's all the magic here ;)
   *)

   skip_parenthesis; // skip function read type
   TmpIdent := read_ident;
   skip_parenthesis; // skip function parameter list
   While not (next_t in [_BRACKET3_OP, _SEMICOLON]) do
    read;

   TmpSymbol := nil;

   For TmpNamespace in PreviousInstance.NamespaceList Do
    if (TmpNamespace.RefSymbol.Name = getCurrentNamespace.RefSymbol.Name) Then
    Begin
     TmpSymbol := TmpNamespace.findSymbol(TmpIdent);
     Break;
    End;

   if (TmpSymbol = nil) Then
    CompileError(eInternalError, ['TmpSymbol = nil']);

   getCurrentNamespace.SymbolList.Add(TmpSymbol);
  End Else
  Begin
   Func                         := TFunction.Create;
   CurrentFunction              := Func;
   Func.RefSymbol.mCompiler     := Compiler;
   Func.RefSymbol.DeclToken     := next_pnt(-1); // `_FUNCTION`
   Func.RefSymbol.DeclNamespace := getCurrentNamespace;
   Func.RefSymbol.DeclFunction  := nil;

   { skip function return type (it will be read in the second pass) }
   Func.Return := nil;

   if (next_t = _LOWER) Then // if explicit type is specified, skip it
    skip_parenthesis;

   { read function name }
   Func.RefSymbol.Name       := read_ident; // [identifier]
   Func.RefSymbol.Visibility := getVisibility;
   Func.ModuleName           := ModuleName;

   RedeclarationCheck(Func.RefSymbol.Name); // check for redeclaration

   { read parameter list }
   ReadParamList;

   { read special attributes }
   ReadAttributes;

   { add this function into the symbol list }
   With getCurrentNamespace do
   Begin
    SymbolList.Add(TSymbol.Create(stFunction, Func));

    With SymbolList.Last do
    Begin
     mVariable                         := TVariable.Create;
     mVariable.RefSymbol.Name          := Func.RefSymbol.Name;
     mVariable.RefSymbol.DeclFunction  := nil;
     mVariable.RefSymbol.DeclNamespace := getCurrentNamespace;
     mVariable.Typ                     := CreateFunctionType(Func);
     mVariable.Value                   := MakeIntExpression('@$function.'+IntToStr(uint32(Pointer(Func))));

     mVariable.Attributes += [vaConst, vaDontAllocate]; // const, don't allocate

     Func.RefVar := mVariable;
    End;
   End;

   if (NamedParams = npNo) and (next_t <> _SEMICOLON) Then
    CompileError(next, eExpected, [';', next.Value]);

   { add parameters }
   With Func do
    For I := Low(ParamList) To High(ParamList) Do
     __variable_create_stackpos(ParamList[I].Name, ParamList[I].Typ, -I-1, [vaFuncParam, vaDontAllocate]+ParamList[I].Attributes);

   { add special constants (if `--internal-const` enabled) }
   if (getBoolOption(opt_internal_const)) Then
   Begin
    NewConst('__self', TYPE_STRING, MakeStringExpression(Func.RefSymbol.Name));
   End;
  End;

  SkipCodeBlock;
  Exit;
 End Else

 (* if second pass *)
 if (CompilePass = _cp2) Then
 Begin
  if (next_t = _LOWER) Then // explicit user type
  Begin
   eat(_LOWER);
   TmpType := read_type; // return type
   eat(_GREATER);
  End Else // implicit 'void'
  Begin
   TmpType := TYPE_VOID;
  End;

  Func := findFunction(read_ident);
  skip_parenthesis; // param list
  While not (next_t in [_BRACKET3_OP, _SEMICOLON]) do
   read;

  Func.Return                := TmpType;
  Func.RefVar.Typ.FuncReturn := TmpType;

  // do some type-checks
  if (Func.RefSymbol.Visibility = mvPublic) Then
  Begin
   if (Func.Return.RefSymbol.Visibility = mvPrivate) and (not Func.Return.RefSymbol.isInternal) Then
    CompileWarning(next(-1), wPublicFunctionUsesPrivateSymbol, [Func.RefSymbol.Name, Func.Return.RefSymbol.Name]);
  End;

  // generate mangled label name
  Func.LabelName  := Func.getSerializedForm;
  CurrentFunction := Func;

  { new label (function begin) }
  With PutLabel(Func.LabelName)^ do
  Begin
   isPublic   := (Func.RefSymbol.Visibility = mvPublic);
   isFunction := True;
  End;

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
   With Func.FlowGraph do
   Begin
    Validate;
    CheckReturns(Compiler, Func.Return.isVoid or Func.isNaked);
   End;

   if (TCompiler(Compiler).getBoolOption(opt__tree_simplify)) Then
   Begin
    {
     @Note:
     Tree simplification can be done before generating the SSA form, because it doesn't use the SSA, and as it changes the whole expression trees, after
     this optimization new SSA had to be generated again anyway.
    }
    DevLog(dvInfo, 'Parse', 'Simplifing expression trees...');
    While (TreeSimplify) do;
   End;

   DevLog(dvInfo, 'Parse', 'Generating SSA form...');
   GenerateSSA;

   if (getBoolOption(opt__dump_cfg)) Then
    SaveGraph(Func.FlowGraph, 'not_optimized/'+Func.RefSymbol.Name+'.d');

   DevLog(dvInfo, 'Parse', 'Optimizing expressions...');
   OptimizeExpressions;

   if (getBoolOption(opt__optimize_branches)) Then
   Begin
    DevLog(dvInfo, 'Parse', 'Optimizing branches...');
    OptimizeBranches;
   End;

   if (getBoolOption(opt__remove_dead)) Then
   Begin
    DevLog(dvInfo, 'Parse', 'Removing dead code...');
    RemoveUnusedAssigns;
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
    We're compiling removed nodes to avoid situations like this:

    if (true)
     main(); else
     this_would_not_be_compiled_and_thus_no_error_would_have_been_raised*2;
   *)

   RemovedNodes.Free;
   VisitedNodes.Free;
  End;

  DevLog(dvInfo, 'Parse', 'Function '''+Func.RefSymbol.Name+''' has been compiled!');
  DevLog;

  if (getBoolOption(opt__dump_cfg)) Then
   SaveGraph(Func.FlowGraph, 'optimized/'+Func.RefSymbol.Name+'.d');

  RemoveScope; // ... and - as we finished compiling this function - remove scope
 End;

 CurrentFunction := nil;
End;
End;
End.
