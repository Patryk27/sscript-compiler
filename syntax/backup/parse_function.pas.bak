(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_FUNCTION;

 Interface
 Uses SysUtils, Classes, Expression, Tokens, Variants, TypInfo;

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses Logging, CommandLine,
     HLCompiler, Messages,
     Opcodes, ExpressionParser,
     symdef, FlowGraph, SSA,
     VariableAllocator, RegisterAllocator, StackAllocator,
     CFGOptimizer, CFGBranchSimplification, CFGDeadCode, CFGExpressions,
     BCGenerator;

(* Parse *)
{
 Parses and compiles functions.
}
Procedure Parse(const CompilerPnt: Pointer);
Var Func: TFunction; // our new function

    NamedParams        : (npNotSetYet, npYes, npNo) = npNotSetYet;
    RequireDefaultValue: Boolean = False;
    DefaultValueType   : TType;

    RemovedNodes: TCFGNodeList; // list of removed nodes

  { NewConst } // used to create internal local constants
  Procedure NewConst(const cName: String; const cTyp: TType; const cValue: TConstantExpressionNode);
  Var Variable: TVariable;
  Begin
   With TCompiler(CompilerPnt) do
   Begin
    Variable       := TVariable.Create;
    Variable.Typ   := cTyp;
    Variable.Value := cValue;

    With Variable.RefSymbol do
    Begin
     Name          := cName;
     mCompiler     := CompilerPnt;
     DeclToken     := getScanner.next_pnt;
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
   // clean parameter list
   SetLength(Func.ParamList, 0);

   With TCompiler(CompilerPnt), getScanner, Func do
   Begin
    eat(_BRACKET1_OP); // eat `(`

    // special case: empty parameter list (`()`)
    if (next_t <> _BRACKET1_CL) Then
    Begin
     While (true) Do
     Begin
      // resize the parameter array
      SetLength(ParamList, Length(ParamList)+1);

      // read parameter
      With ParamList[High(ParamList)] do
      Begin
       // is it a constant parameter?
       if (next_t = _CONST) Then
       Begin
        read;
        Attributes += [vaConst];
        isConst := True;
       End Else

       // is it a pass-by-ref parameter?
       if (next_t = _VAR) Then
       Begin
        read;
        isVar := True;
       End;

       // parse parameter type
       Typ := read_type;

       // ensure it's not a "void"-typed
       if (Typ.isVoid) Then
        CompileError(eVoidParam, [Name]);

        // if `=`, `,` or `)` is next
       if (next_t in [_EQUAL, _COMMA, _BRACKET1_CL]) Then
       Begin
        if (NamedParams = npYes) Then
        Begin
         CompileError(next, eExpectedIdentifier, [next.Value]);
        End Else
        Begin
         NamedParams := npNo;
         goto SkipParamName;
        End;
       End;

       if (NamedParams = npNo) Then
        CompileError(next, eExpected, [',', next.Value]);

       // read parameter name
       Name        := read_ident;
       NamedParams := npYes;

       // check for duplicats
       For I := Low(ParamList) To High(ParamList)-1 Do
       Begin
        if (ParamList[I].Name = Name) Then
        Begin
         CompileError(eRedeclaration, [Name]);
         Break;
        End;
       End;

      SkipParamName:
       // check for default parameter value
       if (next_t = _EQUAL) Then
       Begin
        // eat '='
        eat(_EQUAL);

        // read and parse value
        DefaultValue := readConstantExpression;

        // move one token back
        Dec(TokenPos);

        // fetch value type
        if (DefaultValue is TConstantExpressionNode) Then
         DefaultValueType := TType(TConstantExpressionNode(DefaultValue).getType) Else
         DefaultValueType := nil;

        // do type-check
        if (DefaultValueType <> nil) and (not DefaultValueType.CanBeAssignedTo(Typ)) Then
         CompileError(eWrongType, [DefaultValueType.asString, Typ.asString]);

        RequireDefaultValue := True;
       End Else
       Begin
        if (RequireDefaultValue) Then
         CompileError(eDefaultParamValueRequired, [Name]) Else
         DefaultValue := nil;
       End;
      End;

      // move further
      if (next_t = _BRACKET1_CL) Then
       Break Else
       eat(_COMMA); // parameters are separated by comma
     End;
    End;

    eat(_BRACKET1_CL); // read remaining parenthesis
   End;
  End;

  { ReadAttributes }
  Procedure ReadAttributes;
  Var Token: TToken_P;
  Begin
   With TCompiler(CompilerPnt), getScanner do
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

  { RunOptimizer }
  Procedure RunOptimizer(const OptClass: TCFGOptimizer);
  Begin
   OptClass.Execute;
   OptClass.Free;
  End;

  { DoOptimizationsAndCompile }
  Procedure DoOptimizationsAndCompile;
  Var BCGenerator: TBCGenerator = nil;
      Allocator  : TVariableAllocator;
      Compiler   : TCompiler absolute CompilerPnt;
      TmpNode    : TCFGNode;
  Begin
   RemovedNodes := TCFGNodeList.Create;

   Try
    if (not Compiler.AnyError) Then // if no errors were raised, do optimizations and run the code generator
    Begin
     // validate graph
     With Func.FlowGraph do
     Begin
      Validate;
      CheckReturns(CompilerPnt, Func.Return.isVoid or Func.isNaked);
     End;

     // --tree-simplify
     if (CmdLine.getBoolSwitch(opt__tree_simplify)) Then
     Begin
      {
       @Note:
       Tree simplification can be done before generating the SSA form because it doesn't use the SSA, and as it changes the whole expression trees, after
       this optimization new SSA would have to be generated again anyway.
      }
      DevLog(dvInfo, 'Simplifing expression trees...');
      RunOptimizer(TCFGExpressionSimplification.Create(Compiler, Func, RemovedNodes));
     End;

     // generate SSA
     DevLog(dvInfo, 'Generating SSA form...');
     With TSSAGenerator.Create(Compiler, Func) do
     Begin
      Execute;
      Free;
     End;

     // --dump-cfg
     if (CmdLine.getBoolSwitch(opt__dump_cfg)) Then // @TODO: should it be saved before --tree-simplify is performed?
     Begin
      SaveGraph(Func.FlowGraph, 'not_optimized/'+Func.RefSymbol.Name+'.d');
     End;

     // optimize expression
     DevLog(dvInfo, 'Optimizing expressions...');
     RunOptimizer(TCFGExpressionOptimization.Create(Compiler, Func, RemovedNodes)); // as it performs few optimizations at once, no "if" here (it itself decides what to do)

     // optimize branches
     if (CmdLine.getBoolSwitch(opt__optimize_branches)) Then
     Begin
      DevLog(dvInfo, 'Optimizing branches...');
      RunOptimizer(TCFGBranchSimplification.Create(Compiler, Func, RemovedNodes));
     End;

     // remove dead code
     if (CmdLine.getBoolSwitch(opt__remove_dead)) Then
     Begin
      DevLog(dvInfo, 'Removing dead code...');
      RunOptimizer(TCFGDeadCodeRemoval.Create(Compiler, Func, RemovedNodes));
     End;

     // allocate variables
     DevLog(dvInfo, 'Allocating variables...');

     if (CmdLine.getBoolSwitch(opt__register_alloc)) Then
      Allocator := TRegisterAllocator.Create(Compiler, Func) Else
      Allocator := TStackAllocator.Create(Compiler, Func);

     Try
      Allocator.Execute;
     Finally
      Allocator.Free;
     End;

     // generate bytecode
     DevLog(dvInfo, 'Generating bytecode...');
     BCGenerator := TBCGenerator.Create(Compiler);
     BCGenerator.CompileFunction(Func);
    End;
   Finally
    if (BCGenerator <> nil) Then
    Begin
     Compiler.DoNotStoreOpcodes := True;
     For TmpNode in RemovedNodes Do // compile (but not save generated bytecode of) removed nodes
      BCGenerator.CompileNode(TmpNode);
     Compiler.DoNotStoreOpcodes := False;
    End;

    (*
     @Note:

     We're compiling removed nodes to avoid situations like this:

     if (true)
      main(); else
      this_would_not_be_compiled_and_thus_no_error_would_have_been_raised*somefunc(x^3);
    *)

    RemovedNodes.Free;
    BCGenerator.Free;
   End;
  End;

// main function block
Var I           : Integer;
    TmpType     : TType;
    TmpSymbol   : TSymbol;
    TmpNamespace: TNamespace;
    TmpIdent    : String;
Begin
 With TCompiler(CompilerPnt), getScanner do
 Begin
  (* first compilation pass: scanning of function headers *)
  if (CompilePass = _cp1) Then
  Begin
   if (PreviousInstance <> nil) Then
   Begin
    (*
     @Note: the situation is a bit messy here.

     Let's say, we have 3 files:

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
     The current compiler instance ("CompilerPnt") points here ^

     Since this function had been already parsed in the "PreviousInstance", we have to use that symbol pointer.
     Why we have to?
     Becase when a function is called, it may (and most likely will) not have assigned its "TFunction.MangledName" assigned yet (it's empty) thus
     call would look like this: `call(:)`, oops: no label name because it's unknown here! And as the name is generated in the second pass, for our
     instance such would never be generated! (because at least double included files are only first-pass parsed)
     To prevent it, we use a simple trick: instead of calling label's name, there's used construction:
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
    While (not (next_t in [_BRACKET3_OP, _SEMICOLON])) do
     read;

    TmpSymbol := nil;

    For TmpNamespace in PreviousInstance.NamespaceList Do
     if (TmpNamespace.RefSymbol.Name = getCurrentNamespace.RefSymbol.Name) Then
     Begin
      TmpSymbol := TmpNamespace.findSymbol(TmpIdent);
      Break;
     End;

    if (TmpSymbol = nil) Then
     raise EHLCompilerException.Create('TmpSymbol = nil');

    getCurrentNamespace.SymbolList.Add(TmpSymbol);
   End Else
   Begin
    Func                         := TFunction.Create;
    CurrentFunction              := Func;
    Func.RefSymbol.mCompiler     := CompilerPnt;
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
//      mVariable.Value                   := MakeIntExpression('@$function.'+IntToStr(uint32(Pointer(Func))));
      {$WARNING unimplemented: creating corresponding global function variable}

      mVariable.Attributes += [vaConst, vaDontAllocate]; // const, don't allocate

      Func.RefVar := mVariable;
     End;
    End;

    if (NamedParams = npNo) and (next_t <> _SEMICOLON) Then
     CompileError(next, eExpected, [';', next.Value]);

    { add parameters }
    With Func do
    Begin
     For I := Low(ParamList) To High(ParamList) Do
      __variable_create_stackpos(ParamList[I].Name, ParamList[I].Typ, -I-1, [vaFuncParam, vaDontAllocate]+ParamList[I].Attributes);
    End;

    { add special constants (if `--internal-const` enabled) }
    if (CmdLine.getBoolSwitch(opt__internal_const)) Then
    Begin
     NewConst('__self', TYPE_STRING, TStringExpressionNode.Create(getExpressionCompiler, next, Func.RefSymbol.Name));
    End;
   End;

   SkipCodeBlock;
   Exit;
  End Else

  (* second pass: parsing return and parameter types *)
  if (CompilePass = _cp2) Then
  Begin
   // explicit user return type
   if (next_t = _LOWER) Then
   Begin
    eat(_LOWER);
    TmpType := read_type; // return type
    eat(_GREATER);
   End Else

   // or implicit 'void'
   Begin
    TmpType := findTypeCandidate('void', getDefaultNamespace, next);
   End;

   Func := findFunction(read_ident);
   skip_parenthesis; // parameter list; @TODO
   While (not (next_t in [_BRACKET3_OP, _SEMICOLON])) do
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
   CurrentFunction := nil;

   SkipCodeBlock;
   Exit;
  End Else

  (* third pass: actual function compilation *)
  if (CompilePass = _cp3) Then
  Begin
   // skip return type and parameter list
   if (next_t = _LOWER) Then
   Begin
    skip_parenthesis;
   End;

   Func            := findFunction(read_ident);
   CurrentFunction := Func;

   skip_parenthesis;
   While (not (next_t in [_BRACKET3_OP, _SEMICOLON])) do
    read;

   { new label (function beginning) }
   Func.FirstOpcode := PutLabel(Func.LabelName);

   With PMOpcode(Func.FirstOpcode)^ do
   Begin
    isPublic       := (Func.RefSymbol.Visibility = mvPublic);
    isFunction     := True;
    FunctionSymbol := Func.RefSymbol.DeclNamespace.findSymbol(Func.RefSymbol.Name); // @TODO: it can be done waaay better
   End;

   { new scope (because we're inside a function now) }
   NewScope(sctFunction);

   { ====== parse function's body ====== }
   setNewRootNode(nil, False);

   ParseCodeBlock; // parse!

   Func.FlowGraph.Root := getCurrentRoot;
   Func.FlowGraph.Last := getCurrentNode;

   // now we have the full control flow graph of this function, so let's optimize and generate bytecode
   DoOptimizationsAndCompile;

   DevLog(dvInfo, 'Function '''+Func.RefSymbol.Name+''' has been compiled!');
   DevLog;

   if (CmdLine.getBoolSwitch(opt__dump_cfg)) Then
    SaveGraph(Func.FlowGraph, 'optimized/'+Func.RefSymbol.Name+'.d');

   RemoveScope; // ... and - as we finished compiling this function - remove scope
  End;

  CurrentFunction := nil; // function's been parsed!
 End;
End;
End.
