(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_VAR;

 Interface

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses HLCompiler, Expression, Tokens, symdef, Messages, Opcodes, FlowGraph;

(* ReadArrayInitializer *)
Procedure ReadArrayInitializer(const CompilerPnt, SymbolPnt: Pointer);
Var Compiler: TCompiler absolute CompilerPnt;
    Symbol  : TSymbol absolute SymbolPnt;

    ExpressionDepth: int32 = -1;
    DimensionCount : uint8;

    Variable: TVariable;
    Values  : PArrayInitializerValues;
    Node    : TCFGNode;

  { ReadArray }
  Function ReadArray(const Depth: uint8): PArrayInitializerValues;
  Begin
   New(Result);

   With Compiler, getScanner do
   Begin
    eat(_BRACKET1_OP);

    // nested initializer:
    //  ((1, 2), (3, 4))
    if (next_t = _BRACKET1_OP) Then
    Begin
     // check for invalid initializer like: (1, (2, 3))
     if (ExpressionDepth <> -1) and (Depth >= ExpressionDepth) Then
     Begin
      CompileError(eUnexpected, ['(']);
     End;

     // initialize record
     Result^.Typ := aivtArray;
     SetLength(Result^.ArrayValues, 0);

     While (True) Do
     Begin
      // expand array and parse
      With Result^ do
      Begin
       SetLength(ArrayValues, Length(ArrayValues)+1);
       ArrayValues[High(ArrayValues)] := ReadArray(Depth+1);
      End;

      // check if finished
      if (next_t = _BRACKET1_CL) Then
       Break;

      eat(_COMMA);
     End;
    End Else

    // plain initializer:
    //  (1, 2)
    Begin
     if (ExpressionDepth = -1) Then
      ExpressionDepth := Depth;

     // check for invalid initializer like: ((1, 2), 3)
     if (ExpressionDepth <> Depth) Then
     Begin
      CompileError(eUnexpected, ['(']);
     End;

     // initialize record
     Result^.Typ := aivtExpression;
     SetLength(Result^.ExprValues, 0);

     While (true) Do
     Begin
      // expand array and parse expression
      With Result^ do
      Begin
       SetLength(ExprValues, Length(ExprValues)+1);
       ExprValues[High(ExprValues)] := readExpression([_BRACKET1_CL, _COMMA]);
       Dec(TokenPos);
      End;

      // check if finished
      if (next_t = _BRACKET1_CL) Then
       Break;

      eat(_COMMA);
     End;
    End;

    eat(_BRACKET1_CL);
   End;
  End;

Begin
 Variable := Symbol.mVariable;

 // parse initializer
 Values := ReadArray(0);

 // check dimension count
 DimensionCount := ExpressionDepth+1;

 if (Variable.Typ.isString) Then
  Inc(DimensionCount); // temporarily treat string arrays as arrays of char arrays

 if (DimensionCount <> Variable.Typ.ArrayDimCount) Then
  Compiler.CompileError(eInvalidArrayInitializer, [DimensionCount, Variable.Typ.ArrayDimCount]);

 // add node to the flowgraph
 Node                            := TCompiler(CompilerPnt).getCurrentFunction.createNode(Compiler.fCurrentNode, cetArrayInitializer, nil, Compiler.getScanner.getTokenPnt(Compiler.getScanner.getPosition));
 Node.ArrayInitializer.VarSymbol := Symbol;
 Node.ArrayInitializer.DimCount  := DimensionCount;
 Node.ArrayInitializer.Values    := Values;

 Compiler.CFGAddNode(Node);
End;

// -------------------------------------------------------------------------- //
(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Var Compiler  : TCompiler absolute CompilerPnt;
    SymbolList: TSymbolList;
    Variable  : TVariable;
    VarType   : TType;

    Value: TExpressionNode;
    Token: TToken_P;
Begin
 With Compiler, getScanner do
 Begin
  // variables are parsed in the second pass or inside a function
  if (not ((CompilePass = _cp2) or (inFunction))) Then
  Begin
   read_until(_SEMICOLON);
   Exit;
  End;

  // choose the symbol list - either current function's or namespace's.
  if (inFunction) Then
   SymbolList := getCurrentFunction.SymbolList Else
   SymbolList := getCurrentNamespace.SymbolList;

  eat(_LOWER); { < }
  VarType := read_type; { [type] }
  eat(_GREATER); { > }

  // read variable(s) declaration(s)
  While (true) do
  Begin
   Variable     := TVariable.Create;
   Variable.Typ := VarType.Clone;

   With Variable.RefSymbol do
   Begin
    DeclToken     := next_pnt;
    DeclNamespace := getCurrentNamespace;
    DeclFunction  := getCurrentFunction;
    mCompiler     := Compiler;
    Range         := getCurrentRange;
    Name          := read_ident; { [identifier] }
    Visibility    := getVisibility;

    // check for redeclaration
    RedeclarationCheck(Name);
   End;

   // make sure variable isn't "void"
   if (Variable.Typ.isVoid) Then
    CompileError(eVoidVar, [Variable.RefSymbol.Name]);

   // arrays have to be volatile because - as optimizer doesn't support arrays, weird things happen when it tries to optimize them; @TODO
   if (Variable.Typ.isArray(False)) Then
    Variable.Attributes += [vaVolatile];

   // add variable into the function symbol list
   SymbolList.Add(TSymbol.Create(stVariable, Variable));

   // special case: foreach loop header
   if (ParsingForeachHeader) Then
   Begin
    eat(_IN);
    Dec(TokenPos);
    Break;
   End;

   // read optional array initializer
   if (next_t = _BRACKET1_OP) { ( } Then
   Begin
    if (not inFunction) Then
     CompileError(eGlobalArrayInitializer);

    if (Variable.Typ.isArray(False)) Then
     ReadArrayInitializer(Compiler, SymbolList.Last) Else
     CompileError(eVarArrayRequired);
   End Else

   // or optional default initializer
   if (next_t = _EQUAL) { = } Then
   Begin
    if (inFunction) Then
    Begin
     Token := next;
     Dec(TokenPos);
    End Else
    Begin
     eat(_EQUAL);
     Token := next;
    End;

    Value := readExpression([_SEMICOLON, _COMMA]);
    Dec(TokenPos);

    // local var initializer
    if (inFunction) Then
    Begin;
     CFGAddNode(getCurrentFunction.createNode(fCurrentNode, cetExpression, Value));
    End Else

    // global var initializer
    Begin
     if (Value is TConstantExpressionNode) Then
      Variable.Value := TConstantExpressionNode(Value) Else
      CompileError(Token, eExpectedConstant, []);
    End;
   End;

   // if it's a global variable, we have to provide data for the linker
   {
    @Note: we can't do this anywhere before because optional variable value also
           has to be saved in the serialized form.
   }
   if (not inFunction) Then
   Begin
    Variable.LocationData.Location      := vlMemory;
    Variable.LocationData.MemSymbolName := Variable.getSerializedForm;
   End;

   if (next_t = _COMMA) Then // var(...) name1, name2, name3...
   Begin
    eat(_COMMA);
   End Else
   Begin
    semicolon;
    Break;
   End;
  End;
 End;
End;
End.
