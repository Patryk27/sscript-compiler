(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_VAR;

 Interface

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses HLCompiler, Expression, ExpressionCompiler, Tokens, symdef, Messages, Opcodes, FlowGraph;

(* ReadArrayInitializer *)
Procedure ReadArrayInitializer(const CompilerPnt, SymbolPnt: Pointer);
Var Compiler: TCompiler absolute CompilerPnt;
    Symbol  : TSymbol absolute SymbolPnt;
    Variable: TVariable;
    Values  : Array of PExpressionNode;
    DimSizes: Array of uint32;
    DimCount: uint8 = 0;

    Node: TCFGNode;

  { ReadDimension }
  Procedure ReadDimension(const Dimension: uint8);
  Var ElementCount: uint32 = 0;
  Begin
   With Compiler, getScanner do
   Begin
    if (Dimension >= DimCount) Then
     DimCount := Dimension;

    eat(_BRACKET1_OP);

    While (true) Do
    Begin
     if (next_t = _BRACKET1_OP) Then
     Begin
      ReadDimension(Dimension+1);
     End Else
     Begin
      SetLength(Values, Length(Values)+1);
      Values[High(Values)] := MakeExpression(CompilerPnt, [_COMMA, _BRACKET1_CL]);
      Dec(TokenPos); // expression compiler 'eats' the last token
     End;

     Inc(ElementCount);

     if (next_t = _BRACKET1_CL) Then
     Begin
      eat(_BRACKET1_CL);
      Break;
     End Else
      eat(_COMMA);
    End;

    if (DimSizes[Dimension] = 0) Then
    Begin
     DimSizes[Dimension] := ElementCount;
    End Else

    if (ElementCount > DimSizes[Dimension]) Then // too many elements
    Begin
     CompileError(eExpectedFewerElements, [ElementCount-DimSizes[Dimension]]);
    End Else

    if (ElementCount < DimSizes[Dimension]) Then // not enough elements
    Begin
     CompileError(eExpectedMoreElements, [DimSizes[Dimension]-ElementCount]);
    End;
   End;
  End;

Begin
 Variable := Symbol.mVariable;

 { initialize arrays }
 SetLength(Values, 0);
 SetLength(DimSizes, 256);

 { parse }
 ReadDimension(0);

 { check dimension count }
 Inc(DimCount);

 if (Variable.Typ.isString) Then
  Inc(DimCount); // temporarily treat string arrays as arrays of char arrays (:P)

 if (DimCount <> Variable.Typ.ArrayDimCount) Then
  Compiler.CompileError(eInvalidArrayInitializer, [DimCount, Variable.Typ.ArrayDimCount]);

 { add node to the flowgraph }
 Node                            := TCompiler(CompilerPnt).getCurrentFunction.createNode(Compiler.fCurrentNode, cetArrayInitializer, nil, Compiler.getScanner.getTokenPnt(Compiler.getScanner.getPosition));
 Node.ArrayInitializer.VarSymbol := Symbol;
 Node.ArrayInitializer.Values    := Values;
 Node.ArrayInitializer.DimSizes  := DimSizes;
 Compiler.CFGAddNode(Node);
End;

// -------------------------------------------------------------------------- //
(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Var Compiler  : TCompiler absolute CompilerPnt;
    SymbolList: TSymbolList;
    Variable  : TVariable;
    VarType   : TType;

    Value: PExpressionNode;
    Token: TToken_P;
Begin
 With Compiler, getScanner do
 Begin
  // variables are parsed in the first pass or inside a function
  if (not ((CompilePass = _cp1) or (inFunction))) Then
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

    Value := ExpressionCompiler.MakeExpression(Compiler, [_SEMICOLON, _COMMA]);

    Dec(TokenPos);

    // local var initializer
    if (inFunction) Then
    Begin;
     CFGAddNode(getCurrentFunction.createNode(fCurrentNode, cetExpression, Value));
    End Else

    // global var initializer
    Begin
     Variable.Value := Value^.getValue();

     if (Variable.Value = nil) Then
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
