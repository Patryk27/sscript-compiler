(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_VAR;

 Interface

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses SSCompiler, Expression, ExpressionCompiler, Tokens, symdef, Messages, Opcodes, FlowGraph;

(* ReadArrayInitializer *)
Procedure ReadArrayInitializer(const CompilerPnt, SymbolPnt: Pointer);
Var Compiler: TCompiler absolute CompilerPnt;
    Symbol  : TSymbol absolute SymbolPnt;
    Variable: TVariable;
    Values  : Array of PExpressionNode;
    DimSizes: Array of uint32;
    DimCount: uint8 = 0;

    Node: TCFGNode;

  // ReadDimension
  Procedure ReadDimension(const Dimension: uint8);
  Var ElementCount: uint32 = 0;
  Begin
   With Compiler, Parser do
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
 Node                            := TCFGNode.Create(Compiler.fCurrentNode, cetArrayInitializer, nil, Compiler.Parser.getTokenPnt(Compiler.Parser.getPosition));
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
Begin
 With Compiler, Parser do
 Begin
  if (not ((CompilePass = _cp1) or (inFunction))) Then // variables are parsed in the first pass or inside a function
  Begin
   read_until(_SEMICOLON);
   Exit;
  End;

  if (inFunction) Then // choose the symbol list - either current function's or namespace's.
   SymbolList := getCurrentFunction.SymbolList Else
   SymbolList := getCurrentNamespace.SymbolList;

  eat(_LOWER); // <
  VarType := read_type; // [type]
  eat(_GREATER); // >

  { read variables declarations }
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
    Name          := read_ident; // [identifier]
    Visibility    := getVisibility;

    RedeclarationCheck(Name); // check for redeclaration of the variable
   End;

   if (Variable.Typ.isVoid) Then // cannot create a void-variable
    CompileError(eVoidVar, [Variable.RefSymbol.Name]);

   if (Variable.Typ.isArray(False)) Then
    Variable.Attributes += [vaVolatile]; // arrays have to be volatile because - as optimizer doesn't support arrays - weird things happen when it tries to optimize them; @TODO

   if (not inFunction) Then // if it's a global variable, we have to do a bit more magic
   Begin
    Variable.LocationData.Location      := vlMemory;
    Variable.LocationData.MemSymbolName := Variable.getSerializedForm;
   End;

   { add variable into the function }
   SymbolList.Add(TSymbol.Create(stVariable, Variable));

   if (ParsingForeachHeader) Then
   Begin
    eat(_IN);
    Dec(TokenPos);
    Break;
   End;

   if (next_t = _BRACKET1_OP) { ( } Then // optional array initializer
   Begin
    if (not inFunction) Then
     CompileError(eUnimplemented, ['global variable initializers']);

    if (Variable.Typ.isArray(False)) Then
     ReadArrayInitializer(Compiler, SymbolList.Last) Else
     CompileError(eVarArrayRequired);
   End Else

   if (next_t = _EQUAL) { = } Then // or optional default initializer
   Begin
    if (inFunction) Then // local var initializer
    Begin
     Dec(TokenPos);
     CFGAddNode(TCFGNode.Create(fCurrentNode, cetExpression, ExpressionCompiler.MakeExpression(Compiler, [_SEMICOLON, _COMMA])));
     Dec(TokenPos); // ExpressionCompiler 'eats' comma.
    End Else // global var initializer
    Begin
     CompileError(eUnimplemented, ['global variable initializers']);
    End;
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
