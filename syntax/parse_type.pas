(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Parse_TYPE;

 Interface

 Procedure Parse(const CompilerPnt: Pointer);

 Implementation
Uses HLCompiler, Expression, Messages, Tokens, symdef;

(* Parse *)
Procedure Parse(const CompilerPnt: Pointer);
Var Typ          : TType;
    isEnum       : Boolean = False;
    EnumItem     : TVariable;
    EnumPrevValue: int64 = -1;
    SymbolList   : TSymbolList;
Begin
 With TCompiler(CompilerPnt), getScanner do
 Begin
  // types are parsed in the first pass or inside a function
  if (not ((CompilePass = _cp1) or (inFunction))) Then
  Begin
   read_until(_SEMICOLON);
   Exit;
  End;

  // get proper symbol list
  if (inFunction) Then
   SymbolList := getCurrentFunction.SymbolList Else
   SymbolList := getCurrentNamespace.SymbolList;

  // eat `<`
  eat(_LOWER);

  // special case: enumeration type
  if (next_t = _ENUM) Then
  Begin
   eat(_ENUM);
   isEnum := True;
  End Else

  // regular type
  Begin
   Typ := read_type;
  End;

  // eat `>`
  eat(_GREATER);

  (* enum *)
  if (isEnum) Then
  Begin
   Typ := TYPE_INT;

   Typ.Attributes += [taEnum];

   With Typ.RefSymbol do
   Begin
    Name       := read_ident; // [identifier]
    DeclToken  := next_pnt(-1);
    mCompiler  := CompilerPnt;
    Visibility := getVisibility;
    Range      := getCurrentRange;
    isInternal := False;

    RedeclarationCheck(Name);
   End;

   eat(_EQUAL); // `=`
   eat(_BRACKET2_OP); // `[`

   { enum items }
   While (true) Do
   Begin
    EnumItem              := TVariable.Create;
    EnumItem.Typ          := TYPE_INT;
    EnumItem.Typ.EnumBase := Typ;

    EnumItem.Attributes += [vaConst, vaEnumItem];

    With EnumItem.RefSymbol do
    Begin
     Name          := read_ident; // [identifier]
     DeclToken     := next_pnt(-1);
     DeclNamespace := getCurrentNamespace;
     DeclFunction  := getCurrentFunction;
     mCompiler     := CompilerPnt;
     Visibility    := getVisibility;
     Range         := getCurrentRange;
     isInternal    := False;

     RedeclarationCheck(Name);
    End;

    // add new symbol onto the list
    SymbolList.Add(TSymbol.Create(stConstant, EnumItem));

    // if next token is `=`
    if (next_t = _EQUAL) Then
    Begin
     eat(_EQUAL);
     EnumItem.Value := readConstantIntExpression([_BRACKET2_CL, _COMMA]);
     Dec(TokenPos);
    End Else
    Begin
     EnumItem.Value := TIntegerExpressionNode.Create(getExpressionCompiler, next(-1), EnumPrevValue+1);
    End;

    // insert into list
    Typ.EnumItemList.Add(EnumItem);

    EnumPrevValue := EnumItem.Value.getMixedValue.getInt;

    // if reached end of the enumeration list (']')
    if (next_t = _BRACKET2_CL) Then
    Begin
     eat(_BRACKET2_CL);
     Break;
    End Else

    // eat a comma and parse further otherwise
    Begin
     eat(_COMMA);
    End;
   End;
  End Else

  (* not enum *)
  Begin
   With Typ.RefSymbol do
   Begin
    Name          := read_ident; // [identifier]
    DeclToken     := next_pnt(-1);
    DeclNamespace := getCurrentNamespace;
    DeclFunction  := getCurrentFunction;
    mCompiler     := CompilerPnt;
    Visibility    := getVisibility;
    Range         := getCurrentRange;
    isInternal    := False;

    RedeclarationCheck(Name);
   End;
  End;

  semicolon;

  SymbolList.Add(TSymbol.Create(stType, Typ));
 End;
End;
End.
