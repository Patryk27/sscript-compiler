(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_TYPE;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, Messages, Tokens, symdef;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Base, Typ    : TType;
    isEnum       : Boolean = False;
    EnumItem     : TVariable;
    EnumPrevValue: Int64 = -1;
    SymbolList   : TSymbolList;
Begin
With TCompiler(Compiler), Parser do
Begin
 if not ((CompilePass = _cp1) or (inFunction)) Then // `type` is parsed in the first pass or inside function
 Begin
  read_until(_SEMICOLON);
  Exit;
 End;

 if (inFunction) Then
  SymbolList := getCurrentFunction.SymbolList Else
  SymbolList := getCurrentNamespace.SymbolList;

 eat(_LOWER); // `<`

 if (next_t = _ENUM) Then // if enum...
 Begin
  eat(_ENUM);
  isEnum := True;
 End Else
  Base := read_type; // [type]

 eat(_GREATER); // `>`

 (* enum *)
 if (isEnum) Then
 Begin
  Typ := TYPE_INT;

  Typ.Attributes += [taEnum];

  With Typ.RefSymbol do
  Begin
   Name       := read_ident; // [identifier]
   DeclToken  := next_pnt(-1);
   mCompiler  := Compiler;
   Visibility := getVisibility;
   Range      := getCurrentRange;
   isInternal := False;

   RedeclarationCheck(Name);
  End;

  eat(_EQUAL); // `=`
  eat(_BRACKET3_OP); // `{`

  { enum items }
  While (true) Do
  Begin
   EnumItem     := TVariable.Create;
   EnumItem.Typ := TYPE_INT;
   EnumItem.Attributes += [vaConst, vaEnumItem];

   With EnumItem.RefSymbol do
   Begin
    Name       := read_ident; // [identifier]
    DeclToken  := next_pnt(-1);
    mCompiler  := Compiler;
    Visibility := getVisibility;
    Range      := getCurrentRange;
    isInternal := False;

    RedeclarationCheck(Name);
   End;

   SymbolList.Add(TSymbol.Create(stConstant, EnumItem));

   if (next_t = _EQUAL) Then
   Begin
    eat(_EQUAL);
    EnumItem.Value := MakeIntExpression(read_constant_expr_int);
    Dec(TokenPos);
   End Else
    EnumItem.Value := MakeIntExpression(EnumPrevValue+1);

   Typ.EnumItemList.Add(EnumItem); // insert into list

   EnumPrevValue := EnumItem.Value^.Value;

   if (next_t = _BRACKET3_CL) Then // if end
   Begin
    eat(_BRACKET3_CL);
    Break;
   End Else
    eat(_COMMA);
  End;
 End Else

 (* not enum *)
 Begin
  Typ := Base.Clone;

  With Typ.RefSymbol do
  Begin
   Name       := read_ident; // [identifier]
   DeclToken  := next_pnt(-1);
   mCompiler  := Compiler;
   Visibility := getVisibility;
   Range      := getCurrentRange;
   isInternal := False;

   RedeclarationCheck(Name);
  End;
 End;

 semicolon;

 SymbolList.Add(TSymbol.Create(stType, Typ));
End;
End;
End.
