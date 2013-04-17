(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_TYPE;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, Messages, Tokens, MTypes, symdef;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Base, Typ: TType;
Begin
With TCompiler(Compiler), Parser do
Begin
 if not ((CompilePass = cp1) or (inFunction)) Then // `type` is parsed in second pass or inside function
 Begin
  read_until_semicolon;
  Exit;
 End;

 eat(_LOWER); // `<`
 Base := read_type; // [type]
 eat(_GREATER); // `>`

 Typ := Base.Clone;

 With Typ.RefSymbol do
 Begin
  Name       := read_ident; // [identifier]
  DeclToken  := next_pnt(-1);
  mCompiler  := Compiler;
  Visibility := Visibility;
  Range      := getCurrentRange;

  RedeclarationCheck(Name);
 End;

 semicolon;

 if (inFunction) Then // local type
 Begin
  With getCurrentFunction do
  Begin
   SymbolList.Add(TLocalSymbol.Create(lsType, False));
   SymbolList.Last.mType := Typ;
  End;
 End Else // global type
 Begin
  With getCurrentNamespace do
  Begin
   SymbolList.Add(TGlobalSymbol.Create(gsType, False));
   SymbolList.Last.mType := Typ;
  End;
 End;
End;
End;
End.
