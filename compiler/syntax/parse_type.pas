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
 eat(_LOWER); // `<`
 Base := read_type; // [type]
 eat(_GREATER); // `>`

 Typ            := Base.Clone;
 Typ.Name       := read_ident; // [identifier]
 Typ.DeclToken  := next_pnt(-1);
 Typ.mCompiler  := Compiler;
 Typ.Visibility := Visibility;

 semicolon;

 RedeclarationCheck(Typ.Name);

 if (inFunction) Then // local type
 Begin
  CompileError(eUnimplemented, ['local type declarations']);
 End Else // global type
 Begin
  With getCurrentNamespace do
  Begin
   SetLength(SymbolList, Length(SymbolList)+1);
   SymbolList[High(SymbolList)]       := TGlobalSymbol.Create;
   SymbolList[High(SymbolList)].Typ   := gsType;
   SymbolList[High(SymbolList)].mType := Typ;
  End;
 End;
End;
End;
End.
