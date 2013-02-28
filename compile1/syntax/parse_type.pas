(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_TYPE;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, Messages, Tokens, MTypes;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Base: PMType;
    Typ : TMType;
Begin
With TCompiler(Compiler) do
Begin
 eat(_LOWER); // `<`
 Base := read_type; // [type]
 eat(_GREATER); // `>`

 Typ            := Base^;
 Typ.Name       := read_ident; // [identifier]
 Typ.DeclToken  := next(-1);
 Typ.mCompiler  := Compiler;
 Typ.Visibility := Visibility;

 semicolon;

 RedeclarationCheck(Typ.Name);

 if (inFunction) Then // local type
 Begin
  CompileError(eUnimplemented, ['local type declarations']);
 End Else // global type
 Begin
  With getCurrentNamespacePnt^ do
  Begin
   SetLength(GlobalList, Length(GlobalList)+1);
   GlobalList[High(GlobalList)].Typ   := gdType;
   GlobalList[High(GlobalList)].mType := Typ;
  End;
 End;
End;
End;
End.
