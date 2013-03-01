(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_RETURN;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, MTypes, Tokens;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var C: TMConstruction;
Begin
With TCompiler(Compiler) do
Begin
 if (next_t = _SEMICOLON) Then // `return;`
 Begin
  SetLength(C.Values, 1);

  C.Typ       := ctVoidReturn;
  C.Values[0] := @TokenList[TokenPos];

  AddConstruction(C);
  Exit;
 End;

 C     := ExpressionCompiler.MakeConstruction(TCompiler(Compiler));
 C.Typ := ctReturn;
 AddConstruction(C);
End;
End;
End.
