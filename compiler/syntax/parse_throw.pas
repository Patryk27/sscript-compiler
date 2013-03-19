(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_THROW;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, MTypes, ExpressionCompiler, Tokens;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var M: TMConstruction;
Begin
With TCompiler(Compiler) do
Begin
 M.Typ := ctTHROW;
 SetLength(M.Values, 1);
 M.Values[0] := MakeConstruction(Compiler).Values[0];

 AddConstruction(M);
End;
End;

End.
