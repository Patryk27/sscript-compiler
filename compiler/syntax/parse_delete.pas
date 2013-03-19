(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_DELETE;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, MTypes, ExpressionCompiler, Tokens;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Expr, C: TMConstruction;
Begin
With TCompiler(Compiler), Parser do
Begin
 While (true) Do
 Begin
  Expr := ExpressionCompiler.MakeConstruction(Compiler, [_SEMICOLON, _COMMA]);

  SetLength(C.Values, 1);
  C.Typ       := ctDelete;
  C.Values[0] := Expr.Values[0];
  AddConstruction(C);

  Dec(TokenPos);
  if (next_t = _SEMICOLON) Then
   Break Else
   eat(_COMMA);
 End;
 eat(_SEMICOLON);
End;
End;
End.
