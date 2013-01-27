{
 return expression;
}
Unit Parse_RETURN;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, MTypes;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var C: TMConstruction;
Begin
With TCompiler(Compiler) do
Begin
 C     := ExpressionCompiler.MakeConstruction(TCompiler(Compiler));
 C.Typ := ctReturn;
 AddConstruction(C);
End;
End;
End.
