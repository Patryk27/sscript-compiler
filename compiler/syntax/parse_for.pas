(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_FOR;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, SysUtils, Tokens, MTypes, Messages, Opcodes;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Str               : PChar;
    Condition, Step, C: TMConstruction;
Begin
With TCompiler(Compiler) do
Begin
 eat(_BRACKET1_OP); // (

 Str := GetMem(128);
 StrPCopy(Str, getCurrentFunction.MName+'__for_'+IntToStr(SomeCounter)+'_');

 Inc(SomeCounter);

 C.Typ := ctFOR;
 SetLength(C.Values, 3);

 NewScope(sFOR, Str+'condition', Str+'end');
 Inc(CurrentDeep);

 // init instruction
 ParseToken;

 // condition
 Condition := ExpressionCompiler.MakeConstruction(TCompiler(Compiler), [_SEMICOLON]);

 // step
 Step        := ExpressionCompiler.MakeConstruction(TCompiler(Compiler), [_BRACKET1_CL]);
 C.Values[0] := Condition.Values[0];
 C.Values[1] := Step.Values[0];
 C.Values[2] := Str;
 AddConstruction(C);

 ParseCodeBlock(True); // parse 'for' loop

 C.Typ := ctFOR_end;
 SetLength(C.Values, 0);
 AddConstruction(C);

 Dec(CurrentDeep);
 RemoveScope;
End;
End;
End.
