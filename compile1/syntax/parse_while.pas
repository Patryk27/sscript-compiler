(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_WHILE;

 Interface

 Procedure Parse(Compiler: Pointer);
 Procedure Parse_DO_WHILE(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, SysUtils, Tokens, MTypes, Messages, Opcodes;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var Str         : PChar;
    Condition, C: TMConstruction;
Begin
With TCompiler(Compiler) do
Begin
 eat(_BRACKET1_OP); // (

 Str := GetMem(128);
 StrPCopy(Str, FunctionList[High(FunctionList)].MName+'__while_'+IntToStr(SomeCounter)+'_');

 Inc(SomeCounter);

 C.Typ := ctWHILE;
 SetLength(C.Values, 2);

 NewScope(sWHILE, Str+'condition', Str+'end');
 Inc(CurrentDeep);

 // condition
 Condition := ExpressionCompiler.MakeConstruction(TCompiler(Compiler), [_BRACKET1_CL]);

 C.Values[0] := Condition.Values[0];
 C.Values[1] := Str;
 AddConstruction(C);

 // parse loop's code
 ParseCodeBlock(True);

 C.Typ := ctWHILE_end;
 SetLength(C.Values, 0);
 AddConstruction(C);

 Dec(CurrentDeep);
 RemoveScope;
End;
End;

{ Parse_DO_WHILE }
Procedure Parse_DO_WHILE(Compiler: Pointer);
Var Str         : PChar;
    Condition, C: TMConstruction;
Begin
With TCompiler(Compiler) do
Begin
 Str := GetMem(128);
 StrPCopy(Str, FunctionList[High(FunctionList)].MName+'__do_while_'+IntToStr(SomeCounter)+'_');

 Inc(SomeCounter);

 C.Typ := ct_DO_WHILE;
 SetLength(C.Values, 2);

 NewScope(sWHILE, Str+'begin', Str+'end');
 Inc(CurrentDeep);

 // parse loop's code
 ParseCodeBlock(True);

 // condition
 eat(_WHILE);
 eat(_BRACKET1_OP); // (
 Condition := ExpressionCompiler.MakeConstruction(TCompiler(Compiler), [_BRACKET1_CL]);
 semicolon;

 C.Values[0] := Condition.Values[0];
 C.Values[1] := Str;
 AddConstruction(C);

 C.Typ := ct_DO_WHILE_end;
 SetLength(C.Values, 0);
 AddConstruction(C);

 Dec(CurrentDeep);
 RemoveScope;
End;
End;
End.
