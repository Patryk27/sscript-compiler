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
With TCompiler(Compiler), Parser do
Begin
 eat(_BRACKET1_OP); // (

 Str := CopyStringToPChar(getCurrentFunction.MangledName+'__while_'+IntToStr(SomeCounter)+'_');

 Inc(SomeCounter);

 C.Typ := ctWHILE;
 SetLength(C.Values, 2);

 NewScope(sWHILE, Str+'condition', Str+'end');
 Inc(CurrentDeep);

 { condition }
 Condition := ExpressionCompiler.MakeConstruction(TCompiler(Compiler), [_BRACKET1_CL]);

 C.Values[0] := Condition.Values[0];
 C.Values[1] := Str;
 AddConstruction(C);

 { parse loop's code }
 ParseCodeBlock(True);

 { loop end }
 C.Typ := ctWHILE_end;
 SetLength(C.Values, 0);
 AddConstruction(C);

 { remove scope }
 Dec(CurrentDeep);
 RemoveScope;
End;
End;

{ Parse_DO_WHILE }
Procedure Parse_DO_WHILE(Compiler: Pointer);
Var Str         : PChar;
    Condition, C: TMConstruction;
Begin
With TCompiler(Compiler), Parser do
Begin
 Str := CopyStringToPChar(getCurrentFunction.MangledName+'__do_while_'+IntToStr(SomeCounter)+'_');

 Inc(SomeCounter);

 NewScope(sWHILE, Str+'begin', Str+'end');
 Inc(CurrentDeep);

 { loop begin }
 SetLength(C.Values, 1);
 C.Typ       := ct_DO_WHILE;
 C.Values[0] := Str;
 AddConstruction(C);

 { parse loop's code }
 ParseCodeBlock(True);

 { condition }
 eat(_WHILE);
 eat(_BRACKET1_OP); // (
 Condition := ExpressionCompiler.MakeConstruction(TCompiler(Compiler), [_BRACKET1_CL]);
 semicolon;

 { loop end }
 SetLength(C.Values, 1);
 C.Typ       := ct_DO_WHILE_END;
 C.Values[0] := Condition.Values[0];
 AddConstruction(C);

 { remove scope }
 Dec(CurrentDeep);
 RemoveScope;
End;
End;
End.
