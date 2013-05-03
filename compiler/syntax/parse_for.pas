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
With TCompiler(Compiler), Parser do
Begin
 eat(_BRACKET1_OP); // (

 Str := CopyStringToPChar(getCurrentFunction.MangledName+'__for_'+IntToStr(SomeCounter)+'_');
 Inc(SomeCounter);

 C.Typ := ctFOR;
 SetLength(C.Values, 3);

 NewScope(sFOR, Str+'step', Str+'end');
 Inc(CurrentDeep);

 ParsingFORInitInstruction := True;

 // init instruction
 if (next_t in [_VAR, _SEMICOLON]) Then
  ParseToken Else
  AddConstruction(ExpressionCompiler.MakeConstruction(Compiler));

 ParsingFORInitInstruction := False;

 // condition
 if (next_t = _SEMICOLON) Then
 Begin
  read;
  Condition.Typ := ctNone;
 End Else
  Condition := MakeConstruction(Compiler, [_SEMICOLON]);

 // step
 if (next_t = _BRACKET1_CL) Then
 Begin
  read;
  Step.Typ := ctNone;
 End Else
  Step := MakeConstruction(Compiler, [_BRACKET1_CL]);

 if (Condition.Typ <> ctNone) Then
  C.Values[0] := Condition.Values[0] Else
  C.Values[0] := nil;

 if (Step.Typ <> ctNone) Then
  C.Values[1] := Step.Values[0] Else
  C.Values[1] := nil;

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
