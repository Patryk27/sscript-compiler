(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_IF;

 Interface

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, ExpressionCompiler, Tokens, SysUtils, MTypes;

Procedure Parse(Compiler: Pointer);
Var Str: PChar;
    C   : TMConstruction;
Begin
With TCompiler(Compiler), Parser do
Begin
 eat(_BRACKET1_OP); // (

 C.Typ := ctIF;
 SetLength(C.Values, 2);

 Str := CopyStringToPChar(getCurrentFunction.MangledName+'__if_'+IntToStr(SomeCounter)+'_');
 Inc(SomeCounter);

 C.Values[0] := MakeConstruction(Compiler, [_BRACKET1_CL]).Values[0]; // read condition
 C.Values[1] := Str;

 AddConstruction(C); // add construction ('if' begin)

 NewScope(sIF); // new scope
 Inc(CurrentDeep);

 ParseCodeBlock(True); // parse code block

 C.Typ := ctIF_end;
 SetLength(C.Values, 0);
 AddConstruction(C); // new construction ('if' end)

 Dec(CurrentDeep); // remove scope
 RemoveScope;

 if (next_t = _ELSE) Then // parse 'else' block
 Begin
  NewScope(sIF); // new scope
  Inc(CurrentDeep);

  eat(_ELSE);

  C.Typ := ctIF_else;
  SetLength(C.Values, 0);
  AddConstruction(C); // new construction ('else' block)
  ParseCodeBlock(True);
  C.Typ := ctIF_end;
  SetLength(C.Values, 0);
  AddConstruction(C); // new construction ('if' end)

  Dec(CurrentDeep); // remove scope
  RemoveScope;
 End;
End;
End;
End.
