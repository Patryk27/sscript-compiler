(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_TRY_CATCH;

 Interface
 Uses SysUtils;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, MTypes, symdef, Opcodes, Tokens;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var m_try, m_catch, m_catch_end: TMConstruction;
    Variable                   : TVariable;
Begin
With TCompiler(Compiler), Parser do
Begin
 NewScope(sTryCatch);

 { parse `try` }
 m_try.Typ := ctTRY;
 SetLength(m_try.Values, 1);
 m_try.Values[0] := CopyStringToPChar(getCurrentFunction.MangledName+'__exception_catch_'+IntToStr(SomeCounter)+'_');
 AddConstruction(m_try);
 Inc(SomeCounter);

 ParseCodeBlock(False);

 { parse `catch` }
 Inc(Parser.CurrentDeep);

 eat(_CATCH);
 eat(_BRACKET1_OP);

 Variable            := TVariable.Create;
 Variable.Name       := read_ident;
 Variable.Typ        := TYPE_STRING;
 Variable.MemPos     := 0;
 Variable.DeclToken  := next_pnt;
 Variable.Deep       := CurrentDeep;
 Variable.Attributes := [vaDontAllocate];

 With getCurrentFunction do
 Begin
  SetLength(VariableList, Length(VariableList)+1);
  VariableList[High(VariableList)] := Variable;
 End;

 eat(_BRACKET1_CL);

 m_catch.Typ := ctCATCH;
 SetLength(m_catch.Values, 0);
 AddConstruction(m_catch);

 ParseCodeBlock(False);

 { `catch` end }
 Dec(CurrentDeep);
 m_catch_end.Typ := ctCATCH_END;
 SetLength(m_catch_end.Values, 0);
 AddConstruction(m_catch_end);

 RemoveScope;
End;
End;

End.
