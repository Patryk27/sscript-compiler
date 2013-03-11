(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_TRY_CATCH;

 Interface
 Uses SysUtils;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, MTypes, Opcodes, Tokens;

{ Parse }
Procedure Parse(Compiler: Pointer);
Var m_try, m_catch, m_catch_end: TMConstruction;
Begin
With TCompiler(Compiler) do
Begin
 NewScope(sTryCatch);

 { parse `try` }
 m_try.Typ := ctTRY;
 SetLength(m_try.Values, 1);
 m_try.Values[0] := CopyStringToPChar(getCurrentFunction.MName+'__exception_catch_'+IntToStr(SomeCounter)+'_');
 AddConstruction(m_try);

 ParseCodeBlock(False);

 { parse `catch` }
 eat(_CATCH);
 eat(_BRACKET1_OP);
 eat(_BRACKET1_CL);

 m_catch.Typ := ctCATCH;
 SetLength(m_catch.Values, 0);
 Inc(SomeCounter);
 AddConstruction(m_catch);

 ParseCodeBlock(False);

 { `catch` end }
 m_catch_end.Typ := ctCATCH_END;
 SetLength(m_catch_end.Values, 0);
 AddConstruction(m_catch_end);

 RemoveScope;
End;
End;

End.
