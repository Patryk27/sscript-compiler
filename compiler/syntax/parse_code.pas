(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_CODE;

 Interface

 Procedure Parse(Compiler: Pointer; const DirectlyBytecode: Boolean=False);

 Implementation
Uses Compile1, SysUtils, Messages, MTypes, Tokens, Strings;

{ Parse }
Procedure Parse(Compiler: Pointer; const DirectlyBytecode: Boolean=False);
Type TVarRecArray = Array of TVarRec;
     PVarRecArray = ^TVarRecArray;
Var Deep, I: Integer;
    Token  : TToken_P;
    Name   : PChar;
    Arg    : PChar;
    ArgList: PVarRecArray;
    C      : TMConstruction;

// NewArg
Procedure NewArg;
Const Size = 100;
Var I: Integer;
Begin
 Arg := GetMem(Size);
 For I := 0 To Size-1 Do
  Arg[I] := #0;
End;

Begin
With TCompiler(Compiler), Parser do
Begin
 if (not DirectlyBytecode) Then
  eat(_IDENTIFIER);

 Deep := CurrentDeep;
 Repeat
  Token := read;

  Case Token.Token of
   _BRACKET3_OP: Inc(CurrentDeep);
   _BRACKET3_CL: Dec(CurrentDeep);

   _AMPERSAND: // label
   Begin
    if (DirectlyBytecode) Then
     CompileError(eUnexpected, ['&']);
    PutLabel(getCurrentFunction.MangledName + read_ident, True);
    eat(_COLON);
   End;

   _IDENTIFIER: // opcode or label
   Begin
    { label }
    if (next_t = _COLON) Then
    Begin
     PutLabel(Token.Display, True);
     eat(_COLON);

     if (DirectlyBytecode) Then
     Begin
      if ((next.Display = '.') and (next(1).Display = 'public')) Then
      Begin
       read; read;
       CompileError(eUnimplemented, ['`.public` / `.private` directives']);
      End;
     End;

     Continue;
    End;

    { opcode }
    Name := CopyStringToPChar(Token.Display);

    eat(_BRACKET1_OP);
    New(ArgList);
    SetLength(ArgList^, 0);

    NewArg;

    While (true) do
    Begin
     Token := read;

     if (Token.Token = _BRACKET1_CL) Then // )
     Begin
      if (Arg[0] <> #0) Then
      Begin
       SetLength(ArgList^, Length(ArgList^)+1);
       ArgList^[High(ArgList^)].VType  := vtPChar;
       ArgList^[High(ArgList^)].VPChar := Arg;
      End;

      Break;
     End;

     if (Token.Token = _PERCENT) Then // %
     Begin
      Token := read;
      I     := findLocalVariable(Token.Display);

      if (I = -1) Then
      Begin
       Token.Display := '[0]';
       CompileError(eUnknownVariable, [Token.Display]);
      End Else
       With getCurrentFunction.VariableList[I] do
        if (MemPos <= 0) Then
         Token.Display := '['+IntToStr(MemPos)+']' Else
         Token.Display := 'e'+Typ.RegPrefix+IntToStr(MemPos);
     End;

     if (Token.Token = _AMPERSAND) Then // &
     Begin
      Token.Display := getCurrentFunction.MangledName;
     End;

     if (Token.Token = _COMMA) Then // ,
     Begin
      SetLength(ArgList^, Length(ArgList^)+1);
      ArgList^[High(ArgList^)].VType  := vtPChar;
      ArgList^[High(ArgList^)].VPChar := Arg;
      NewArg;
      Continue;
     End;

     if (Token.Token = _STRING) Then
      Token.Display := '"'+Token.Display+'"';
     Arg := StrCat(Arg, PChar(Token.Display));
    End;

    C.Typ := ctInlineBytecode;
    SetLength(C.Values, 3);
    C.Values[0] := Name;
    C.Values[1] := ArgList;
    C.Values[2] := @TokenList[TokenPos-1];
    AddConstruction(C);
   End;
  End;
 Until (Deep = CurrentDeep);
End;
End;
End.
