(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_CODE;

 Interface
 Uses SysUtils, Variants;

 Procedure Parse(Compiler: Pointer; const DirectBytecode: Boolean=False);

 Implementation
Uses SSCompiler, ExpressionCompiler, Messages, Tokens, Opcodes, FlowGraph, symdef;

(* Parse *)
Procedure Parse(Compiler: Pointer; const DirectBytecode: Boolean=False);
Var Opcode: PMOpcode;

    Deep : Integer;
    Token: TToken_P;

    Variable: TVariable;

    Name   : PChar;
    Arg    : String;
    ArgList: PVarRecArray;

    Node: TCFGNode;
Begin
With TCompiler(Compiler), Parser do
Begin
 if (not DirectBytecode) Then
  eat(_IDENTIFIER); // `:CODE`

 Deep := CurrentDeep;
 Repeat
  Token := read;

  Case Token.Token of
   _BRACKET3_OP: Inc(CurrentDeep);
   _BRACKET3_CL: Dec(CurrentDeep);

   _AMPERSAND: // label
   Begin
    if (DirectBytecode) Then
     CompileError(eUnexpected, ['&']);
    PutLabel(getCurrentFunction.LabelName + read_ident, not DirectBytecode);
    eat(_COLON);
   End;

   _IDENTIFIER: // opcode or label
   Begin
    { label }
    if (next_t = _COLON) Then
    Begin
     Opcode := PutLabel(Token.Value, not DirectBytecode);
     eat(_COLON);

     if {(DirectBytecode) and} (next_t = _POINT) Then
     Begin
      eat(_POINT);

      Token := read;

      Case Token.Token of
       _PUBLIC : Opcode^.isPublic := True;
       _PRIVATE: Opcode^.isPublic := False;
       else
        CompileError(eUnknownMacro, [Token.Value]);
      End;
     End;

     Continue;
    End;

    { opcode }
    Name := CopyStringToPChar(Token.Value);

    eat(_BRACKET1_OP); { `(` }

    New(ArgList);
    SetLength(ArgList^, 0);

    While (true) do
    Begin
     Arg   := '';
     Token := read;

     if (Token.Token = _BRACKET1_CL) Then { `)` }
      Break;

     Case Token.Token of
      _STRING                       : Arg := '"'+Token.Value+'"';
      _CHAR                         : Arg := ''''+Token.Value+'''';
      _INT, _FLOAT, _IDENTIFIER, _IF: Arg := VarToStr(Token.Value);

      _HASH { # }: Arg += '#'+IntToStr(read_int);

      _MINUS { - }:
      Begin
       Token := read;

       if (Token.Token in [_INT, _FLOAT]) Then
        Arg := '-'+VarToStr(Token.Value) Else
        CompileError(Token, eExpected, ['int', Token.TokenName]);
      End;

      _BRACKET2_OP { [ }:
      Begin
       Arg := '[';
       if (next_t = _MINUS) Then // `-`
       Begin
        eat(_MINUS);
        Arg += '-';
       End;
       Arg += IntToStr(read_int)+']';
       eat(_BRACKET2_CL); // `]`
      End;

      _COLON { : }, _AT { @ }:
      Begin
       Arg := Token.Value;
       if (next_t = _AMPERSAND) and (not DirectBytecode) Then
       Begin
        eat(_AMPERSAND);
        Arg += getCurrentFunction.LabelName;
       End;
       Arg += read_ident;
      End;

      _AMPERSAND { & }:
      Begin
       if (DirectBytecode) Then
        CompileError(eUnexpected, ['&']);

       Arg := getCurrentFunction.LabelName+read_ident;
      End;

      _PERCENT { % }:
      Begin
       Arg := read_ident;

       Variable := findVariableCandidate(Arg, nil, Token);

       if (Variable = nil) Then { var not found }
       Begin
        CompileError(eUnknownVariable, [Arg]);
        Arg := 'ei1';
       End Else { var found }
       Begin
        if (Variable.RefSymbol.DeclFunction <> nil) Then
        Begin
         With Variable do
          if (isConst) and (Value <> nil) Then // if constant...
          Begin
           Arg := getValueFromExpression(Value);
          End Else
          Begin
           Attributes += [vaVolatile]; // optimizer could remove this variable or do any other optimization to it, which could break the user's bytecode, so we're just letting optimizer know, that it mustn't do anything with this variable
           Arg        := 'localvar.'+IntToStr(LongWord(Variable)); // a bit lame solution, but I have no idea how to make it work in a better way
          End;
        End Else
         With Variable do
          if (isConst) Then
           Arg := getValueFromExpression(Value) Else
           Arg := Variable.getAllocationPos;
       End;
      End;

      // @TODO: func call (maybe `$funcname`? eg.`call(:$myfunc)`)

      else
       CompileError(eUnexpected, [Token.Value]);
     End;

     SetLength(ArgList^, Length(ArgList^)+1);
     ArgList^[High(ArgList^)].VType  := vtPChar;
     ArgList^[High(ArgList^)].VPChar := CopyStringToPChar(Arg);

     if (next_t <> _BRACKET1_CL) Then
      eat(_COMMA);
    End;

    if (DirectBytecode) Then
    Begin
     PutOpcode(Name, ArgList^);
    End Else
    Begin
     Node                        := TCFGNode.Create(fCurrentNode, next_pnt(-1));
     Node.Typ                    := cetBytecode;
     Node.Bytecode.OpcodeName    := Name;
     Node.Bytecode.OpcodeArgList := ArgList;

     CFGAddNode(Node);
    End;
   End;
  End;
 Until (Deep = CurrentDeep);
End;
End;
End.
