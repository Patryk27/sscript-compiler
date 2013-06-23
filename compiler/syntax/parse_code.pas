(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_CODE;

 Interface
 Uses SysUtils;

 Procedure Parse(Compiler: Pointer; const DirectBytecode: Boolean=False);

 Implementation
Uses Compile1, ExpressionCompiler, Messages, Tokens, Opcodes, cfgraph, symdef;

(* Parse *)
Procedure Parse(Compiler: Pointer; const DirectBytecode: Boolean=False);
Var Opcode: PMOpcode;

    Deep, IdentID, IdentNamespace: Integer;
    Token                        : TToken_P;

    isIdentLocal: Boolean;

    Name: PChar;

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
    PutLabel(getCurrentFunction.MangledName + read_ident, not DirectBytecode);
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
      _STRING                  : Arg := '"'+Token.Value+'"';
      _CHAR                    : Arg := ''''+Token.Value+'''';
      _INT, _FLOAT, _IDENTIFIER: Arg := Token.Value;

      _HASH { # }: Arg += '#'+IntToStr(read_int);

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
        Arg += getCurrentFunction.MangledName;
       End;
       Arg += read_ident;
      End;

      _AMPERSAND { & }:
      Begin
       if (DirectBytecode) Then
        CompileError(eUnexpected, ['&']);

       Arg := getCurrentFunction.MangledName+read_ident;
      End;

      _PERCENT { % }:
      Begin
       Arg := read_ident;

       IdentID      := findLocalVariable(Arg);
       isIdentLocal := (IdentID <> -1);

       if (IdentID = -1) Then
        findGlobalVariableCandidate(Arg, SelectedNamespaces, IdentID, IdentNamespace);

       if (IdentID = -1) Then { var not found }
       Begin
        CompileError(eUnknownVariable, [Arg]);
        Arg := '[0]';
       End Else { var found }
       Begin
        if (isIdentLocal) Then
        Begin
         With getCurrentFunction.SymbolList[IdentID] do
          With mVariable do
           if (isConst) and (Value <> nil) Then // if constant...
            Arg := getValueFromExpression(Value) Else
            Begin
             Attributes += [vaVolatile]; // optimizer could remove this variable, what we don't want.
             Arg        := 'localvar.'+IntToStr(LongWord(mVariable)); // a bit lame solution, but I have no idea how to make it work in better way
            End;
        End Else
         With NamespaceList[IdentNamespace].SymbolList[IdentID].mVariable do
          if (isConst) Then
           Arg := getValueFromExpression(Value) Else
           CompileError(eInternalError, ['Global variables have not been implemented yet.']);
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
