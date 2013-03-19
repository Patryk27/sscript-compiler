(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Parse_FUNCTION;

 Interface
 Uses MTypes, Tokens, Variants, TypInfo;

 Procedure Parse(Compiler: Pointer);

 Implementation
Uses Compile1, Messages, Opcodes, ExpressionCompiler, symdef, SysUtils, CompilerUnit;

(* Parse *)
{
 Parses a whole function or a function declaration.
}
Procedure Parse(Compiler: Pointer);
Type TVarRecArray = Array of TVarRec;
     PVarRecArray = ^TVarRecArray;
Var CList: TMConstructionList;
    Func : TFunction; // our new function
    c_ID : Integer;

    VarsOnStack: LongWord = 0;

  { ParseConstruction }
  Procedure ParseConstruction(ID: Integer);

    // ParseUntil
    Procedure ParseUntil(M: TMConstructionType);
    Begin
     Inc(c_ID);
     While (CList[c_ID].Typ <> M) Do // parse loop
     Begin
      ParseConstruction(c_ID);
      Inc(c_ID);
    End;
    End;

    // RemoveRedundantPush
    Procedure RemoveRedundantPush;
    Begin
     With TCompiler(Compiler) do
      if (OpcodeList.Last^.Opcode = o_push) Then
       With OpcodeList.Last^ do
        OpcodeList.Delete(OpcodeList.Count-1);
    End;

  // ParseConstruction
  Var Str1, Str2, Str3, Str4: String;
      EType                 : TType;
      Item                  : PMOpcode;
  Begin
  With TCompiler(Compiler) do
  Begin
   if (ID > High(CList)) Then
    Exit;
   With CList[ID] do
   Begin
    Case Typ of
  (* ctJump *)
     ctJump:
     Begin
      PutOpcode(o_jmp, [PChar(Values[0])]);
     End;

  (* ctLabel *)
     ctLabel:
     Begin
      if (findLabel(PChar(Values[0])) <> -1) Then
       CompileError(PToken_P(Values[1])^, eRedeclaration, [PChar(Values[0])]);

      New(Item);
      With Item^ do
      Begin
       Name    := PChar(Values[0]);
       isLabel := True;
      End;
      OpcodeList.Add(Item);
     End;

  (* ctExpression *)
     ctExpression:
     Begin
      ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
      RemoveRedundantPush;
     End;

  (* ctReturn *)
     ctReturn:
     Begin
      EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);

      if (not EType.CanBeAssignedTo(Func.Return)) Then // type check
       CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, Func.Return.asString]);

      if (PMExpression(Values[0])^.ResultOnStack) Then // result must be in the `e_1` register, not on the stack
       PutOpcode(o_pop, ['e'+Func.Return.RegPrefix+'1']);

      if (not Func.isNaked) Then
       PutOpcode(o_jmp, [':'+Func.MangledName+'_end']) Else
       PutOpcode(o_ret);
     End;

  (* ctVoidReturn *)
     ctVoidReturn:
     Begin
      if (not Func.Return.isVoid) Then // type check
       CompileError(PToken_P(Values[0])^, eWrongType, ['void', Func.Return.asString]);
      PutOpcode(o_ret);
     End;

  (* ctInlineBytecode *)
     ctInlineBytecode: PutOpcode(PChar(Values[0]), PVarRecArray(Values[1])^, Values[2]);

  (* ctFOR *)
     ctFOR:
     Begin
      Str1 := PChar(Values[2]);
      Str2 := Str1+'condition';
      Str3 := Str1+'end';

      { condition }
      PutLabel(Str2);
      EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
      if (not EType.isBool) Then
       CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'bool']);

      { condition check }
      PutOpcode(o_pop, ['if']);
      PutOpcode(o_fjmp, [':'+Str3]);

      ParseUntil(ctFOR_end);

      { step }
      ExpressionCompiler.CompileConstruction(Compiler, Values[1]);
      RemoveRedundantPush;

      PutOpcode(o_jmp, [':'+Str2]);

      PutLabel(Str3); { end }
     End;

  (* ctIF *)
     ctIF:
     Begin
      Str1 := PChar(Values[1]);
      Str2 := Str1+'true';
      Str3 := Str1+'false';
      Str4 := Str1+'end';

      { condition }
      EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
      if (not EType.isBool) Then
       CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'bool']);

      { jump }
      PutOpcode(o_pop, ['if']);
      PutOpcode(o_tjmp, [':'+Str2]);
      PutOpcode(o_jmp, [':'+Str3]); // or 'o_fjmp' - it doesn't matter, but `o_jmp` is a slightly faster (as it doesn't check the `if` register's value)

      { on true }
      PutLabel(Str2);
      ParseUntil(ctIF_end);
      PutOpcode(o_jmp, [':'+Str4]);

      PutLabel(Str3);

      { on false }
      if (CList[c_ID+1].Typ = ctIF_else) Then // compile 'else'
      Begin
       Inc(c_ID); // skip `ctIF_else`
       ParseUntil(ctIF_end);
      End;

      PutLabel(Str4);
     End;

  (* ctWHILE *)
     ctWHILE:
     Begin
      Str1 := PChar(Values[1]);
      Str2 := Str1+'condition';
      Str3 := Str1+'end';

      { condition (loop begin) }
      PutLabel(Str2);
      EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
      if (not EType.isBool) Then
       CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'bool']);

      { condition check }
      PutOpcode(o_pop, ['if']);
      PutOpcode(o_fjmp, [':'+Str3]);

      { loop body }
      ParseUntil(ctWHILE_end);

      PutOpcode(o_jmp, [':'+Str2]);

      { loop end }
      PutLabel(Str3);
     End;

  (* ct_DO_WHILE *)
     ct_DO_WHILE:
     Begin
      Str1 := PChar(Values[0]);
      Str2 := Str1+'begin';
      Str3 := Str1+'end';

      { loop begin }
      PutLabel(Str2);

      { parse loop }
      ParseUntil(ct_DO_WHILE_end);

      { condition }
      With CList[c_ID] do
      Begin
       EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
       if (not EType.isBool) Then
        CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'bool']);
      End;

      { condition check }
      PutOpcode(o_pop, ['if']);
      PutOpcode(o_tjmp, [':'+Str2]);

      { loop end }
      PutLabel(Str3);
     End;

  (* ctDELETE *)
     ctDELETE:
     Begin
      EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
      if (not EType.isObject) Then
       CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'object']);

      PutOpcode(o_pop, ['er1']);
      PutOpcode(o_objfree, ['er1']);
     End;

  (* ctTHROW *)
     ctTHROW:
     Begin
      EType := ExpressionCompiler.CompileConstruction(Compiler, Values[0]);
      if (not EType.isString) Then
      Begin
       CompileError(PMExpression(Values[0])^.Token, eWrongType, [EType.asString, 'string']);
       Exit;
      End;

      PutOpcode(o_pop, ['es1']);
      PutOpcode(o_push, ['es1']);
      PutOpcode(o_icall, ['"vm.throw"']);
     End;

  (* ctTRY *)
     ctTRY:
     Begin
      { save current exception handler and set the new one }
      PutOpcode(o_icall, ['"vm.save_exception_state"']);
      PutOpcode(o_push, ['@'+PChar(Values[0])]);
      PutOpcode(o_icall, ['"vm.set_exception_handler"']);

      { parse `try` block }
      ParseUntil(ctCatch);
      PutOpcode(o_jmp, [':'+PChar(Values[0])+'_end']);

      { parse `catch` block }
      PutLabel(PChar(Values[0]));
      PutOpcode(o_icall, ['"vm.restore_exception_state"']); // restore previous exception state
      PutOpcode(o_icall, ['"vm.get_last_exception"']); // get exception

      ParseUntil(ctCATCH_END);
      PutOpcode(o_sub, ['stp', 1]); // remove the exception message variable holder
      PutLabel(PChar(Values[0])+'_end');
     End;

     else
      CompileError(Token, eInternalError, ['Unexpected construction: `'+GetEnumName(TypeInfo(TMConstructionType), ord(Typ))+'`']);
    End;
   End;
  End;
  End;

  // NewConst (used to create internal constants, if enabled)
  Procedure NewConst(const Name: String; Typ: TType; Value: PMExpression);
  Var Variable: TVariable;
  Begin
   With TCompiler(Compiler) do
   Begin
    Variable           := TVariable.Create;
    Variable.Name      := Name;
    Variable.Typ       := Typ;
    Variable.Value     := Value;
    Variable.mCompiler := Compiler;
    Variable.DeclToken := Parser.next_pnt;

    Variable.Attributes += [vaConst, vaDontAllocate];

    if (findLocalVariable(Name) = -1) Then // don't duplicate
    Begin
     With getCurrentFunction do
     Begin
      SetLength(VariableList, Length(VariableList)+1);
      VariableList[High(VariableList)] := Variable;
     End;
    End;
   End;
  End;

// main function block
Var I, SavedRegs : Integer;
    Token        : TToken_P;
    Namespaces   : Array of Integer;
    isDeclaration: Boolean = False;
Begin
 Func := TFunction.Create;

 Func.mCompiler := Compiler;

With TCompiler(Compiler), Parser do
Begin
 // make a backup of current namespaces (as we'll restore them when we'll finish compiling this namespace)
 SetLength(Namespaces, Length(SelectedNamespaces));
 For I := Low(Namespaces) To High(Namespaces) Do
  Namespaces[I] := SelectedNamespaces[I];

 Func.DeclToken     := next_pnt(-1); // _FUNCTION
 Func.NamespaceName := getCurrentNamespace.Name;

 { read function return type }
 eat(_LOWER); // <
 Func.Return := read_type; // [type]
 eat(_GREATER); // >

 { read function name and check for duplicates (redeclaration) }
 Func.Name       := read_ident; // [identifier]
 Func.ModuleName := ModuleName;
 Func.Visibility := getVisibility;

 RedeclarationCheck(Func.Name); // check for redeclaration

 Case Func.Name of
  '': CompileError(eRedeclaration, [Func.Name]); // cannot redeclare internal function
 End;

 { make parameter list }
 eat(_BRACKET1_OP); // (

 SetLength(Func.ParamList, 0);
 While (next_t <> _BRACKET1_CL) Do
 Begin
  if (Length(Func.ParamList) > 0) Then
   eat(_COMMA); // parameters are separated by comma (except the very first parameter)

  With Func do
  Begin
   SetLength(ParamList, Length(ParamList)+1); // resize the param array
   With ParamList[High(ParamList)] do // read parameter
   Begin
    Typ := read_type;

    if (next_t in [_COMMA, _BRACKET1_CL]) Then
    Begin
     if (not isDeclaration) and (Length(ParamList) <> 1) Then
      CompileError(next, eExpectedIdentifier, [next.Display]);

     isDeclaration := True;
     Continue;
    End;

    Name := read_ident;

    if (Typ.isVoid) Then // error: void-typed param
     CompileError(eVoidParam, [Name]);

    { check duplicates }
    For I := Low(ParamList) To High(ParamList)-1 Do
     if (ParamList[I].Name = Name) Then
     Begin
      CompileError(eRedeclaration, [Name]); // error: parameter has been redeclared
      Break;
     End;
   End;
  End;
 End;
 eat(_BRACKET1_CL); // read remaining parenthesis

 { read special function attributes }
 Exclude(Func.Attributes, faNaked);

 While (true) Do
 Begin
  Token := read;
  Case Token.Token of
   _NAKED    : Include(Func.Attributes, faNaked);
   _IN       : Func.LibraryFile := read_string;
   _NAMESPACE: Func.NamespaceName := read_string;
   _NAME     : Func.MangledName := read_string;
   else Break;
  End;
 End;
 Dec(TokenPos);

 if (Func.LibraryFile <> '') or ((TCompiler(Compiler).Parent.CompileMode = cmLibrary) and (Pointer(TCompiler(Compiler).Parent) = Compiler)) Then
  Func.MangledName := CreateFunctionMangledName(Func, True) Else
  Func.MangledName := CreateFunctionMangledName(Func, False); // create function mangled name

 SetLength(Func.ConstructionList, 0); // there are no constructions in this compiled function so far

 { add new function into the list }
 With getCurrentNamespace do
 Begin
  SetLength(SymbolList, Length(SymbolList)+1);
  SymbolList[High(SymbolList)] := TGlobalSymbol.Create;

  With SymbolList[High(SymbolList)] do
  Begin
   Typ       := gsFunction;
   mFunction := Func;

   mVariable       := TVariable.Create;
   mVariable.Name  := Func.Name;
   mVariable.Typ   := NewTypeFromFunction(Func);
   mVariable.Value := MakeIntExpression('@'+Func.MangledName);

   mVariable.Attributes += [vaConst, vaDontAllocate];
  End;
 End;

 if (Func.LibraryFile <> '') Then // if file is imported from library, we don't create any bytecode
 Begin
  semicolon;
  Exit;
 End;

 if (isDeclaration) and not (next_t = _SEMICOLON) Then
  CompileError(next, eExpected, [';', next.Display]);

 { add parameters }
 With Func do
  For I := Low(ParamList) To High(ParamList) Do
   __variable_create(ParamList[I].Name, ParamList[I].Typ, -I-2, [vaFuncParam, vaDontAllocate]);

 { add special constants (if `-Sconst` enabled) }
 if (getBoolOption(opt_internal_const)) Then
 Begin
  NewConst('__self', TYPE_STRING, MakeStringExpression(Func.Name));
 End;

 { new label (function's begin) }
 PutLabel(Func.MangledName)^.isFunctionBeginLabel := True;

 { new scope (because we're in function now) }
 NewScope(sFunction);

 { ====== parse function's body ====== }
 ParseCodeBlock;

 (* now, we have a full construction list used in this function; so - let's generate bytecode! :) *)
 CList := getCurrentFunction.ConstructionList;

 { allocate local stack variables }
 VarsOnStack := 0;

 if (not Func.isNaked) Then
 Begin
  With getCurrentFunction do
   For I := Low(VariableList) To High(VariableList) Do
    if (VariableList[I].MemPos <= 0) and (not VariableList[I].DontAllocate) and (not VariableList[I].isConst) Then
     Inc(VarsOnStack); // next variable to allocate

  PutOpcode(o_add, ['stp', VarsOnStack+1]); // `+1`, because `stack[stp]` is the caller's IP (instruction pointer)
 End;

 { if register is occupied by a variable, we need to at first save this register's value (and restore it at the end of the function) }
 SavedRegs := 0;
 With getCurrentFunction do
 Begin
  For I := Low(VariableList) To High(VariableList) Do
   With VariableList[I] do
    if (MemPos > 0) and (not isConst) Then
    Begin
     PutOpcode(o_push, ['e'+Typ.RegPrefix+IntToStr(MemPos)]);
     Inc(SavedRegs);
    End;

  For I := Low(VariableList) To High(VariableList) Do // move "back" variables
   With VariableList[I] do
    if (MemPos <= 0) and (not isConst) Then
    Begin
     MemPos -= SavedRegs;

     if (isFuncParam) Then
      MemPos -= VarsOnStack;
    End;
 End;

 { new label (main function's body; nothing should jump here, it's just facilitation for optimizer) }
 PutLabel(Func.MangledName+'_body');

 { parse constructions }
 c_ID := 0;
 Repeat
  ParseConstruction(c_ID);
  Inc(c_ID);
 Until (c_ID > High(CList));

 { function end code }
 PutLabel(Func.MangledName+'_end');

 With getCurrentFunction do
 Begin
  For I := High(VariableList) Downto Low(VariableList) Do
   With VariableList[I] do
    if (MemPos > 0) and (not isConst) Then
     PutOpcode(o_pop, ['e'+Typ.RegPrefix+IntToStr(MemPos)]);
 End;

 if (not Func.isNaked) Then
  PutOpcode(o_sub, ['stp', VarsOnStack+1]);

 PutOpcode(o_ret);
 // </>

 RemoveScope; // ... and - as we finished compiling this function - remove scope

 SetLength(SelectedNamespaces, Length(Namespaces));
 For I := Low(Namespaces) To High(Namespaces) Do
  SelectedNamespaces[I] := Namespaces[I];
End;
End;
End.
