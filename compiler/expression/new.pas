Procedure ParseNEW;
Var BaseType, TmpType, Typ: TType;
Begin
 { get the array's primary (base) type (int, char, (...) }
 BaseType := getType(Left^.Value);

 if (BaseType = nil) Then // suitable error message has been displayed when creating the tree, so we don't need to display anything else here; thus, just exit this function
  Exit;

 Typ := BaseType.Clone;

 While (Typ.ArrayBase.isArray(False)) Do
  Typ.ArrayBase := Typ.ArrayBase.ArrayBase;

 Typ.ArrayDimCount := 0;
 Typ.ArrayBase     := BaseType;

 With Compiler do // type-check
  if (BaseType.isArray(False)) Then
  Begin
   Error(eWrongType, [BaseType.asString, 'not array-derived type']);
   Exit;
  End;

 { just some checks... }
 if (Right^.Typ <> mtArrayElement) Then
  Error(eInternalError, ['`Right^.Typ` should be a `mtArrayElement`, but for some reason it''s not...']);

 if (type_equal(BaseType, TYPE_ANY)) Then
  Error(eInternalError, ['Cannot create an `any`-typed array!']);

 { make an array from that base type }
 While (Right^.Typ = mtArrayElement) Do
 Begin
  if (Right^.Right^.Value <> null) Then
   TmpType := Parse(Right^.Right) Else
   TmpType := Parse(Right^.Left);

  if (TmpType = nil) Then
  Begin
   DevLog('Info: ParseNEW() -> TmpType = nil; leaving...');
   Exit;
  End;

  With Compiler do // array subscript must be an integer value
   if (not TmpType.isInt) Then
    Error(eInvalidArraySubscript, [BaseType.asString, TmpType.asString]);

  Right := Right^.Left;
  Inc(Typ.ArrayDimCount);
 End;

 { prepare type }
 Typ.RegPrefix := 'r';
 Dec(PushedValues, Typ.ArrayDimCount);

 { put opcode }
 Compiler.PutOpcode(o_arcrt, ['er1', BaseType.InternalID, Typ.ArrayDimCount]);

 { return type }
 if (Typ.isString) Then
  Inc(Typ.ArrayDimCount);

 Result := Typ;
End;
