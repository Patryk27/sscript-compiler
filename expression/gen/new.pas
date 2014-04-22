Procedure ParseNEW;
Var BaseType, TmpType, Typ: TType;
    Reg                   : String;
Begin
 // get the array primary (base) type (int, char...)
 BaseType := getType(Left^.Value);

 if (BaseType = nil) Then // error message has been displayed when creating the tree so we don't need to display anything else here; so just exit this function
  Exit;

 Typ := BaseType.Clone;

 While (Typ.ArrayBase.isArray(False)) Do
  Typ.ArrayBase := Typ.ArrayBase.ArrayBase;

 Typ.ArrayDimCount := 0;
 Typ.ArrayBase     := BaseType;

 // type-check
 if (BaseType.isArray(False)) Then
 Begin
  Error(eWrongType, [BaseType.asString, 'not array-derived type']);
  Exit;
 End;

 // some more checks
 if (Right^.Typ <> mtArrayElement) Then
  Error(eInternalError, ['`Right^.Typ` should be a `mtArrayElement`, but for some reason it''s not...']);

 if (type_equal(BaseType, TYPE_ANY)) Then
  Error(eInternalError, ['Cannot create an `any`-typed array!']);

 // make an array from that base type
 While (Right <> nil) Do
 Begin
  // "new type[size][]"
  if (Right^.Left = nil) Then
  Begin
   Compiler.PutOpcode(o_push, ['0']);
  End Else

  // "new type[size]"
  Begin
   TmpType := Parse(Right^.Left);

   if (TmpType = nil) Then
   Begin
    DevLog(dvInfo, 'TmpType = nil; leaving...');
    Exit;
   End;

   // array subscript must be an integer value
   if (not TmpType.isInt) Then
    Error(eInvalidArraySubscript, [BaseType.asString, TmpType.asString]);
  End;

  Right := Right^.Right;
  Inc(Typ.ArrayDimCount);
 End;

 // prepare type
 Typ.RegPrefix := 'r';
 Dec(PushedValues, Typ.ArrayDimCount);

 // put opcode
 if (FinalRegID > 0) and (FinalRegChar <> #0) Then
 Begin
  Reg          := 'e'+FinalRegChar+IntToStr(FinalRegID);
  FinalRegDone := True;
 End Else
 Begin
  Reg := 'er1';
 End;

 Compiler.PutOpcode(o_arcrt, [Reg, BaseType.InternalID, Typ.ArrayDimCount]);

 { return type }
 if (Typ.isString) Then
  Inc(Typ.ArrayDimCount);

 Result := Typ;
End;
