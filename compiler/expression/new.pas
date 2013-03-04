Procedure ParseNEW;
Var BaseType, TmpType: PMType;
    Typ              : TMType;
    isStringBased    : Boolean;
Begin
 { get the array's primary (base) type (int, char, (...) }
 BaseType := getType(Left^.Value);

 if (BaseType = nil) Then // suitable error message has been displayed when creating the tree, so we don't need to display anything else here; thus, just exit this function
  Exit;

 Typ           := BaseType^;
 isStringBased := (Typ = TYPE_STRING);
 Typ.ArrayBase := BaseType;

 With Compiler do // type-check
  if (isTypeArray(BaseType)) and (not isTypeString(BaseType)) Then
  Begin
   Error(eWrongType, [getTypeDeclaration(BaseType), 'any not array-derived type']);
   Exit;
  End;

 if (isStringBased) Then
  Dec(Typ.ArrayDimCount);

 { just some checks... }
 if (Right^.Typ <> mtArrayElement) Then
  Error(eInternalError, ['`Right^.Typ` should be a `mtArrayElement`, but for some reason it''s not...']);

 if (BaseType^ = TYPE_ANY) Then
  Error(eInternalError, ['Cannot create an `any`-typed array!']);

 { make an array from that base type }
 While (Right^.Typ = mtArrayElement) Do
 Begin
  if (Right^.Left^.Right^.Typ { it's a bit abstract even for me :P } <> mtNothing) Then
   TmpType := Parse(Right^.Left) Else
   TmpType := Parse(Right^.Right);

  With Compiler do // array subscript must be an integer value
   if (not isTypeInt(TmpType)) Then
    Error(eInvalidArraySubscript, [getTypeDeclaration(BaseType), getTypeDeclaration(TmpType)]);

  Right := Right^.Left;
  Inc(Typ.ArrayDimCount);
 End;

 { prepare type }
 Typ.Name      := Compiler.getTypeDeclaration(Typ, False);
 Typ.RegPrefix := 'r';
 Dec(PushedValues, Typ.ArrayDimCount);

 { put opcode }
 Compiler.PutOpcode(o_arcrt, ['er1', BaseType^.InternalID, Typ.ArrayDimCount]);

 { return type }
 if (isStringBased) Then
  Inc(Typ.ArrayDimCount);

 New(Result);
 Result^ := Typ;
End;
