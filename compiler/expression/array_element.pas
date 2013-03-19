Procedure ParseArrayElement;
Var TmpType, Typ, ArrayType: TType;
    IndexCount             : Integer;
    TmpExpr                : PMExpression;
Begin
 { find array origin (a variable, cast (...)) }
 TmpExpr := Expr;
 While (TmpExpr^.Typ = mtArrayElement) Do
  TmpExpr := TmpExpr^.Left;

 ArrayType := Parse(TmpExpr, 1);
 Typ       := ArrayType.Clone;

 { push indexes onto the stack }
 TmpExpr    := Expr;
 IndexCount := 0;

 Repeat
  TmpType := Parse(TmpExpr^.Right);
  With Compiler do // array subscript must be an integer value
   if (not TmpType.isInt) or (Typ.ArrayDimCount = 0) Then
   Begin
    Error(TmpExpr^.Right^.Token, eInvalidArraySubscript, [Typ.asString, TmpType.asString]);
    Exit;
   End;

  TmpExpr := TmpExpr^.Left;

  Dec(Typ.ArrayDimCount);
  Inc(IndexCount);
 Until not (TmpExpr^.Typ = mtArrayElement);

 { type change }
 if (Typ.ArrayDimCount = 0) Then
  Typ.RegPrefix := Typ.ArrayBase.RegPrefix Else
  Begin // invalid conversion (except strings)
   if not ((Typ.isString) and (Typ.ArrayDimCount = 1)) Then
   Begin
    Error(eInvalidConversion, [Typ.asString, Typ.ArrayBase.asString]);
    Exit;
   End;
  End;

 if (Typ.ArrayBase.isString and (Typ.ArrayDimCount = 1)) Then // `string`
  Typ := TYPE_STRING;

 if (Typ.ArrayBase.isString and (Typ.ArrayDimCount = 0)) Then // `string`
  Typ := TYPE_CHAR;

 { get value }
 Compiler.PutOpcode(o_arget, ['e'+ArrayType.RegPrefix+'1', IndexCount, 'e'+Typ.RegPrefix+'1']);
 Dec(PushedValues, IndexCount);

 { set result value }
 Result := Typ;
End;
