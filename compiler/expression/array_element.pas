Procedure ParseArrayElement;
Var ArrayType, TmpType: PMType;
    Typ               : TMType;
    IndexCount        : Integer;
    TmpExpr           : PMExpression;
Begin
 ArrayType := Parse(Left, 1);

 if (ArrayType = nil) Then
  Exit;

 Typ := ArrayType^;

 {
  @Note:
  There's no need to checking whether `TmpType` is an array or not, as if it's not, `eInvalidArraySubscript` will be shown, because
  its `ArrayDimCount` will be equal zero and it'll fail in `repeat..until` below.
 }

 { push indexes onto the stack }
 TmpExpr    := Expr;
 IndexCount := 0;

 Repeat
  TmpType := Parse(TmpExpr^.Right);
  With Compiler do // array subscript must be an integer value
   if (not isTypeInt(TmpType)) or (Typ.ArrayDimCount = 0) Then
   Begin
    Error(TmpExpr^.Right^.Token, eInvalidArraySubscript, [getTypeDeclaration(Typ), getTypeDeclaration(TmpType)]);
    Exit;
   End;

  TmpExpr := TmpExpr^.Left;

  Dec(Typ.ArrayDimCount);
  Inc(IndexCount);
 Until not (TmpExpr^.Typ in [mtArrayElement]);

 { type change }
 if (Typ.ArrayDimCount = 0) Then
  Typ.RegPrefix := Compiler.getTypePrefix(Typ.ArrayBase);

 if (Compiler.isTypeString(Typ.ArrayBase) and (Typ.ArrayDimCount = 1)) Then // `string`
  Typ := TYPE_STRING;

 if (Compiler.isTypeString(Typ.ArrayBase) and (Typ.ArrayDimCount = 0)) Then // `string`
  Typ := TYPE_CHAR;

 { get value }
 Compiler.PutOpcode(o_arget, ['e'+Compiler.getTypePrefix(ArrayType)+'1', IndexCount, 'e'+Typ.RegPrefix+'1']);
 Dec(PushedValues, IndexCount);

 { set result value }
 Typ.Name := Compiler.getTypeDeclaration(Typ);

 New(Result);
 Result^ := Typ;
End;
