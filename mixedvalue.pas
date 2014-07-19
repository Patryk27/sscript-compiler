(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$MODESWITCH ADVANCEDRECORDS}
Unit MixedValue;

 Interface
 Uses SysUtils, Variants;

 { EMixedValueException }
 Type EMixedValueException = Class(Exception);

 { TMixedValueKind }
 Type TMixedValueKind = (mvUnknown, mvBool, mvChar, mvInt, mvFloat, mvString);

 Const MixedValueKinds: Array[TMixedValueKind] of String = ('unknown', 'bool', 'char', 'int', 'float', 'string');

 { TMixedValueValue }
 Type TMixedValueValue =
      Record
       vBool  : Boolean;
       vChar  : Char;
       vInt   : Int64;
       vFloat : Double;
       vString: String;
      End;

 { TMixedValue }
 Type TMixedValue =
      Record
       Public
        Kind : TMixedValueKind;
        Value: TMixedValueValue;

       Public
        Function getValue: Variant;

        Function getInt: Int64;
        Function getFloat: Double;
        Function getString: String;

        Function isZero: Boolean;
        Function isNumerical: Boolean;
       End;

 Operator := (const Value: Boolean): TMixedValue;
 Operator := (const Value: Char): TMixedValue;
 Operator := (const Value: Int64): TMixedValue;
 Operator := (const Value: Double): TMixedValue;
 Operator := (const Value: String): TMixedValue;

 Operator + (const A, B: TMixedValue): TMixedValue;
 Operator - (const A, B: TMixedValue): TMixedValue;
 Operator * (const A, B: TMixedValue): TMixedValue;
 Operator / (const A, B: TMixedValue): TMixedValue;
 Operator mod (const A, B: TMixedValue): TMixedValue;

 Operator or (const A, B: TMixedValue): TMixedValue;
 Operator and (const A, B: TMixedValue): TMixedValue;

 Operator < (const A, B: TMixedValue): TMixedValue;
 Operator <= (const A, B: TMixedValue): TMixedValue;
 Operator = (const A, B: TMixedValue): TMixedValue;
 Operator > (const A, B: TMixedValue): TMixedValue;
 Operator >= (const A, B: TMixedValue): TMixedValue;
 Operator <> (const A, B: TMixedValue): TMixedValue;

 Operator - (const A: TMixedValue): TMixedValue;

 Function getMixedValueKindName(const Kind: TMixedValueKind): String;

 Implementation

(* TMixedValue.getValue *)
{
 Returns value of TMixedValue.
}
Function TMixedValue.getValue: Variant;
Begin
 Case Kind of
  mvBool  : Result := Value.vBool;
  mvChar  : Result := ord(Value.vChar);
  mvInt   : Result := Value.vInt;
  mvFloat : Result := Value.vFloat;
  mvString: Result := Value.vString;
 End;
End;

(* TMixedValue.getInt *)
{
 Casts TMixedValue to an int.
 Throws an EMixedValueException if conversion failed.
}
Function TMixedValue.getInt: Int64;
Begin
 Case Kind of
  mvChar : Result := ord(Value.vChar);
  mvInt  : Result := Value.vInt;
  mvFloat: Result := Round(Value.vFloat);

  else
   raise EMixedValueException.CreateFmt('Cannot cast from %s to int.', [MixedValueKinds[Kind]]);
 End;
End;

(* TMixedValue.getFloat *)
{
 Casts TMixedValue to a float.
 Throws an EMixedValueException if conversion failed.
}
Function TMixedValue.getFloat: Double;
Begin
 Case Kind of
  mvChar : Result := ord(Value.vChar);
  mvInt  : Result := Value.vInt;
  mvFloat: Result := Value.vFloat;

  else
   raise EMixedValueException.CreateFmt('Cannot cast from %s to float.', [MixedValueKinds[Kind]]);
 End;
End;

(* TMixedValue.getString *)
{
 Casts TMixedValue to a string.
 Throws an EMixedValueException if conversion failed.
}
Function TMixedValue.getString: String;
Begin
 Case Kind of
  mvChar  : Result := Value.vChar;
  mvString: Result := Value.vString;

  else
   raise EMixedValueException.CreateFmt('Cannot cast from %s to string.', [MixedValueKinds[Kind]]);
 End;
End;

(* TMixedValue.isZero *)
{
 Returns 'true' if MixedValue integer or floating-point value is equal to zero.
 Returns 'false' otherwise.
}
Function TMixedValue.isZero: Boolean;
Begin
 Case Kind of
  mvInt  : Result := (Value.vInt = 0);
  mvFloat: Result := (Value.vFloat = 0);

  else
   Result := False;
 End;
End;

(* TMixedValue.isNumerical *)
{
 Returns 'true' if MixedValue is a char, int or float.
}
Function TMixedValue.isNumerical: Boolean;
Begin
 Result := (Kind in [mvChar, mvInt, mvFloat]);
End;

// -------------------------------------------------------------------------- //
(* SelectLargerKind *)
Function SelectLargerKind(const A, B: TMixedValueKind): TMixedValueKind;
Begin
 // same :: same
 if (A = B) Then
  Exit(A);

 // int/float :: float/int
 if (A in [mvInt, mvFloat]) and (B in [mvInt, mvFloat]) Then
  Exit(mvFloat);

 // return mvUnknown otherwise
 Result := mvUnknown;
End;

// -------------------------------------------------------------------------- //
(* TMixedValue := Boolean *)
Operator := (const Value: Boolean): TMixedValue;
Begin
 Result.Kind        := mvBool;
 Result.Value.vBool := Value;
End;

(* TMixedValue := Char *)
Operator := (const Value: Char): TMixedValue;
Begin
 Result.Kind        := mvChar;
 Result.Value.vChar := Value;
End;

(* TMixedValue := Int64 *)
Operator := (const Value: Int64): TMixedValue;
Begin
 Result.Kind       := mvInt;
 Result.Value.vInt := Value;
End;

(* TMixedValue := Double *)
Operator := (const Value: Double): TMixedValue;
Begin
 Result.Kind         := mvFloat;
 Result.Value.vFloat := Value;
End;

(* TMixedValue := String *)
Operator := (const Value: String): TMixedValue;
Begin
 Result.Kind          := mvString;
 Result.Value.vString := Value;
End;

(* TMixedValue + TMixedValue *)
Operator + (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // int/float + int/float
 if (A.Kind in [mvInt, mvFloat]) and (B.Kind in [mvInt, mvFloat]) Then
 Begin
  Result.Kind := SelectLargerKind(A.Kind, B.Kind);

  if (Result.Kind = mvInt) Then
   Result.Value.vInt := A.getInt + B.getInt Else
   Result.Value.vFloat := A.getFloat + B.getFloat;
 End Else

 // int + char
 if (A.Kind = mvInt) and (B.Kind = mvChar) Then
 Begin
  Result := A.getInt + B.getInt;
 End Else

 // char + int
 if (A.Kind = mvChar) and (B.Kind = mvInt) Then
 Begin
  Result := Char(A.getInt + B.getInt);
 End Else

 // string + string
 if (A.Kind = mvString) and (B.Kind = mvString) Then
 Begin
  Result := A.getString + B.getString;
 End Else

 // string+char | char+string
 if ((A.Kind = mvString) and (B.Kind = mvChar)) or
    ((A.Kind = mvChar) and (B.Kind = mvString)) Then
 Begin
  Result := A.getString + B.getString;
 End;
End;

(* TMixedValue - TMixedValue *)
Operator - (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // int/float - int/float
 if (A.Kind in [mvInt, mvFloat]) and (B.Kind in [mvInt, mvFloat]) Then
 Begin
  Result.Kind := SelectLargerKind(A.Kind, B.Kind);

  if (Result.Kind = mvInt) Then
   Result.Value.vInt := A.getInt - B.getInt Else
   Result.Value.vFloat := A.getFloat - B.getFloat;
 End Else

 // int - char
 if (A.Kind = mvInt) and (B.Kind = mvChar) Then
 Begin
  Result := A.getInt - B.getInt;
 End Else

 // char - int
 if (A.Kind = mvChar) and (B.Kind = mvInt) Then
 Begin
  Result := Char(A.getInt - B.getInt);
 End Else
End;

(* TMixedValue * TMixedValue *)
Operator * (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // int/float * int/float
 if (A.Kind in [mvInt, mvFloat]) and (B.Kind in [mvInt, mvFloat]) Then
 Begin
  Result.Kind := SelectLargerKind(A.Kind, B.Kind);

  if (Result.Kind = mvInt) Then
   Result.Value.vInt := A.getInt * B.getInt Else
   Result.Value.vFloat := A.getFloat * B.getFloat;
 End;
End;

(* TMixedValue / TMixedValue *)
Operator / (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // int/float / int/float
 if (A.Kind in [mvInt, mvFloat]) and (B.Kind in [mvInt, mvFloat]) Then
 Begin
  Result.Kind := SelectLargerKind(A.Kind, B.Kind);

  if (Result.Kind = mvInt) Then
   Result.Value.vInt := A.getInt div B.getInt Else
   Result.Value.vFloat := A.getFloat / B.getFloat;
 End;
End;

(* TMixedValue mod TMixedValue *)
Operator mod (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // int % int
 if (A.Kind = mvInt) and (B.Kind = mvInt) Then
 Begin
  Result := A.Value.vInt mod B.Value.vInt;
 End;
End;

(* TMixedValue or TMixedValue *)
Operator or (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // bool or bool
 if (A.Kind = mvBool) and (B.Kind = mvBool) Then
 Begin
  Result := A.Value.vBool or B.Value.vBool;
 End;
End;

(* TMixedValue and TMixedValue *)
Operator and (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // bool and bool
 if (A.Kind = mvBool) and (B.Kind = mvBool) Then
 Begin
  Result := A.Value.vBool and B.Value.vBool;
 End;
End;

(* TMixedValue < TMixedValue *)
Operator < (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // numerical < numerical
 if (A.isNumerical) and (B.isNumerical) Then
 Begin
  Result := A.getFloat < B.getFloat;
 End;
End;

(* TMixedValue <= TMixedValue *)
Operator <= (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // numerical <= numerical
 if (A.isNumerical) and (B.isNumerical) Then
 Begin
  Result := A.getFloat <= B.getFloat;
 End;
End;

(* TMixedValue = TMixedValue *)
Operator = (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // numerical = numerical
 if (A.isNumerical) and (B.isNumerical) Then
 Begin
  Result := A.getFloat = B.getFloat;
 End;
End;

(* TMixedValue > TMixedValue *)
Operator > (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // numerical > numerical
 if (A.isNumerical) and (B.isNumerical) Then
 Begin
  Result := A.getFloat > B.getFloat;
 End;
End;

(* TMixedValue >= TMixedValue *)
Operator >= (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // numerical >= numerical
 if (A.isNumerical) and (B.isNumerical) Then
 Begin
  Result := A.getFloat >= B.getFloat;
 End;
End;

(* TMixedValue <> TMixedValue *)
Operator <> (const A, B: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 // numerical <> numerical
 if (A.isNumerical) and (B.isNumerical) Then
 Begin
  Result := A.getFloat <> B.getFloat;
 End;
End;

(* - TMixedValue *)
Operator - (const A: TMixedValue): TMixedValue;
Begin
 Result.Kind := mvUnknown;

 Case A.Kind of
  mvInt  : Result := -A.Value.vInt;
  mvFloat: Result := -A.Value.vFloat;
 End;
End;

(* getMixedValueKindName *)
Function getMixedValueKindName(const Kind: TMixedValueKind): String;
Begin
 Result := MixedValueKinds[Kind];
End;
End.
