(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
{$H+}
Unit Stream;

 Interface
 Uses SysUtils, Classes;

 { EStreamException }
 Type EStreamException = Class(Exception);

 { TStream }
 Type TStream =
      Class (Classes.TMemoryStream)
       Private
        ConvertToBE: Boolean;

       Public
        Constructor Create(const fConvertToBigEndian: Boolean=True);

        Procedure EnableConverting;
        Procedure DisableConverting;

        // `write` functions
        Procedure write_uint8(const V: uint8);
        Procedure write_uint16(const V: uint16);
        Procedure write_uint32(const V: uint32);
        Procedure write_uint64(const V: uint64);
        Procedure write_int8(const V: int8);
        Procedure write_int16(const V: int16);
        Procedure write_int32(const V: int32);
        Procedure write_int64(const V: int64);
        Procedure write_float(const V: Extended);
        Procedure write_string(const V: String);
        Procedure write_nullstring(const V: String);

        // `read` functions
        Function read_uint8: uint8;
        Function read_uint16: uint16;
        Function read_uint32: uint32;
        Function read_uint64: uint64;
        Function read_int8: int8;
        Function read_int16: int16;
        Function read_int32: int32;
        Function read_int64: int64;
        Function read_float: Extended;
        Function read_string: String;

        // other functions
        Function Can: Boolean;
        Function getData: Pointer;
        Function getPointer: Pointer;
       End;

 Implementation

(* TStream.Create *)
Constructor TStream.Create(const fConvertToBigEndian: Boolean);
Begin
 ConvertToBE := fConvertToBigEndian;
End;

(* TStream.EnableConverting *)
Procedure TStream.EnableConverting;
Begin
 ConvertToBE := True;
End;

(* TStream.DisableConverting *)
Procedure TStream.DisableConverting;
Begin
 ConvertToBE := False;
End;

(* TStream.write_uint8 *)
Procedure TStream.write_uint8(const V: uint8);
Begin
 Write(V, sizeof(V));
End;

(* TStream.write_uint16 *)
Procedure TStream.write_uint16(const V: uint16);
Begin
 if (ConvertToBE) Then
  Write(NtoBE(V), sizeof(V)) Else
  Write(V, sizeof(V));
End;

(* TStream.write_uint32 *)
Procedure TStream.write_uint32(const V: uint32);
Begin
 if (ConvertToBE) Then
  Write(NtoBE(V), sizeof(V)) Else
  Write(V, sizeof(V));
End;

(* TStream.write_uint64 *)
Procedure TStream.write_uint64(const V: uint64);
Begin
 if (ConvertToBE) Then
  Write(NtoBE(V), sizeof(V)) Else
  Write(V, sizeof(V));
End;

(* TStream.write_int8 *)
Procedure TStream.write_int8(const V: int8);
Begin
 Write(V, sizeof(V));
End;

(* TStream.write_int16 *)
Procedure TStream.write_int16(const V: int16);
Begin
 if (ConvertToBE) Then
  Write(NtoBE(V), sizeof(V)) Else
  Write(V, sizeof(V));
End;

(* TStream.write_int32 *)
Procedure TStream.write_int32(const V: int32);
Begin
 if (ConvertToBE) Then
  Write(NtoBE(V), sizeof(V)) Else
  Write(V, sizeof(V));
End;

(* TStream.write_int64 *)
Procedure TStream.write_int64(const V: int64);
Begin
 if (ConvertToBE) Then
  Write(NtoBE(V), sizeof(V)) Else
  Write(V, sizeof(V));
End;

(* TStream.write_float *)
Procedure TStream.write_float(const V: Extended);
Begin
 Write(V, sizeof(V));
End;

(* TStream.write_string *)
Procedure TStream.write_string(const V: String);
Var Ch: Char;
Begin
 write_uint16(Length(V));

 For Ch in V Do
  write_uint8(ord(Ch));
End;

(* TStream.write_nullstring *)
Procedure TStream.write_nullstring(const V: String);
Var Ch: Char;
Begin
 For Ch in V Do
  write_uint8(ord(Ch));

 write_uint8(0);
End;

(* TStream.read_uint8 *)
Function TStream.read_uint8: uint8;
Begin
 Read(Result, sizeof(Result));
End;

(* TStream.read_uint16 *)
Function TStream.read_uint16: uint16;
Begin
 Read(Result, sizeof(Result));

 if (ConvertToBE) Then
  Result := BEtoN(Result);
End;

(* TStream.read_uint32 *)
Function TStream.read_uint32: uint32;
Begin
 Read(Result, sizeof(Result));

 if (ConvertToBE) Then
  Result := BEtoN(Result);
End;

(* TStream.read_uint64 *)
Function TStream.read_uint64: uint64;
Begin
 Read(Result, sizeof(Result));

 if (ConvertToBE) Then
  Result := BEtoN(Result);
End;

(* TStream.read_int8 *)
Function TStream.read_int8: int8;
Begin
 Read(Result, sizeof(Result));
End;

(* TStream.read_int16 *)
Function TStream.read_int16: int16;
Begin
 Read(Result, sizeof(Result));

 if (ConvertToBE) Then
  Result := BEtoN(Result);
End;

(* TStream.read_int32 *)
Function TStream.read_int32: int32;
Begin
 Read(Result, sizeof(Result));

 if (ConvertToBE) Then
  Result := BEtoN(Result);
End;

(* TStream.read_int64 *)
Function TStream.read_int64: int64;
Begin
 Read(Result, sizeof(Result));

 if (ConvertToBE) Then
  Result := BEtoN(Result);
End;

(* TStream.read_float *)
Function TStream.read_float: Extended;
Begin
 Read(Result, sizeof(Result));
End;

(* TStream.read_string *)
Function TStream.read_string: String;
Var Len: uint16;
Begin
 Result := '';
 Len    := read_uint16;

 For Len := 1 To Len Do
  Result += chr(read_uint8);
End;

(* TStream.Can *)
Function TStream.Can: Boolean;
Begin
 Result := (Position < Size);
End;

(* TStream.getData *)
Function TStream.getData: Pointer;
Begin
 Result := Memory;
End;

(* TStream.getPointer *)
Function TStream.getPointer: Pointer;
Begin
 Result := Memory + Position;
End;
End.
