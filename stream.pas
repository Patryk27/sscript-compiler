(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Stream;

 Interface
 Uses SysUtils, Classes;

 { TStream }
 Type TStream =
      Class (Classes.TMemoryStream)
       Public { methods }
       // `write` functions
        Procedure write_byte(const V: Byte);
        Procedure write_word(const V: Word);
        Procedure write_integer(const V: Integer);
        Procedure write_longword(const V: Longword);
        Procedure write_int64(const V: Int64);
        Procedure write_float(const V: Extended);
        Procedure write_string(const V: String);

       // `read` functions
        Function read_byte: Byte;
        Function read_word: Word;
        Function read_integer: Integer;
        Function read_longword: Longword;
        Function read_int64: Int64;
        Function read_float: Extended;
        Function read_string: String;

       // other functions
        Function Can: Boolean;
       End;

 Implementation

(* TStream.write_byte *)
Procedure TStream.write_byte(const V: Byte);
Begin
 Write(V, sizeof(V));
End;

(* TStream.write_word *)
Procedure TStream.write_word(const V: Word);
Begin
 Write(NtoBE(V), sizeof(V));
End;

(* TStream.write_integer *)
Procedure TStream.write_integer(const V: Integer);
Begin
 Write(NtoBE(V), sizeof(V));
End;

(* TStream.write_longword *)
Procedure TStream.write_longword(const V: Longword);
Begin
 Write(NtoBE(V), sizeof(V));
End;

(* TStream.write_int64 *)
Procedure TStream.write_int64(const V: Int64);
Begin
 Write(NtoBE(V), sizeof(V));
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
 For Ch in V Do
 Begin
  if (Ch = #0) Then
   raise Exception.Create('TStream.write_string: terminator char (0x00) found in string content!');

  write_byte(ord(Ch));
 End;

 write_byte(0);
End;

(* TStream.read_byte *)
Function TStream.read_byte: Byte;
Begin
 if (not Can) Then
  Exit(0);
 Read(Result, sizeof(Result));
End;

(* TStream.read_word *)
Function TStream.read_word: Word;
Begin
 if (not Can) Then
  Exit(0);
 Read(Result, sizeof(Result));
 Result := BEtoN(Result);
End;

(* TStream.read_integer *)
Function TStream.read_integer: Integer;
Begin
 if (not Can) Then
  Exit(0);
 Read(Result, sizeof(Result));
 Result := BEtoN(Result);
End;

(* TStream.read_longword *)
Function TStream.read_longword: Longword;
Begin
 if (not Can) Then
  Exit(0);
 Read(Result, sizeof(Result));
 Result := BEtoN(Result);
End;

(* TStream.read_int64 *)
Function TStream.read_int64: Int64;
Begin
 if (not Can) Then
  Exit(0);
 Read(Result, sizeof(Result));
 Result := BEtoN(Result);
End;

(* TStream.read_float *)
Function TStream.read_float: Extended;
Begin
 if (not Can) Then
  Exit(0);
 Read(Result, sizeof(Result));
End;

(* TStream.read_string *)
Function TStream.read_string: String;
Var Ch: Byte;
Begin
 Result := '';

 While (true) Do
 Begin
  Ch := read_byte;

  if (Ch = 0) Then // stop on terminator char (0x00)
   Break;

  Result += char(Ch);
 End;
End;

(* TStream.Can *)
Function TStream.Can: Boolean;
Begin
 Result := (Position < Size);
End;

End.
