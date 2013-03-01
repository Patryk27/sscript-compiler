(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
{$H+}
Unit Stream;

 Interface
 Uses Classes;

 Type TStream = Class (Classes.TMemoryStream)
                 Public
                  // `write` functions
                  Procedure write_byte(const V: Byte);
                  Procedure write_word(const V: Word);
                  Procedure write_integer(const V: Integer);
                  Procedure write_longword(const V: Longword);
                  Procedure write_extended(const V: Extended);
                  Procedure write_string(const V: String);

                  // `read` functions
                  Function read_byte: Byte;
                  Function read_longword: Longword;
                  Function read_string: String;

                  // other functions
                  Procedure CopyToBuffer(var Buffer: PByte; const AllocateMemory: Boolean);
                  Function Can: Boolean;
                 End;

 Implementation

{ TStream.write_byte }
Procedure TStream.write_byte(const V: Byte);
Begin
 Write(V, sizeof(V));
End;

{ TStream.write_word }
Procedure TStream.write_word(const V: Word);
Begin
 Write(V, sizeof(V));
End;

{ TStream.write_integer }
Procedure TStream.write_integer(const V: Integer);
Begin
 Write(V, sizeof(V));
End;

{ TStream.write_longword }
Procedure TStream.write_longword(const V: Longword);
Begin
 Write(V, sizeof(V));
End;

{ TStream.write_extended }
Procedure TStream.write_extended(const V: Extended);
Begin
 Write(V, sizeof(V));
End;

{ TStream.write_string }
Procedure TStream.write_string(const V: String);
Var Ch: Char;
Begin
 For Ch in V Do
  write_byte(ord(Ch));
 write_byte(0);
End;

{ TStream.read_byte }
Function TStream.read_byte: Byte;
Begin
 Read(Result, sizeof(Result));
End;

{ TStream.read_longword }
Function TStream.read_longword: Longword;
Begin
 Read(Result, sizeof(Result));
End;

{ TStream.read_string }
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

{ TStream.CopyToBuffer }
Procedure TStream.CopyToBuffer(var Buffer: PByte; const AllocateMemory: Boolean);
Var OldPos, I: LongWord;
Begin
 OldPos := Position;

 if (AllocateMemory) Then
  Buffer := AllocMem(Size);

 For I := 0 To Size Do
  Buffer[I] := read_byte;

 Position := OldPos;
End;

{ TStream.Can }
Function TStream.Can: Boolean;
Begin
 Result := (Position < Size);
End;

End.
