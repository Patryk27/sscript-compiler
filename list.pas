(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.

 TList class.
*)
Unit List;

 Interface
 Uses SysUtils;

 (* EListException *)
 Type EListException = Class(Exception);

 (* TListEnumerator *)
 Type generic TListEnumerator<T> =
      Class
       Private
        List    : TObject;
        Position: uint32;

       Private
        Function getCurrent: T;

       Public
        Constructor Create(const fList: TObject);

        Function MoveNext: Boolean;
        Property Current: T read getCurrent;
       End;

 (* TList *)
      generic TList<T> =
      Class
       Type TSpecListEnumerator = specialize TListEnumerator<T>;

       Private
        Data    : Array of T;
        Position: uint32;

       Private
        Function getItem(const Index: uint32): T;
        Procedure setItem(const Index: uint32; const Value: T);

        Function getCount: int32;

       Public
        Constructor Create(const fSize: uint32=0);
        Destructor Destroy; override;

        Function Add(const Item: T): uint32;
        Procedure Remove(const Index: uint32);

        Function getEnumerator: TSpecListEnumerator;

       Public
        Property Items[const Index: uint32]: T read getItem write setItem; default;
        Property Count: int32 read getCount;
       End;

 Implementation

(* TListEnumerator.getCurrent *)
Function TListEnumerator.getCurrent: T;
Type TSpecList = specialize TList<T>;
Begin
 Result := TSpecList(List)[Position-1];
End;

(* TListEnumerator.Create *)
Constructor TListEnumerator.Create(const fList: TObject);
Begin
 List     := fList;
 Position := 0;
End;

(* TListEnumerator.MoveNext *)
Function TListEnumerator.MoveNext: Boolean;
Type TSpecList = specialize TList<T>;
Begin
 Result := (Position < uint32(TSpecList(List).Count));
 Inc(Position);
End;

// -------------------------------------------------------------------------- //
(* TList.getItem *)
{
 Returns item with given index on the list.
}
Function TList.getItem(const Index: uint32): T;
Begin
 // check bounds
 if (Index >= Position) Then
  raise EListException.CreateFmt('Cannot fetch element from the list - index out of the bounds (index=%d, size=%d)', [Index, Position]);

 // return element
 Result := Data[Index];
End;

(* TList.setItem *)
{
 Changes list item.
}
Procedure TList.setItem(const Index: uint32; const Value: T);
Begin
 // check bounds
 if (Index >= Position) Then
  raise EListException.CreateFmt('Cannot change element on the list - index out of the bounds (index=%d, size=%d)', [Index, Position]);

 // change element
 Data[Index] := Value;
End;

(* TList.getCount *)
{
 Returns list size (number of elements).
}
Function TList.getCount: int32;
Begin
 Result := Position;
End;

(* TList.Create *)
Constructor TList.Create(const fSize: uint32);
Begin
 SetLength(Data, fSize);
 Position := fSize;
End;

(* TList.Destroy *)
Destructor TList.Destroy;
Begin
 SetLength(Data, 0);

 inherited Destroy;
End;

(* TList.Add *)
Function TList.Add(const Item: T): uint32;
Begin
 // extend array, if necessary
 if (Position >= uint32(Length(Data))) Then
  SetLength(Data, Length(Data)+50);

 // put element
 Data[Position] := Item;
 Result         := Position;

 // increment counter
 Inc(Position);
End;

(* TList.Remove *)
Procedure TList.Remove(const Index: uint32);
Var I: uint32;
Begin
 // check bounds
 if (Index >= Position) Then
  raise EListException.CreateFmt('Cannot remove element from the list - index out of the bounds (index=%d, size=%d)', [Index, Position]);

 // remove element
 For I := Index To Position Do
  Data[I] := Data[I+1];

 // decrement counter
 Dec(Position);

 // shrink array, if more than 50 elements are unused (empty)
 if (uint32(Length(Data)) - Position > 50) Then
  SetLength(Data, Length(Data)-50);
End;

(* TList.getEnumerator *)
{
 Returns enumerator for the list.
}
Function TList.getEnumerator: TSpecListEnumerator;
Begin
 Result := TSpecListEnumerator.Create(self);
End;
End.
