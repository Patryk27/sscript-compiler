(*
 Copyright © by Patryk Wychowaniec, 2013
 All rights reserved.
*)
Unit Serialization;

 Interface
 Uses SysUtils, FGL;

 Type EInvalidSerializedData = class(Exception);

 Type TNode = class;
      TNodeList = specialize TFPGList<TNode>;

 { TNodeType }
 Type TNodeType = (ntValue, ntParent);

 { TNode }
 Type TNode =
      Class
       Private { fields }
        Typ     : TNodeType;
        Value   : String;
        Children: TNodeList;

       Public { methods }
        Constructor Create;
        Constructor Create(const fValue: String);
        Destructor Destroy; override;

        Function getChild(const Index: uint16): TNode;

        Property getType: TNodeType read Typ;
        Function getString: String;
        Function getInt: int32;
        Function getBool: Boolean;

        Property getValue: String read Value;
        Property getChildren: TNodeList read Children;

        Property Child[Index: uint16]: TNode read getChild; default;
       End;

 { TUnserializer }
 Type TUnserializer =
      Class
       Private
        Root: TNode;

       Public
        Constructor Create(const SerializedData: String);
        Destructor Destroy; override;

        Property getRoot: TNode read Root;
       End;

 Implementation

(* TNode.Create *)
Constructor TNode.Create;
Begin
 Children := TNodeList.Create;
End;

(* TNode.Create *)
Constructor TNode.Create(const fValue: String);
Begin
 Typ      := ntValue;
 Value    := fValue;
 Children := TNodeList.Create;
End;

(* TNode.Destroy *)
Destructor TNode.Destroy;
Var Node: TNode;
Begin
 inherited Destroy;

 For Node in Children Do
  Node.Free;
 Children.Free;
End;

(* TNode.getChild *)
Function TNode.getChild(const Index: uint16): TNode;
Begin
 Result := Children[Index];
End;

(* TNode.getString *)
Function TNode.getString: String;
Begin
 Result := Value;
End;

(* TNode.getInt *)
Function TNode.getInt: int32;
Begin
 Result := StrToInt(Value); // @TODO: any invalid data protection?
End;

(* TNode.getBool *)
Function TNode.getBool: Boolean;
Begin
 Result := (getInt = 1);
End;

// -------------------------------------------------------------------------- //
(* TUnserializer.Create *)
Constructor TUnserializer.Create(const SerializedData: String);
Var Position: uint32 = 1;

  { ReadNode }
  Function ReadNode: TNode;
  Var Value: String = '';
  Begin
   Result := TNode.Create;

   if (SerializedData[Position] = '(') Then
   Begin
    Inc(Position);

    While (true) Do
    Begin
     { ) }
     if (SerializedData[Position] = ')') Then
     Begin
      if (Value <> '') Then
      Begin
       if (Result.Children.Count = 0) Then
       Begin
        Result.Typ   := ntValue;
        Result.Value := Value;
       End Else
       Begin
        Result.Children.Add(TNode.Create(Value));
       End;
      End;

      Inc(Position);
      Exit;
     End Else

     { ( }
     if (SerializedData[Position] = '(') Then
     Begin
      Value      := '';
      Result.Typ := ntParent;
      Result.Children.Add(ReadNode());

      Continue;
     End Else

     { $ }
     if (SerializedData[Position] = '$') Then
     Begin
      if (SerializedData[Position-1] <> ')') Then
      Begin
       Result.Typ := ntParent;
       Result.Children.Add(TNode.Create(Value));
      End;

      Value := '';
     End Else

     Begin
      Value += SerializedData[Position];
     End;

     Inc(Position);
    End;
   End Else
    raise EInvalidSerializedData.CreateFmt('Expected a node opening at %d in "%s"', [Position, SerializedData]);
  End;

Begin
 if (Length(SerializedData) = 0) Then
  raise EInvalidSerializedData.Create('Input string is empty!');

 Root := ReadNode;
End;

(* TUnserializer.Destroy *)
Destructor TUnserializer.Destroy;
Begin
 inherited Destroy;

 Root.Free;
End;
End.
