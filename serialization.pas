(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Serialization;

 Interface
 Uses Variants, SysUtils, FGL;

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

        Function getChild(const Index: uint32): TNode;

        Function getString: String;
        Function getInt: int64;
        Function getFloat: Extended;
        Function getBool: Boolean;
        Function getVariant: Variant;
        Function getExpression: Pointer;

       Public
        Property getType: TNodeType read Typ;
        Property getValue: String read Value;
        Property getChildren: TNodeList read Children;

        Property Child[Index: uint32]: TNode read getChild; default;
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
Uses Expression;

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
Function TNode.getChild(const Index: uint32): TNode;
Begin
 Result := Children[Index];
End;

(* TNode.getString *)
Function TNode.getString: String;
Begin
 Result := Value;
End;

(* TNode.getInt *)
Function TNode.getInt: int64;
Begin
 Result := StrToInt64(Value); // @TODO: any invalid data protection?
End;

(* TNode.getFloat *)
Function TNode.getFloat: Extended;
Begin
 Result := StrToFloat(Value);
End;

(* TNode.getBool *)
Function TNode.getBool: Boolean;
Begin
 Result := (LowerCase(getString) = 'true') or (getInt = 1);
End;

(* TNode.getVariant *)
Function TNode.getVariant: Variant;
Var Str  : String;
    Int  : int64;
    Float: Extended;
Begin
 Str := getString;

 // try converting to an int
 if (TryStrToInt64(Str, Int)) Then
  Exit(Int);

 // try converting to a float
 if (TryStrToFloat(Str, Float)) Then
  Exit(Float);

 // maybe a bool then?
 if (LowerCase(Str) = 'true') Then
  Exit(True);

 if (LowerCase(Str) = 'false') Then
  Exit(False);

 // so it's a string
 Exit(Str);
End;

(* TNode.getExpression *)
Function TNode.getExpression: Pointer;
Begin
 Result := TExpressionNode.Create(self);
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
    raise EInvalidSerializedData.CreateFmt('Expected node opening at %d in "%s"', [Position, SerializedData]);
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
