(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 ----------
 Declarations of nullary expression nodes.
*)

{ TConstantExpressionNode }
Type TConstantExpressionNode =
     Class (TNullaryExpressionNode)
      Private
       Typ  : TObject;
       Value: TMixedValue;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fValue: TMixedValue);
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const Node: Serialization.TNode);

       Function Clone: TConstantExpressionNode; override;

       Function getPredictedType: TMixedValueKind; override;

       Function getValue: String;
       Function getSerializedForm: String;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;

      Public
       Property getType: TObject read Typ;
       Property getMixedValue: TMixedValue read Value;
      End;

{ TGenericConstantExpressionNode }
Type generic TGenericConstantExpressionNode<T> =
     Class (TConstantExpressionNode)
      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fValue: T);
      End;

{ TBooleanExpressionNode }
Type TBooleanExpressionNode =
     Class (specialize TGenericConstantExpressionNode<Boolean>)
     End;

{ TCharExpressionNode }
Type TCharExpressionNode =
     Class (specialize TGenericConstantExpressionNode<Char>)
     End;

{ TIntegerExpressionNode }
Type TIntegerExpressionNode =
     Class (specialize TGenericConstantExpressionNode<int64>)
     End;

{ TFloatExpressionNode }
Type TFloatExpressionNode =
     Class (specialize TGenericConstantExpressionNode<Extended>)
     End;

{ TStringExpressionNode }
Type TStringExpressionNode =
     Class (specialize TGenericConstantExpressionNode<String>)
     End;

{ TIdentifierExpressionNode }
Type TIdentifierExpressionNode =
     Class (TNullaryExpressionNode)
      Private
       Name  : String;
       Symbol: TObject;

      Public
       Constructor Create(const fExprCompiler: TExpressionCompiler; const fToken: TToken_P; const fName: String; const fSymbol: TObject);

       Function Evaluate: TExpressionNode; override;
       Function Clone: TIdentifierExpressionNode; override;

       Function findIdentifier(const fSymbol: TObject): Boolean; override;

       Function isLValue: Boolean; override;
       Function getType: TObject;
       Function getLocation: String;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;

      Public
       Property getName: String read Name;
       Property getSymbol: TObject read Symbol;
      End;
