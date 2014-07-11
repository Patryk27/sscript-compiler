(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 ----------
 Declarations of unary expression nodes.
*)

{ TPrePostOpExpressionNodeKind }
Type TPrePostOpExpressionNodeKind = (pePreInc, pePreDec, pePostInc, pePostDec);

{ TPrePostOpExpressionNode }
Type TPrePostOpExpressionNode =
     Class (TUnaryExpressionNode)
      Private
       Op: TPrePostOpExpressionNodeKind;

      Private
       Function getOpcode: TOpcodeKind;
       Function getStringOp: String;

       Function isPre: Boolean;
       Function isPost: Boolean;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode; const fOp: TPrePostOpExpressionNodeKind);

       Function Clone: TPrePostOpExpressionNode; override;
      End;

{ TNegExpressionNode }
Type TNegExpressionNode =
     Class (TUnaryExpressionNode)
      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;

      Public
       Function Evaluate: TExpressionNode; override;
       Function Clone: TNegExpressionNode; override;
      End;

{ TLogicalNotExpressionNode }
Type TLogicalNotExpressionNode =
     Class (TUnaryExpressionNode)
      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;

      Public
       Function Clone: TLogicalNotExpressionNode; override;
      End;

{ TBitwiseNotExpressionNode }
Type TBitwiseNotExpressionNode =
     Class (TUnaryExpressionNode)
      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;

      Public
       Function Clone: TBitwiseNotExpressionNode; override;
      End;

{ TCallExpressionNode }
Type TCallExpressionNode =
     Class (TUnaryExpressionNode)
      Private
       ArgumentList: TCallArgumentList;

      Private
       Procedure CompileParameterList(const FunctionName: String; const fParamList: Pointer);
       Procedure CleanAfterCall(const fParamList: Pointer);

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;

      Public
       Constructor Create(const fExprCompiler: TExpressionCompiler; const fToken: TToken_P; const Callee: TExpressionNode);

       Function Evaluate: TCallExpressionNode; override;
       Function Clone: TCallExpressionNode; override;

       Function getCost: uint32; override;

       Procedure AddArgument(const Expr: TExpressionNode);

      Public
       Property getArgumentList: TCallArgumentList read ArgumentList;
      End;

{ TNewExpressionNode }
Type TNewExpressionNode =
     Class (TUnaryExpressionNode)
      Protected
       mType: TObject;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode; const fType: TObject);

       Function Clone: TNewExpressionNode; override;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;
      End;

{ TCastExpressionNode }
Type TCastExpressionNode =
     Class (TUnaryExpressionNode)
      Protected
       mType: TObject;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode; const fType: TObject);

       Function Clone: TCastExpressionNode; override;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;
      End;

{ TFieldFetchExpressionNode }
Type TFieldFetchExpressionNode =
     Class (TUnaryExpressionNode)
      Protected
       FieldName: String;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fChild: TExpressionNode; const fFieldName: String);

       Function Clone: TFieldFetchExpressionNode; override;

      Public
       Property getFieldName: String read FieldName;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;
      End;
