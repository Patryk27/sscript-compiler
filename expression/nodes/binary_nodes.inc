(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.

 ----------
 Declarations of binary expression nodes.
*)

{ TMathExpressionNodeKind }
Type TMathExpressionNodeKind = (meAdd, meSub, meMul, meDiv, meMod);

{ TBitwiseMathExpressionNodeKind }
Type TBitwiseMathExpressionNodeKind = (beOr, beAnd, beXor, beShl, beShr);

{ TLogicalMathExpressionNodeKind }
Type TLogicalMathExpressionNodeKind = (leOr, leAnd);

{ TComprasionExpressionNodeKind }
Type TComprasionExpressionNodeKind = (ceLower, ceLowerEqual, ceEqual, ceGreaterEqual, ceGreater, ceDifferent);

{ TAssignExpressionNodeKind }
Type TAssignExpressionNodeKind = (aeAssign, aeAddAssign, aeSubAssign, aeMulAssign, aeDivAssign, aeModAssign, aeShlAssign, aeShrAssign, aeOrAssign, aeAndAssign, aeXorAssign);

{ TMathExpressionNode }
Type TMathExpressionNode =
     Class (TBinaryExpressionNode)
      Private
       Op: TMathExpressionNodeKind;

      Private
       Function getOpcode: TOpcodeKind;
       Function getStringOp: String;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TMathExpressionNodeKind);

       Function Optimize: TExpressionNode; override;
       Function Evaluate: TExpressionNode; override;
       Function Clone: TMathExpressionNode; override;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;
      End;

{ TBitwiseMathExpressionNode }
Type TBitwiseMathExpressionNode =
     Class (TBinaryExpressionNode)
      Private
       Op: TBitwiseMathExpressionNodeKind;

      Private
       Function getOpcode: TOpcodeKind;
       Function getStringOp: String;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TBitwiseMathExpressionNodeKind);

       Function Optimize: TExpressionNode; override;
       Function Clone: TBitwiseMathExpressionNode; override;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;
      End;

{ TLogicalMathExpressionNode }
Type TLogicalMathExpressionNode =
     Class (TBinaryExpressionNode)
      Private
       Op: TLogicalMathExpressionNodeKind;

      Private
       Function getOpcode: TOpcodeKind;
       Function getStringOp: String;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TLogicalMathExpressionNodeKind);

       Function Optimize: TExpressionNode; override;
       Function Evaluate: TExpressionNode; override;
       Function Clone: TLogicalMathExpressionNode; override;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;
      End;

{ TComprasionExpressionNode }
Type TComprasionExpressionNode =
     Class (TBinaryExpressionNode)
      Private
       Op: TComprasionExpressionNodeKind;

      Private
       Function CheckTypes(const fLType, fRType: TObject): Boolean;

       Function getOpcode: TOpcodeKind;
       Function getStringOp: String;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TComprasionExpressionNodeKind);

       Function Evaluate: TExpressionNode; override;
       Function Clone: TComprasionExpressionNode; override;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;
      End;

{ TAssignExpressionNode } // left = right
Type TAssignExpressionNode =
     Class (TBinaryExpressionNode)
      Private
       Op: TAssignExpressionNodeKind;

      Private
       Function getOpcode(const ReplaceAddToStrjoin: Boolean): TOpcodeKind;
       Function getStringOp: String;

       Function __compile_variable(const CanBeImmediate: Boolean; out LArg, RArg: TExpressionCompileResult; out VarName: String): TExpressionCompileResult;
       Function __compile_array(const CanBeImmediate: Boolean; out LArg, RArg: TExpressionCompileResult; out VarName: String): TExpressionCompileResult;

      Public
       Constructor Create(const fExprCompiler: TObject; const fToken: TToken_P; const fLeft, fRight: TExpressionNode; const fOp: TAssignExpressionNodeKind);

       Function Clone: TAssignExpressionNode; override;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;
      End;

{ TArrayElementExpressionNode }
Type TArrayElementExpressionNode =
     Class (TBinaryExpressionNode)
      Private
       // filled automatically in the '__prepare' routine during node compilation
       ArrayVarName: String;
       ArrayRefNode: TExpressionNode;
       ArrayTypeObj: TObject;

       ValueTypeObj: TObject;
       ValueRef    : TExpressionCompileResult;

      Private
       Procedure __prepare;
       Procedure __compile_indexes(out Index: TExpressionCompileResult);
       Function __compile_array(const CanBeImmediate: Boolean=False): TExpressionCompileResult;

      Public
       Function Clone: TArrayElementExpressionNode; override;
       Function isLValue: Boolean; override;

      Protected
       Function __compile(const CanBeImmediate: Boolean=False): TExpressionCompileResult; override;
      End;
