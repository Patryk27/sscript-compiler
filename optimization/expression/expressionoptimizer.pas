(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
Unit ExpressionOptimizer;

 Interface
 Uses Optimizer, Expression;

 { TExpressionOptimizer }
 Type TExpressionOptimizer =
      Class (TOptimizer)
       Private
       Public
        Function Execute(var Node: PExpressionNode): Boolean; virtual; abstract;
       End;

 { TOperatorSimplifyData }
 Type TOperatorSimplifyData =
      Record
       Before, After: TExpressionNodeType;
      End;

 { x = x op expr -> x op= expr }
 Const OperatorSimplifyData1: Array[0..8] of TOperatorSimplifyData =
 (
  (Before: mtAdd; After: mtAddEq),
  (Before: mtSub; After: mtSubEq),
  (Before: mtMul; After: mtMulEq),
  (Before: mtDiv; After: mtDivEq),
  (Before: mtShl; After: mtShlEq),
  (Before: mtShr; After: mtShrEq),
  (Before: mtBitwiseAND; After: mtAndEq),
  (Before: mtBitwiseOR; After: mtOrEq),
  (Before: mtXor; After: mtXorEq)
 );

 { x = expr op x -> x op= expr }
 Const OperatorSimplifyData2: Array[0..4] of TOperatorSimplifyData =
 (
  (Before: mtAdd; After: mtAddEq),
  (Before: mtMul; After: mtMulEq),
  (Before: mtBitwiseAND; After: mtAndEq),
  (Before: mtBitwiseOR; After: mtOrEq),
  (Before: mtXor; After: mtXorEq)
 );

 { !(boolA op boolB) -> (boolA not-op boolB) }
 Const OperatorSimplifyData3: Array[0..5] of TOperatorSimplifyData =
 (
  (Before: mtEqual; After: mtDifferent),
  (Before: mtDifferent; After: mtEqual),
  (Before: mtLower; After: mtGreaterEqual),
  (Before: mtGreater; After: mtLowerEqual),
  (Before: mtLowerEqual; After: mtGreater),
  (Before: mtGreaterEqual; After: mtLower)
 );

 Implementation

End.
