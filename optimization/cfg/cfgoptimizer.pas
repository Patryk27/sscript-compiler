(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
Unit CFGOptimizer;

 Interface
 Uses HLCompiler, Optimizer, FlowGraph, symdef, SysUtils;

 { ECFGOptimizerException }
 Type ECFGOptimizerException = Class(Exception);

 { TCFGOptimizer }
 Type TCFGOptimizer =
      Class (TOptimizer)
       Protected
        RemovedNodes: TCFGNodeList;

       Public
        Constructor Create(const fCompiler: TCompiler; const fCurrentFunction: TFunction);

        Function Execute: Boolean; virtual; abstract;
       End;

 Implementation

(* TCFGOptimizer.Create *)
Constructor TCFGOptimizer.Create(const fCompiler: TCompiler; const fCurrentFunction: TFunction);
Begin
 inherited Create(fCompiler, fCurrentFunction);

 RemovedNodes := TCFGNodeList.Create;
End;
End.
