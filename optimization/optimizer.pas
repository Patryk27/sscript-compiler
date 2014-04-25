(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
Unit Optimizer;

 Interface
 Uses HLCompiler, symdef;

 { TOptimizer }
 Type TOptimizer =
      Class
       Protected
        Compiler       : TCompiler;
        CurrentFunction: TFunction;
        RaiseErrors    : Boolean;

       Public
        Constructor Create(const fCompiler: TCompiler; const fCurrentFunction: TFunction; const fRaiseErrors: Boolean=True);
        Constructor Create(const fCompiler: Pointer; const fCurrentFunction: TFunction; const fRaiseErrors: Boolean=True);
       End;

 Implementation

(* TOptimizer.Create *)
Constructor TOptimizer.Create(const fCompiler: TCompiler; const fCurrentFunction: TFunction; const fRaiseErrors: Boolean);
Begin
 Compiler        := fCompiler;
 CurrentFunction := fCurrentFunction;
 RaiseErrors     := fRaiseErrors;
End;

(* TOptimizer.Create *)
Constructor TOptimizer.Create(const fCompiler: Pointer; const fCurrentFunction: TFunction; const fRaiseErrors: Boolean);
Begin
 Compiler        := TCompiler(fCompiler);
 CurrentFunction := fCurrentFunction;
 RaiseErrors     := fRaiseErrors;
End;
End.
