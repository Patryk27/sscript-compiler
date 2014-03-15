(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit VariableAllocator;

 Interface
 Uses SSCompiler, symdef;

 { TVariableAllocator }
 Type TVariableAllocator =
      Class
       Protected
        Compiler       : TCompiler;
        CurrentFunction: TFunction;

       Public
        Constructor Create(const fCompiler: TCompiler; const fCurrentFunction: TFunction);

        Procedure Execute; virtual; abstract;
       End;

 Implementation

(* TVariableAllocator.Create *)
Constructor TVariableAllocator.Create(const fCompiler: TCompiler; const fCurrentFunction: TFunction);
Begin
 Compiler        := fCompiler;
 CurrentFunction := fCurrentFunction;
End;
End.
