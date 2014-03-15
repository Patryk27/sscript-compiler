(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit StackAllocator;

 Interface
 Uses VariableAllocator;

 { TStackAllocator }
 Type TStackAllocator =
      Class (TVariableAllocator)
       Private
       Public
        Procedure Execute; override;
       End;

 Implementation
Uses symdef;

(* TStackAllocator.Execute *)
Procedure TStackAllocator.Execute;
Var Symbol  : TSymbol;
    StackPos: uint16 = 0;
Begin
 { stack-only allocation }
 For Symbol in CurrentFunction.SymbolList Do
 Begin
  if (Symbol.Typ = stVariable) Then
  Begin
   if (not Symbol.mVariable.isConst) and (not Symbol.mVariable.isFuncParam) and (not Symbol.mVariable.DontAllocate) Then
   Begin
    With Symbol.mVariable.LocationData do
    Begin
     Location      := vlStack;
     StackPosition := -StackPos;
    End;

    Inc(StackPos);
    Inc(CurrentFunction.StackSize);
   End;
  End;
 End;
End;
End.
