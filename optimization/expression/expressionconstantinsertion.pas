(*
 Copyright © by Patryk Wychowaniec, 2014
 All rights reserved.
*)
Unit ExpressionConstantInsertion;

 Interface
 Uses ExpressionOptimizer, Expression;

 { TExpressionConstantInsertion }
 Type TExpressionConstantInsertion =
      Class (TExpressionOptimizer)
       Private
        Function Parse(var Node: PExpressionNode): Boolean;

       Public
        Function Execute(var Node: PExpressionNode): Boolean; override;
       End;

 Implementation
Uses Messages, symdef;

(* TExpressionConstantInsertion.Parse *)
Function TExpressionConstantInsertion.Parse(var Node: PExpressionNode): Boolean;
Var Param: int32;
Begin
 Result := False;

 if (Node = nil) or (Node^.Typ = mtArrayElement) Then // stop on invalid/array expressions
  Exit;

 if (Node^.Typ = mtIdentifier) Then // if identifier
 Begin
  if (Node^.Symbol = nil) Then // if unknown symbol
  Begin
   if (RaiseErrors) Then
    Compiler.CompileError(Node^.Token, eUnknownVariable, [Node^.IdentName]);
   Exit;
  End;

  With TSymbol(Node^.Symbol) do
  Begin
   if (Typ <> stConstant) Then // is it a constant?
   Begin
    if (RaiseErrors) Then
     Compiler.CompileError(Node^.Token, eNotAConstant, [Node^.IdentName]);
    Exit;
   End;

   if (mVariable.Typ.isEnumItem) Then
    Exit;

   Node^ := mVariable.Value^;
   Exit(True);
  End;
 End;

 Parse(Node^.Left);
 Parse(Node^.Right);

 For Param := Low(Node^.ParamList) To High(Node^.ParamList) Do
  Parse(Node^.ParamList[Param]);
End;

(* TExpressionConstantInsertion.Execute *)
Function TExpressionConstantInsertion.Execute(var Node: PExpressionNode): Boolean;
Begin
 Result := Parse(Node);
End;
End.
