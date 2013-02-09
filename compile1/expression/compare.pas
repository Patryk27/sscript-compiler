Procedure ParseCompare;
Var TypeLeft, TypeRight: TVType;
    Opcode             : TOpcode_E;
Begin
 Result := CompileSimple(TypeLeft, TypeRight);

 Case Expr^.Typ of
  mtLower       : Opcode := o_if_l;
  mtGreater     : Opcode := o_if_g;
  mtEqual       : Opcode := o_if_e;
  mtLowerEqual  : Opcode := o_if_le;
  mtGreaterEqual: Opcode := o_if_ge;
  mtDifferent   : Opcode := o_if_ne;
 End;

 Compiler.PutOpcode(Opcode, ['e'+Compiler.getTypePrefix(TypeLeft)+'1', 'e'+Compiler.getTypePrefix(TypeRight)+'2']);

 Result      := TYPE_BOOL;
 Push_IF_reg := True;
End;
