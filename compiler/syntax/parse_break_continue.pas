{ ParseBreak }
Procedure ParseBreak;
Var I: Integer;
    C: TMConstruction;
Begin
 For I := High(Scope) Downto Low(Scope) Do
  if (Scope[I].Typ in [sFOR, sWHILE]) Then
  Begin
   C.Typ := ctJump;
   SetLength(C.Values, 1);
   C.Values[0] := CopyStringToPChar(':'+Scope[I].LoopEnd);
   AddConstruction(C);
   eat(_SEMICOLON);
   Exit;
  End;

 CompileError(eNotAllowed, ['break']);
End;

{ ParseContinue }
Procedure ParseContinue;
Var I: Integer;
    C: TMConstruction;
Begin
 For I := High(Scope) Downto Low(Scope) Do
  if (Scope[I].Typ in [sFOR, sWHILE]) Then
  Begin
   C.Typ := ctJump;
   SetLength(C.Values, 1);
   C.Values[0] := CopyStringToPChar(':'+Scope[I].LoopEnd);
   AddConstruction(C);
   eat(_SEMICOLON);
   Exit;
  End;

 CompileError(eNotAllowed, ['continue']);
End;
