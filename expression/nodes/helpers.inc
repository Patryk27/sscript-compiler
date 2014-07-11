{ TExpressionCompileResultHelper }
Type TExpressionCompileResultHelper =
     Record helper for TExpressionCompileResult
      Function getType: TType;
      Function getTypeName: String;
     End;

{ TExpressionNodeHelper }
Type TExpressionNodeHelper =
     Class helper for TExpressionNode
      Protected
       Procedure CompileError(const Token: TToken_P; const Error: TCompileError; const Args: Array of Const);
       Procedure CompileHint(const Token: TToken_P; const Hint: TCompileHint; const Args: Array of Const);

      Protected
       Function getCompiler: TCompiler;
      End;

{ TIdentifierExpressionNodeHelper }
Type TIdentifierExpressionNodeHelper =
     Class helper for TIdentifierExpressionNode
      Protected
       Function getSymbol: TSymbol;
       Function getVariable: TVariable;
      End;

// -------------------------------------------------------------------------- //
(* TExpressionCompileResultHelper.getType *)
Function TExpressionCompileResultHelper.getType: TType;
Begin
 Result := TType(Typ);
End;

(* TExpressionCompileResultHelper.getTypeName *)
Function TExpressionCompileResultHelper.getTypeName: String;
Begin
 if (getType = nil) Then
  Exit('erroneous type');

 Result := getType.RefSymbol.Name;

 if (Length(Result) = 0) { "" } or
    (Copy(Result, Length(Result)-1, 2) = '::') { self:: } Then
 Begin
  Result := getType.asString;
 End;
End;

// -------------------------------------------------------------------------- //
(* TExpressionNodeHelper.CompileError *)
Procedure TExpressionNodeHelper.CompileError(const Token: TToken_P; const Error: TCompileError; const Args: Array of const);
Begin
 getCompiler.CompileError(Token, Error, Args);
End;

(* TExpressionNodeHelper.CompileHint *)
Procedure TExpressionNodeHelper.CompileHint(const Token: TToken_P; const Hint: TCompileHint; const Args: Array of const);
Begin
 getCompiler.CompileHint(Token, Hint, Args);
End;

(* TExpressionNodeHelper.getCompiler *)
Function TExpressionNodeHelper.getCompiler: TCompiler;
Begin
 Result := TCompiler(ExprCompiler.getCompiler);
End;

// -------------------------------------------------------------------------- //
(* TIdentifierExpressionNodeHelper.getSymbol *)
Function TIdentifierExpressionNodeHelper.getSymbol: TSymbol;
Begin
 Result := TSymbol(inherited getSymbol);
End;

(* TIdentifierExpressionNodeHelper.getVariable *)
Function TIdentifierExpressionNodeHelper.getVariable: TVariable;
Var Sym: TSymbol;
Begin
 // fetch symbol
 Sym := getSymbol;

 // check symbol type
 if (not (Sym.Typ in [stVariable, stConstant])) Then
  raise EExpressionNodeException.Create('Invalid symbol type.');

 // return result
 Result := Sym.mVariable;
End;
