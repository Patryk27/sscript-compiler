(*
 Copyright © by Patryk Wychowaniec, 2013-2014
 All rights reserved.
*)
Unit Messages;

 Interface

 { errors }
 Type TCompileError =
 (
  eEOF, eUnexpected, eExpected, eExpectedIdentifier, eExpectedOperator, eExpectedValue, eExpectedDeclOrDef, eExpectedString,
  eExpectedInt, eNotAllowed,
  eRedeclaration, eLValueExpected, eUnknownType, eUnknownVariable, eUnknownFunction, eWrongType, eUnsupportedOperator, eUnsupportedUOperator,
  eDivByZero, eWrongParamCount, eInvalidArraySubscript, eUnknownInclude, eCorruptedSSMFile, eStringExceedsLine, eUnknownMacro, eFunctionNotFound,
  eVoidVar, eVoidParam, eVoidCasting, eVoidArray, eAnyArray, eExpectedConstant, eWrongTypeInCall, eWrongTypeInAssign, eInvalidArrayAssign, eInvalidExpression,
  ePrevDeclared, eUnknownNamespace, eUnimplemented, eAmbiguousIdentifier, eVoidNoNameParam, eNoValidMainFunctionFound,
  eNotAConstant, eNonObjectMethodCall, eMethodNotFound, eInvalidConversion, eUnfinishedComment, eInvalidInteger, eInvalidFloat, eInvalidCharLiteral,
  eUnknownAttribute, eFileNotFound, eDefaultParamValueRequired, eInvalidForeach, eVarArrayRequired,
  eInvalidArrayInitializer, eCannotBeCalled, eNotAType, eCannotBeUsedAsFunction, eUnknownIdentifier, eReturnWithNoValue, eGlobalArrayInitializer,
  eBytecode_LabelNotFound, eBytecode_ExportNotFound, eBytecode_InvalidOpcode, eBytecode_StringNotFound,
  eLinker_UnknownReference
 );

 Const error_stop: Set of TCompileError = [eEOF, eUnexpected, eExpected, eExpectedIdentifier, eExpectedOperator, eExpectedValue, eExpectedString,
                                           eExpectedInt, eUnknownType, eFileNotFound, eDivByZero, eVarArrayRequired,
                                           eExpectedDeclOrDef, eNotAllowed, eUnknownInclude, eCorruptedSSMFile, eStringExceedsLine,
                                           eUnknownMacro, eInvalidExpression, eUnimplemented, eInvalidInteger, eInvalidFloat, eInvalidCharLiteral,
                                           eGlobalArrayInitializer];

 Const CompileError_fmt: Array[TCompileError] of String =
 (
  'Unexpected end-of-file',
  'Syntax error: unexpected `%s`',
  'Syntax error: expected `%s` but `%s` found',
  'Syntax error: expected identifier but `%s` found',
  'Syntax error: expected operator but `%s` found',
  'Syntax error: expected value but `%s` found',
  'Syntax error: expected declaration or definition but `%s` found',
  'Syntax error: expected string but `%s` found',
  'Syntax error: expected int but `%s` found',
  'Syntax error: `%s` not allowed here',
  'Redeclaration of identifier: `%s`',
  'An l-value was expected',
  'Unknown type: `%s`',
  'Unknown variable: `%s`',
  'Unknown function: `%s`',
  'Wrong type; got `%s` expected `%s`',
  'Unsupported binary operator: `%s` %s `%s`',
  'Unsupported unary operator: %s %s',
  'Division by zero',
  'Wrong number of parameters (in call to `%s`) - expected %d got %d',
  'Invalid types ''%s [%s]'' for array subscript',
  'Unknown include file: ''%s''',
  'Corrupted SSM file: ''%s''',
  'String exceeds line',
  'Unknown macro: ''%s''',
  'Function ''%s'' not found in library ''%s''',
  'Variable ''%s'' declared as void',
  'Parameter ''%s'' declared as void',
  'Cannot cast from or to ''void'' type',
  'Array declared as ''void''',
  'Array declared as ''any''',
  'A constant value was expected',
  'Wrong type (in call to ''%s'', param #%d) - got `%s` expected `%s`',
  'Wrong type (in assignment to ''%s'') - got `%s` expected `%s`',
  'Invalid array assignment',
  'Invalid expression',
  'Previously declared here',
  'Namespace not found: `%s`',
  'Feature `%s` is not implemented yet, sorry ;< Check in future releases.',
  'Identifier `%s` is ambiguous in current context',
  'Parameter declared as void',
  'No valid `main` function found',
  'Not a constant var: `%s`',
  'Tried to call method `%s` on non-object (`%s`)',
  'Method `%s` not found in object `%s`',
  'Invalid conversion from `%s` to `%s`',
  'Unfinished comment',
  'Invalid integer: `%s`',
  'Invalid float: `%s`',
  'Invalid char literal',
  'Unknown attribute: `%s`',
  'File not found: %s',
  'Default parameter value required for `%s`',
  'Invalid foreach construction (foreach-var and foreach-expr have the same types)',
  'Variable of an array type required',
  'Invalid array initializer; got %dD array, expecting %d-dimensional',
  '`%s` cannot be called',
  'Not a type: `%s`',
  'Expression of type `%s` cannot be used as a function',
  'Unknown identifier: `%s`',
  'Return with no value in function returning non-void',
  'Global array initializers cannot be declared',

  '[Bytecode] Label not found: `%s`',
  '[Bytecode] Export (label) not found: `%s`',
  '[Bytecode] Invalid opcode',
  '[Bytecode] String not found: `%s`',

  '[Linker] Unknown reference to: `%s`'
 );

 { warnings }
 Type TCompileWarning =
 (wNotEveryPathReturnsAValue, wPublicFunctionUsesPrivateSymbol);

 Const CompileWarning_fmt: Array[TCompileWarning] of String =
 (
  'Not every code path returns a value!',
  'Public function ''%s'' uses a private symbol ''%s''!'
 );

 { hints }
 Type TCompileHint =
 (hDidntYouMean, hUnreachableCode, hUseOfUninitializedVariable, hExpressionHasNoEffect, hExpressionAlwaysTrue, hExpressionAlwaysFalse);

 Const CompileHint_fmt: Array[TCompileHint] of String =
 (
  'Didn''t you mean: `%s`?',
  'Unreachable code',
  'Use of a possibly uninitialized variable `%s`',
  'Expression has no effect',
  'Expression always evaluates to ''true''',
  'Expression always evaluates to ''false'''
 );

 { notes }
 Type TCompileNote =
 (nCandidates, nCandidate);

 Const CompileNote_fmt: Array[TCompileNote] of String =
 (
  'Candidates are:',
  '%s'
 );

 Implementation

End.
