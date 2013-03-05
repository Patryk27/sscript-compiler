Unit Messages;

 Interface

 { errors }
 Type TCompileError =
 (eInternalError, eEOF, eUnexpected, eExpected, eExpectedIdentifier, eExpectedOperator, eExpectedValue, eExpectedDeclOrDef, eExpectedType, eNotAllowed,
  eRedeclaration, eLValueRequired, eUnknownType, eUnknownVariable, eUnknownFunction, eWrongType, eUnsupportedOperator, eUnsupportedUOperator,
  eDivByZero, eWrongParamCount, eInvalidArraySubscript, eUnknownInclude, eCorruptedSSMFile, eStringExceedsLine, eUnknownMacro, eFunctionNotFound,
  eVoidVar, eVoidParam, eVoidCasting, eVoidArray, eExpectedConstant, eWrongTypeInCall, eWrongTypeInAssign, eInvalidArrayAssign, eInvalidExpression,
  ePrevDeclared, eUnknownNamespace, eUnimplemented, eAmbiguousCall, eAmbiguousVariable, eAmbiguousIdentifier, eVoidNoNameParam, eNoValidMainFunctionFound,
  eNotAConstant,
  eBytecode_LabelNotFound, eBytecode_ExportNotFound, eBytecode_InvalidOpcode, eBytecode_StringNotFound);

 Const error_stop: Set of TCompileError = [eEOF, eUnexpected, eExpected, eExpectedIdentifier, eExpectedOperator, eExpectedValue,
                                           eExpectedDeclOrDef, eExpectedType, eNotAllowed, eUnknownInclude, eCorruptedSSMFile, eStringExceedsLine,
                                           eUnknownMacro, eInvalidExpression, eUnimplemented];

 Const CompileError_fmt: Array[TCompileError] of String =
 (
  'Internal error: %s',
  'Unexpected end-of-file',
  'Syntax error: unexpected `%s`',
  'Syntax error: expected `%s` but `%s` found',
  'Syntax error: expected identifier but `%s` found',
  'Syntax error: expected operator but `%s` found',
  'Syntax error: expected value but `%s` found',
  'Syntax error: expected declaration or definition but `%s` found',
  'Syntax error: expected type but `%s` found',
  'Syntax error: `%s` not allowed here',
  'Redeclaration of identifier: `%s`',
  'lvalue required',
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
  'Function ''%s'' not found in library ''%s'' (wrong declaration?)',
  'Variable ''%s'' declared as void',
  'Parameter ''%s'' declared as void',
  'Cannot cast from or to ''void'' type',
  'Array declared as void',
  'Expected constant value',
  'Wrong type (in call to ''%s'', param #%d) - got `%s` expected `%s`',
  'Wrong type (in assignment to ''%s'') - got `%s` expected `%s`',
  'Invalid array assignment',
  'Invalid expression',
  'Previously declared here',
  'Namespace not found: `%s`',
  'Feature `%s` is not implemented yet, sorry ;< Check in future releases.',
  'Call to function `%s` is ambiguous in current namespace',
  'Variable `%s` is ambiguous in current namespace',
  'Identifier `%s` is ambiguous in current namespace',
  'Parameter declared as void',
  'No valid `main` function found',
  'Not a constant-var: `%s`',

  '[Bytecode] Label not found: `%s`',
  '[Bytecode] Export (label) not found: `%s`',
  '[Bytecode] Invalid opcode',
  '[Bytecode] String not found: `%s`'
 );

 { warnings }

 { hints }
 Type TCompileHint =
 (hDidntYouMean);

 Const CompileHint_fmt: Array[TCompileHint] of String =
 (
  'Didn''t you mean: `%s`?'
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
