Unit Messages;

 Interface

 Type TCompileError =
 (eInternalError, eEOF, eUnexpected, eExpected, eExpectedIdentifier, eExpectedOperator, eExpectedValue, eExpectedDeclOrDef, eNotAllowed,
  eRedeclaration, eLValueExpected, eUnknownType, eUnknownVariable, eUnknownFunction, eWrongType, eUnsupportedOperator, eUnsupportedUOperator,
  eDivByZero, eWrongParamCount, eInvalidArraySubscript, eUnknownInclude, eCorruptedSSMFile, eStringExceedsLine, eUnknownMacro, eFunctionNotFound,
  eVarVoid, eParamVoid,
  eBytecode_LabelNotFound, eBytecode_ExportNotFound, eBytecode_InvalidOpcode, eBytecode_StringNotFound);

 Const error_stop: Set of TCompileError = [eEOF, eUnexpected, eExpected, eExpectedIdentifier, eExpectedOperator, eExpectedValue,
                                           eExpectedDeclOrDef, eNotAllowed, eUnknownInclude, eCorruptedSSMFile, eStringExceedsLine,
                                           eUnknownMacro];

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
  'Syntax error: `%s` not allowed here',
  'Redeclaration of identifier: `%s`',
  'Expected l-value but `%s` found',
  'Unknown type: `%s`',
  'Unknown variable: `%s`',
  'Unknown function: `%s`',
  'Wrong type; got `%s` expected `%s`',
  'Unsupported binary operator: `%s` %s `%s`',
  'Unsupported unary operator: %s %s',
  'Division by zero',
  'Wrong number of parameters (in call to `%s`) - expected %d got %d',
  'Invalid types ''%s[%s]'' for array subscript',
  'Unknown include file: ''%s''',
  'Corrupted SSM file: ''%s''',
  'String exceeds line',
  'Unknown macro: ''%s''',
  'Function ''%s'' not found in library ''%s'' (wrong declaration?)',
  'Variable ''%s'' declared as void',
  'Parameter ''%s'' declared as void',

  '[Bytecode] Label not found: `%s`',
  '[Bytecode] Export (label) not found: `%s`',
  '[Bytecode] Invalid opcode: `%s`',
  '[Bytecode] String not found: `%s`'
 );

 Implementation

End.
