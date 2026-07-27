package morphir.langkit.elm.compiler

import morphir.langkit.core.{DiagnosticContextLine, SourceOffsets, SourceSpan, Span}

final case class ParseDiagnostic(
    code: DiagnosticCode,
    span: SourceSpan,
    message: String,
    expected: List[String],
    suggestion: Option[String] = None,
    contextLines: List[DiagnosticContextLine] = Nil
) derives CanEqual:

  def toSpan: Span = span.range

object ParseDiagnostic:

  def unexpectedEndOfInput(source: String, line: Int, column: Int, expected: List[String]): ParseDiagnostic =
    val start     = SourceOffsets.offsetAt(source, line, column)
    val formatted = DiagnosticMessageFormatter.format(
      source = source,
      code = DiagnosticCode.UnexpectedEndOfInput,
      line = line,
      column = column,
      unexpected = Some("end of input"),
      expected = expected,
      reasons = Nil,
      suggestion = None,
      errorWidth = 0
    )
    ParseDiagnostic(
      code = DiagnosticCode.UnexpectedEndOfInput,
      span = SourceSpan.fromStartEnd(start = start, end = start, line = line, column = column),
      message = formatted.message,
      expected = expected,
      contextLines = formatted.contextLines
    )

  def unexpectedToken(
      source: String,
      line: Int,
      column: Int,
      width: Int,
      unexpected: String,
      expected: List[String]
  ): ParseDiagnostic =
    val start     = SourceOffsets.offsetAt(source, line, column)
    val end       = (start + width.max(1)).min(source.length.max(start + 1))
    val formatted = DiagnosticMessageFormatter.format(
      source = source,
      code = DiagnosticCode.UnexpectedToken,
      line = line,
      column = column,
      unexpected = Some(unexpected),
      expected = expected,
      reasons = Nil,
      suggestion = None,
      errorWidth = width
    )
    ParseDiagnostic(
      code = DiagnosticCode.UnexpectedToken,
      span = SourceSpan.fromStartEnd(start = start, end = end, line = line, column = column),
      message = formatted.message,
      expected = expected,
      contextLines = formatted.contextLines
    )

  def tokenizerUnexpectedCharacter(source: String, offset: Int, lexeme: String): ParseDiagnostic =
    val (line, column) = SourceOffsets.lineColumnAt(source, offset)
    val formatted      = DiagnosticMessageFormatter.format(
      source = source,
      code = DiagnosticCode.TokenizerUnexpectedCharacter,
      line = line,
      column = column,
      unexpected = Some(lexeme),
      expected = Nil,
      reasons = Nil,
      suggestion = None,
      errorWidth = lexeme.length
    )
    ParseDiagnostic(
      code = DiagnosticCode.TokenizerUnexpectedCharacter,
      span = SourceSpan.fromStartEnd(start = offset, end = offset + lexeme.length, line = line, column = column),
      message = formatted.message,
      expected = Nil,
      contextLines = formatted.contextLines
    )
