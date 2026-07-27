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

  /**
   * Two operators sit side by side in a chain that cannot be grouped: they share a precedence, and at least one of them
   * refuses to give way. Elm reports this during canonicalisation; the wording follows it.
   */
  def conflictingOperators(source: String, span: Span, left: String, right: String): ParseDiagnostic =
    val (line, column) = SourceOffsets.lineColumnAt(source, span.offset)
    val formatted      = DiagnosticMessageFormatter.formatWithSummary(
      source = source,
      code = DiagnosticCode.ConflictingOperators,
      line = line,
      column = column,
      summary = s"You cannot mix ($left) and ($right) without parentheses.",
      expected = Nil,
      reasons = List("I do not know how to group these expressions."),
      suggestion = Some("Add parentheses to say which operator applies first."),
      errorWidth = span.length
    )
    ParseDiagnostic(
      code = DiagnosticCode.ConflictingOperators,
      span = SourceSpan.fromStartEnd(start = span.start, end = span.end, line = line, column = column),
      message = formatted.message,
      expected = Nil,
      suggestion = Some("Add parentheses to say which operator applies first."),
      contextLines = formatted.contextLines
    )

  /**
   * An operator whose fixity nothing in scope declares. The parser sees one module, so an operator declared in a
   * dependency is unknowable without that dependency's source.
   */
  def unknownOperator(source: String, span: Span, operator: String): ParseDiagnostic =
    val (line, column) = SourceOffsets.lineColumnAt(source, span.offset)
    val suggestion     =
      s"Declare it with an `infix` declaration in this module, or supply its fixity through ElmParseOptions."
    val formatted = DiagnosticMessageFormatter.formatWithSummary(
      source = source,
      code = DiagnosticCode.UnknownOperator,
      line = line,
      column = column,
      summary = s"I do not know the precedence or associativity of ($operator).",
      expected = Nil,
      reasons = List(
        "Without them I cannot tell how to group the expression around it."
      ),
      suggestion = Some(suggestion),
      errorWidth = span.length
    )
    ParseDiagnostic(
      code = DiagnosticCode.UnknownOperator,
      span = SourceSpan.fromStartEnd(start = span.start, end = span.end, line = line, column = column),
      message = formatted.message,
      expected = Nil,
      suggestion = Some(suggestion),
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
