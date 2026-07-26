package morphir.langkit.elm.compiler

/** Formats structured parse diagnostics into human-friendly, Elm-inspired messages. */
object DiagnosticMessageFormatter:

  final case class FormattedDiagnostic(
      message: String,
      contextLines: List[DiagnosticContextLine]
  ) derives CanEqual

  def format(
      source: String,
      code: DiagnosticCode,
      line: Int,
      column: Int,
      unexpected: Option[String],
      expected: List[String],
      reasons: Seq[String],
      suggestion: Option[String],
      errorWidth: Int
  ): FormattedDiagnostic =
    val header = formatHeader(code, line, column)
    val body   =
      List(
        unexpectedExplanation(code, unexpected),
        expectedExplanation(expected),
        reasonsExplanation(reasons)
      ).filter(_.nonEmpty).mkString("\n\n")
    val snippet = SourceSnippetBuilder.build(
      source = source,
      errorLine = line,
      column = column,
      errorWidth = errorWidth
    )
    val hint = suggestion.map(s => s"\n\nHint: $s").getOrElse("")
    FormattedDiagnostic(
      message = List(header, body, snippet.rendered).filter(_.nonEmpty).mkString("\n\n") + hint,
      contextLines = snippet.contextLines
    )

  private def formatHeader(code: DiagnosticCode, line: Int, column: Int): String =
    s"-- ${DiagnosticCode.kindLabel(code)} (${DiagnosticCode.unwrap(code)}) at line $line, column $column"

  private def unexpectedExplanation(code: DiagnosticCode, unexpected: Option[String]): String =
    unexpected match
      case Some("end of input") =>
        "I ran into the end of the file unexpectedly."
      case Some(value) if DiagnosticCode.isTokenizer(code) =>
        s"I ran into an unexpected character:\n\n    $value"
      case Some(token) =>
        s"I ran into an unexpected token:\n\n    $token"
      case None =>
        "I ran into something I did not expect here."

  private def expectedExplanation(expected: List[String]): String =
    if expected.isEmpty then ""
    else
      val items = expected.map(item => s"    $item").mkString("\n")
      s"I was expecting one of the following:\n\n$items"

  private def reasonsExplanation(reasons: Seq[String]): String =
    reasons.filter(_.nonEmpty) match
      case Nil   => ""
      case lines => lines.mkString("\n")
