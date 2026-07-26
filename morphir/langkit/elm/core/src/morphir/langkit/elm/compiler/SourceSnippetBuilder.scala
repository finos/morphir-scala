package morphir.langkit.elm.compiler

final case class SourceSnippet(
    contextLines: List[DiagnosticContextLine],
    rendered: String
) derives CanEqual

/** Builds multi-line source snippets with aligned gutters and a caret on the error line. */
object SourceSnippetBuilder:

  val DefaultLinesBefore: Int = 2
  val DefaultLinesAfter: Int  = 1

  def build(
      source: String,
      errorLine: Int,
      column: Int,
      errorWidth: Int,
      linesBefore: Int = DefaultLinesBefore,
      linesAfter: Int = DefaultLinesAfter
  ): SourceSnippet =
    val lines        = sourceLines(source)
    val totalLines   = lines.length.max(1)
    val boundedLine  = errorLine.max(1).min(totalLines)
    val startLine    = (boundedLine - linesBefore).max(1)
    val endLine      = (boundedLine + linesAfter).min(totalLines)
    val gutterWidth  = endLine.toString.length
    val contextLines = (startLine to endLine).toList.map { lineNumber =>
      DiagnosticContextLine(
        line = lineNumber,
        text = lines.lift(lineNumber - 1).getOrElse(""),
        isErrorLine = lineNumber == boundedLine
      )
    }
    val renderedLines =
      contextLines.map(line => s"${paddedLineNumber(line.line, gutterWidth)}| ${line.text}")
    val caret =
      val gutterPrefix = paddedLineNumber(boundedLine, gutterWidth) + "| "
      val caretStart   = gutterPrefix.length + (column - 1).max(0)
      val caretWidth   = if errorWidth <= 0 then 1 else errorWidth
      " " * caretStart + ("^" * caretWidth)
    SourceSnippet(
      contextLines = contextLines,
      rendered = (renderedLines :+ caret).mkString("\n")
    )

  private def sourceLines(source: String): Vector[String] =
    if source.isEmpty then Vector("")
    else if source.endsWith("\n") then source.linesIterator.toVector :+ ""
    else source.linesIterator.toVector

  private def paddedLineNumber(line: Int, width: Int): String =
    line.toString.reverse.padTo(width, ' ').reverse
