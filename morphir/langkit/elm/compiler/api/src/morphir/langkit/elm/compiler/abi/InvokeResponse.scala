package morphir.langkit.elm.compiler.abi

import morphir.langkit.elm.compiler.CompileError
import morphir.langkit.elm.compiler.CompilerComponent
import morphir.langkit.elm.compiler.DiagnosticContextLine
import morphir.langkit.elm.compiler.DiagnosticCode
import morphir.langkit.elm.compiler.Span

final case class InvokeSpan(start: Int, end: Int) derives CanEqual

object InvokeSpan:

  def fromCompilerSpan(span: Span): InvokeSpan =
    InvokeSpan(start = span.start, end = span.end)

final case class InvokeContextLine(
    line: Int,
    text: String,
    isErrorLine: Boolean
) derives CanEqual

object InvokeContextLine:

  def fromDiagnosticContextLine(line: DiagnosticContextLine): InvokeContextLine =
    InvokeContextLine(
      line = line.line,
      text = line.text,
      isErrorLine = line.isErrorLine
    )

final case class InvokeError(
    phase: String,
    message: String,
    span: Option[InvokeSpan] = None,
    code: Option[String] = None,
    expected: List[String] = Nil,
    suggestion: Option[String] = None,
    line: Option[Int] = None,
    column: Option[Int] = None,
    contextLines: List[InvokeContextLine] = Nil
) derives CanEqual

object InvokeError:

  def fromCompileError(error: CompileError): InvokeError =
    error match
      case CompileError.ParseError(phase, diagnostic) =>
        InvokeError(
          phase = phase,
          message = diagnostic.message,
          span = Some(InvokeSpan.fromCompilerSpan(diagnostic.toCompilerSpan)),
          code = Some(DiagnosticCode.unwrap(diagnostic.code)),
          expected = diagnostic.expected,
          suggestion = diagnostic.suggestion,
          line = Some(diagnostic.span.line),
          column = Some(diagnostic.span.column),
          contextLines = diagnostic.contextLines.map(InvokeContextLine.fromDiagnosticContextLine)
        )
      case CompileError.QueryError(message, span) =>
        InvokeError(
          phase = "query",
          message = message,
          span = span.map(InvokeSpan.fromCompilerSpan)
        )
      case CompileError.InternalError(message) =>
        InvokeError(
          phase = "internal",
          message = message
        )

final case class InvokeResponse(
    ok: Boolean,
    value: Option[String],
    logs: Vector[String],
    errors: Vector[InvokeError]
) derives CanEqual

object InvokeResponse:

  def success(value: String, logs: Vector[String] = Vector.empty): InvokeResponse =
    InvokeResponse(
      ok = true,
      value = Some(value),
      logs = logs,
      errors = Vector.empty
    )

  def failure(errors: Vector[InvokeError], logs: Vector[String] = Vector.empty): InvokeResponse =
    InvokeResponse(
      ok = false,
      value = None,
      logs = logs,
      errors = errors
    )

  def fromCompileResult[A](
      result: CompilerComponent.CompileResult[Unit, A],
      renderValue: A => String
  ): InvokeResponse =
    result.value match
      case Right(value) =>
        success(
          value = renderValue(value),
          logs = result.logs
        )
      case Left(_) =>
        failure(
          errors = result.errors.map(InvokeError.fromCompileError),
          logs = result.logs
        )
