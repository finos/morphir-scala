package morphir.langkit.elm.compiler

/**
 * Structured compilation diagnostics emitted by Krueger compiler and tooling APIs.
 *
 * Cases carry enough context (phase, diagnostic payload) so downstream UIs can render actionable errors without
 * re-parsing messages.
 */
sealed trait CompileError derives CanEqual:
  def message: String

object CompileError:

  /** Failure while parsing Elm source into a CST or AST. */
  final case class ParseError(phase: String, diagnostic: ParseDiagnostic) extends CompileError:
    def message: String = diagnostic.message

    def span: Option[Span] = Some(diagnostic.toCompilerSpan)

    def code: DiagnosticCode = diagnostic.code

    def expected: List[String] = diagnostic.expected

    def suggestion: Option[String] = diagnostic.suggestion

  /** Failure while parsing or evaluating a query. */
  final case class QueryError(message: String, span: Option[Span] = None) extends CompileError

  /** Unexpected internal failure (bug in the compiler surface). */
  final case class InternalError(message: String) extends CompileError

/** Source span `[start, end)` in 0-based character offsets. */
final case class Span(start: Int, end: Int) derives CanEqual
