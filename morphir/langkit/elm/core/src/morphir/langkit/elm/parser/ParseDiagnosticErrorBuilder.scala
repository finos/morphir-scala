package morphir.langkit.elm.parser

import morphir.langkit.core.{SourceOffsets, SourceSpan}

import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.SingleChar

import morphir.langkit.elm.compiler.*

sealed trait DiagnosticBody derives CanEqual

object DiagnosticBody:

  final case class Vanilla(
      unexpected: Option[String],
      expected: Set[String],
      reasons: Seq[String],
      errorWidth: Int
  ) extends DiagnosticBody

  final case class Specialised(messages: Seq[String], errorWidth: Int) extends DiagnosticBody

/** Threads the active source through Parsley error construction so diagnostics can compute offsets. */
object ParseDiagnosticContext:

  private val activeSource = new ThreadLocal[String]

  def withSource[A](source: String)(thunk: => A): A =
    activeSource.set(source)
    try thunk
    finally activeSource.remove()

  private[parser] def source: String =
    val value = activeSource.get()
    if value == null then "" else value

/** Builds structured [[ParseDiagnostic]] values from Parsley failures for a single parse invocation. */
final class ParseDiagnosticErrorBuilder(parseSource: String) extends ErrorBuilder[ParseDiagnostic] with SingleChar:

  type Position       = (Int, Int)
  type Source         = Unit
  type ErrorInfoLines = DiagnosticBody
  type Item           = String
  type Raw            = String
  type Named          = String
  type EndOfInput     = String
  type Message        = String
  type Messages       = Seq[Message]
  type ExpectedItems  = Set[Item]
  type ExpectedLine   = Set[Item]
  type UnexpectedLine = Option[Item]
  type LineInfo       = Int

  def format(pos: Position, source: Unit, body: ErrorInfoLines): ParseDiagnostic =
    val src                                                      = parseSource
    val (line, column)                                           = pos
    val start                                                    = SourceOffsets.offsetAt(src, line, column)
    val (code, unexpected, expected, reasons, suggestion, width) = body match
      case DiagnosticBody.Vanilla(unexp, exp, rsns, w) =>
        (
          classify(unexp),
          unexp,
          exp.toList.sorted,
          rsns,
          suggestionFor(unexp, exp, rsns),
          w
        )
      case DiagnosticBody.Specialised(msgs, w) =>
        (DiagnosticCode.SpecialisedParseFailure, None, Nil, msgs, None, w)
    val end =
      if unexpected.contains(endOfInput) then start
      else (start + width.max(1)).min(src.length.max(start + 1))
    val message =
      body match
        case DiagnosticBody.Vanilla(_, _, rsns, w) =>
          val formatted = DiagnosticMessageFormatter.format(
            source = src,
            code = code,
            line = line,
            column = column,
            unexpected = unexpected,
            expected = expected,
            reasons = rsns,
            suggestion = suggestion,
            errorWidth = w
          )
          formatted.message -> formatted.contextLines
        case DiagnosticBody.Specialised(msgs, w) =>
          val formatted = DiagnosticMessageFormatter.format(
            source = src,
            code = code,
            line = line,
            column = column,
            unexpected = None,
            expected = Nil,
            reasons = msgs,
            suggestion = suggestion,
            errorWidth = w
          )
          formatted.message -> formatted.contextLines
    ParseDiagnostic(
      code = code,
      span = SourceSpan.fromStartEnd(start = start, end = end, line = line, column = column),
      message = message._1,
      expected = expected,
      suggestion = suggestion,
      contextLines = message._2
    )

  def vanillaError(
      unexpected: UnexpectedLine,
      expected: ExpectedLine,
      reasons: Messages,
      line: LineInfo
  ): ErrorInfoLines =
    DiagnosticBody.Vanilla(unexpected, expected, reasons, line)

  def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines =
    DiagnosticBody.Specialised(msgs, line)

  def pos(line: Int, col: Int): Position = (line, col)

  def source(sourceName: Option[String]): Source = ()

  def combineExpectedItems(alts: Set[Item]): ExpectedLine = alts

  def combineMessages(alts: Seq[Message]): Messages = alts

  def unexpected(item: Option[Item]): UnexpectedLine = item

  def expected(alts: ExpectedLine): ExpectedLine = alts

  def reason(reason: String): Message = reason

  def message(msg: String): Message = msg

  def raw(item: String): Raw = item

  def named(item: String): Named = item

  val endOfInput: EndOfInput = "end of input"

  val numLinesBefore: Int = 0
  val numLinesAfter: Int  = 0

  def lineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int,
      errorWidth: Int
  ): LineInfo = errorWidth

  private def classify(unexpected: Option[Item]): DiagnosticCode =
    unexpected match
      case Some(`endOfInput`) => DiagnosticCode.UnexpectedEndOfInput
      case Some(_)            => DiagnosticCode.UnexpectedToken
      case None               => DiagnosticCode.UnexpectedToken

  private def suggestionFor(
      unexpected: Option[Item],
      expected: Set[Item],
      reasons: Seq[String]
  ): Option[String] =
    reasons.headOption.orElse {
      if unexpected.contains(endOfInput) && expected.exists(_.contains("in")) then
        Some("Did you forget `in` after a `let` binding?")
      else None
    }

object ParseDiagnosticErrorBuilder:

  def apply(source: String): ParseDiagnosticErrorBuilder =
    new ParseDiagnosticErrorBuilder(source)
