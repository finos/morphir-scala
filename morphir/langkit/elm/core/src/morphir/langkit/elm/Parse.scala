package morphir.langkit.elm

import kyo.*
import kyo.kernel.ArrowEffect

import morphir.langkit.elm.compiler.ParseDiagnostic

/** How much a diagnostic matters, which is a question of the options in force rather than of the diagnostic itself. */
enum Severity derives CanEqual:
  /** The parse cannot stand: whatever tree came out is a guess the caller asked not to be given. */
  case Error

  /** Worth saying, but the caller asked for a tree anyway — `ElmParseOptions.lenient` and its like. */
  case Advisory

/** A diagnostic together with the weight the reporting stage gave it. */
final case class Reported(diagnostic: ParseDiagnostic, severity: Severity) derives CanEqual:
  def isError: Boolean = severity == Severity.Error

/**
 * What a parse stage asks of whoever is running it.
 *
 * These are the pipeline's own vocabulary rather than generic plumbing: a stage reads the options it must obey, says
 * what it found, or gives up. Everything else it does itself.
 */
enum ParseOp[+A] derives CanEqual:
  /** How to parse. Answered by the interpreter, so no stage has to be handed its configuration. */
  case Options extends ParseOp[ElmParseOptions]

  /** Something the stage could describe but not resolve. It carries on afterwards; the interpreter decides the rest. */
  case Report(reported: Reported) extends ParseOp[Unit]

  /** Nothing usable came out of this stage, so there is no continuation worth resuming. */
  case Halt(diagnostic: ParseDiagnostic) extends ParseOp[Nothing]

/**
 * The effect an Elm parse pipeline runs in.
 *
 * A stage's type says `CstModule < Parse` and nothing more: which diagnostics are fatal, whether reporting one stops
 * the pipeline, and where the options come from are all the interpreter's business. [[Parse.run]] is the interpreter
 * this codebase ships — it collects everything reported and withholds the tree if any of it was an error — but the
 * shape admits others without touching a stage. An editor could resume from a `Report` with a placeholder node; a
 * linter could downgrade every `Error` to an `Advisory` and keep the tree.
 *
 * That freedom is the point of suspending rather than returning: a stage hands its request to the interpreter and the
 * interpreter decides whether, and with what, to resume.
 */
sealed trait Parse extends ArrowEffect[ParseOp, Id]

object Parse:

  /** What a handled pipeline produced: everything it reported, and the value if it survived. */
  final case class Outcome[+A](diagnostics: Chunk[Reported], value: Option[A]) derives CanEqual:
    def isSuccess: Boolean               = value.isDefined
    def errors: Chunk[Reported]          = diagnostics.filter(_.isError)
    def messages: Chunk[ParseDiagnostic] = diagnostics.map(_.diagnostic)

  /** The options this pipeline must obey. */
  def options(using Frame): ElmParseOptions < Parse =
    ArrowEffect.suspend[ElmParseOptions](Tag[Parse], ParseOp.Options)

  /** Report a diagnostic and carry on, so one problem does not hide the next. */
  def report(diagnostic: ParseDiagnostic, severity: Severity)(using Frame): Unit < Parse =
    ArrowEffect.suspend[Unit](Tag[Parse], ParseOp.Report(Reported(diagnostic, severity)))

  /** Report several diagnostics and carry on. */
  def reportAll(reported: Seq[Reported])(using Frame): Unit < Parse =
    Kyo.foreachDiscard(reported)(r => ArrowEffect.suspend[Unit](Tag[Parse], ParseOp.Report(r)))

  /** Give up: this stage produced nothing the next one could work with. */
  def halt(diagnostic: ParseDiagnostic)(using Frame): Nothing < Parse =
    ArrowEffect.suspend[Nothing](Tag[Parse], ParseOp.Halt(diagnostic))

  /** Give up if `result` failed, otherwise carry on with its value. */
  def fromResult[A](result: parsley.Result[ParseDiagnostic, A])(using Frame): A < Parse =
    result.fold(halt, value => value)

  /**
   * Interpret a pipeline: answer its questions from `options`, collect everything it reports, and withhold the value if
   * any report was an error.
   *
   * A `Report` resumes the stage — that is what lets a module with four unresolvable operator chains describe all four
   * instead of only the first. A `Halt` does not: its continuation is dropped, because a stage that halted has nothing
   * to hand on.
   */
  def run[A](options: ElmParseOptions)(pipeline: A < Parse)(using Frame): Outcome[A] =
    ArrowEffect.handleLoop(Tag[Parse], Chunk.empty[Reported], pipeline)(
      handle = [C] =>
        (input, state, cont) =>
          input match
            case ParseOp.Options =>
              Loop.continue(state, cont(options.asInstanceOf[C]))
            case ParseOp.Report(reported) =>
              Loop.continue(state.append(reported), cont(().asInstanceOf[C]))
            case ParseOp.Halt(diagnostic) =>
              Loop.done(Outcome[A](state.append(Reported(diagnostic, Severity.Error)), None)),
      done = (state, value) =>
        if state.exists(_.isError) then Outcome(state, None)
        else Outcome(state, Some(value))
    ).eval
