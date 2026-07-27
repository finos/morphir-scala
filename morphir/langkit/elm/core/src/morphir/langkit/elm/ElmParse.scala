package morphir.langkit.elm

import kyo.*
import kyo.kernel.ArrowEffect

import morphir.langkit.core.{Reported, Severity}
import morphir.langkit.elm.ast.Module
import morphir.langkit.elm.compiler.ParseDiagnostic
import morphir.langkit.elm.cst.{CstModule, CstTrivia}
import morphir.langkit.elm.parser.{
  CommentScanner,
  CstLowering,
  ModuleParser,
  OperatorReassociator,
  ParseDiagnosticErrorBuilder,
  TriviaAssociator,
  TupleArityChecker
}

/**
 * What an Elm parse stage asks of whoever is running it.
 *
 * The pipeline's own vocabulary rather than generic plumbing: read the options, say what you found, or give up. Lives
 * beside [[ElmParse]] rather than inside its companion because Kyo's `Tag` macro wants the effect's input type
 * constructor at the top level.
 */
enum ElmParseOp[+A] derives CanEqual:
  /** How to parse. Answered by the interpreter, so no stage has to be handed its configuration. */
  case Options extends ElmParseOp[ElmParseOptions]

  /** Something the stage described but could not resolve. It carries on; the interpreter decides the rest. */
  case Report(reported: Reported[ParseDiagnostic]) extends ElmParseOp[Unit]

  /** Nothing usable came out of this stage, so there is no continuation worth resuming. */
  case Halt(diagnostic: ParseDiagnostic) extends ElmParseOp[Nothing]

/**
 * The effect the Elm parse pipeline runs in.
 *
 * Parsing Elm is several passes — syntax, operator re-association, comment association, with layout and lexical checks
 * to come — and they share three needs: the options in force, somewhere to put what they found, and a way to give up.
 * Those are the effect's three operations, so a stage's signature is `CstModule < ElmParse` and nothing more: no
 * options parameter, no diagnostic accumulator, no early-return plumbing.
 *
 * Whether a report is fatal, whether reporting one stops the pipeline, and where the options come from are the
 * *interpreter's* business. [[ElmParse.run]] is the interpreter this module ships — it collects everything reported and
 * withholds the tree if any of it was an error — but the shape admits others without touching a stage. An editor could
 * resume a `Report` with a placeholder node; a linter could keep the tree regardless of severity.
 *
 * That freedom is the point of suspending rather than returning: a stage hands its request to the interpreter, and the
 * interpreter decides whether, and with what, to resume it.
 *
 * The name is deliberately Elm's. A general Morphir parse and compile pipeline is coming, and this is to be its Elm
 * instance rather than its definition — so `Parse` and `Compile` stay free for the general concept, and the pieces that
 * are already language-neutral live in [[morphir.langkit.core.Reported]] and [[morphir.langkit.core.Severity]].
 */
sealed trait ElmParse extends ArrowEffect[ElmParseOp, Id]

object ElmParse:

  /** A diagnostic and the weight the stage that found it gave it. */
  type Report = Reported[ParseDiagnostic]

  /** What a handled pipeline produced: everything it reported, and the value if it survived. */
  final case class Outcome[+A](diagnostics: Chunk[Report], value: Option[A]) derives CanEqual:
    def isSuccess: Boolean               = value.isDefined
    def errors: Chunk[Report]            = diagnostics.filter(_.isError)
    def messages: Chunk[ParseDiagnostic] = diagnostics.map(_.diagnostic)

  // -----------------------------------------------------------------------
  // Operations
  // -----------------------------------------------------------------------

  /** The options this pipeline must obey. */
  def options(using Frame): ElmParseOptions < ElmParse =
    ArrowEffect.suspend[ElmParseOptions](Tag[ElmParse], ElmParseOp.Options)

  /** Report a diagnostic and carry on, so one problem does not hide the next. */
  def report(diagnostic: ParseDiagnostic, severity: Severity)(using Frame): Unit < ElmParse =
    ArrowEffect.suspend[Unit](Tag[ElmParse], ElmParseOp.Report(Reported(diagnostic, severity)))

  /** Report several diagnostics and carry on. */
  def reportAll(reported: Seq[Report])(using Frame): Unit < ElmParse =
    Kyo.foreachDiscard(reported)(r => ArrowEffect.suspend[Unit](Tag[ElmParse], ElmParseOp.Report(r)))

  /** Give up: this stage produced nothing the next one could work with. */
  def halt(diagnostic: ParseDiagnostic)(using Frame): Nothing < ElmParse =
    ArrowEffect.suspend[Nothing](Tag[ElmParse], ElmParseOp.Halt(diagnostic))

  /** Give up if `result` failed, otherwise carry on with its value. */
  def fromResult[A](result: parsley.Result[ParseDiagnostic, A])(using Frame): A < ElmParse =
    result.fold(halt, value => value)

  // -----------------------------------------------------------------------
  // Stages
  // -----------------------------------------------------------------------

  /** Parse `source` into a CST: syntax, then operator re-association, then comment association. */
  def cst(source: String)(using Frame): CstModule < ElmParse =
    for
      parsed <- syntax(source)
      withComments = CstModule(
        parsed.moduleDecl,
        parsed.imports,
        parsed.declarations,
        CstTrivia(CommentScanner.scan(source).toIndexedSeq)
      )(parsed.span)
      shaped <- reassociate(withComments, source)
      _      <- checkTupleArity(shaped, source)
    yield TriviaAssociator.associate(shaped)

  /** Parse `source` into an AST, by lowering the CST. */
  def ast(source: String)(using Frame): Module < ElmParse =
    cst(source).map(CstLowering.lowerModule)

  /**
   * The syntax stage: text to a CST, with no interpretation of what it means.
   *
   * Its failure halts rather than reports — a file that does not parse leaves the later stages nothing to work on.
   */
  def syntax(source: String)(using Frame): CstModule < ElmParse =
    val errorBuilder = ParseDiagnosticErrorBuilder(source)
    fromResult(ModuleParser.module.parse[ParseDiagnostic](source)(using errorBuilder))

  /**
   * The operator stage: shape every chain by the fixities in scope, reporting what it could not resolve.
   *
   * The rewrite itself is an ordinary pure function over the tree — suspending inside a recursive tree walk would buy
   * nothing, since it makes no requests until it is finished. It hands back what it found, and this stage is where
   * those findings become the pipeline's business.
   */
  def reassociate(module: CstModule, source: String)(using Frame): CstModule < ElmParse =
    options.map { opts =>
      val (shaped, reported) = OperatorReassociator.reassociate(module, source, opts)
      reportAll(reported).andThen(shaped)
    }

  /**
   * The tuple stage: Elm accepts tuples of two or three entries and no more.
   *
   * Nothing about the grammar rules a longer one out, so — as in `elm/compiler`, which catches this during
   * canonicalisation — it is checked over the finished tree. Reporting rather than halting means a module says which of
   * its tuples are too long, not just the first.
   */
  def checkTupleArity(module: CstModule, source: String)(using Frame): Unit < ElmParse =
    options.map(opts => reportAll(TupleArityChecker.check(module, source, opts.tupleArity)))

  // -----------------------------------------------------------------------
  // Interpretation
  // -----------------------------------------------------------------------

  /**
   * Interpret a pipeline: answer its questions from `options`, collect everything it reports, and withhold the value if
   * any report was an error.
   *
   * A `Report` resumes the stage — that is what lets a module with four unresolvable operator chains describe all four
   * instead of only the first. A `Halt` does not: its continuation is dropped, because a stage that halted has nothing
   * to hand on.
   */
  def run[A](options: ElmParseOptions)(pipeline: A < ElmParse)(using Frame): Outcome[A] =
    ArrowEffect.handleLoop(Tag[ElmParse], Chunk.empty[Report], pipeline)(
      handle = [C] =>
        (input, state, cont) =>
          input match
            case ElmParseOp.Options =>
              Loop.continue(state, cont(options.asInstanceOf[C]))
            case ElmParseOp.Report(reported) =>
              Loop.continue(state.append(reported), cont(().asInstanceOf[C]))
            case ElmParseOp.Halt(diagnostic) =>
              Loop.done(Outcome[A](state.append(Reported.error(diagnostic)), None)),
      done = (state, value) =>
        if state.exists(_.isError) then Outcome(state, None)
        else Outcome(state, Some(value))
    ).eval
