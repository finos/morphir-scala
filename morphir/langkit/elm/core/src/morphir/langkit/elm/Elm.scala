package morphir.langkit.elm

import kyo.Frame

import morphir.langkit.elm.ast.Module
import morphir.langkit.elm.compiler.ParseDiagnostic
import morphir.langkit.elm.cst.CstModule

/**
 * Public API entry point for the Elm dialect parser.
 *
 * Every entry point takes an [[ElmParseOptions]], defaulted to `ElmParseOptions.elm`: what `elm/compiler` accepts, and
 * nothing else. Pass `ElmParseOptions.lenient` for best-effort trees, or a tailored one to supply the fixities of
 * operators declared in dependencies.
 *
 * These are the pipeline in [[ElmParse]] with its effects already handled. `parseCst` and `parseAst` report the first
 * problem and stop; `diagnoseCst` and `diagnoseAst` report everything the pipeline found. Compose [[ElmParse]] itself
 * to run parsing alongside your own effects.
 */
object Elm:

  /** Parse Elm source code into a CST, or the first diagnostic that stopped it. */
  def parseCst(
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): parsley.Result[ParseDiagnostic, CstModule] =
    firstDiagnostic(diagnoseCst(source, options))

  /** Parse Elm source code into an AST (CST lowered), or the first diagnostic that stopped it. */
  def parseAst(
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): parsley.Result[ParseDiagnostic, Module] =
    firstDiagnostic(diagnoseAst(source, options))

  /**
   * Parse Elm source code into a CST, reporting every diagnostic rather than the first.
   *
   * A file with several unresolvable operator chains describes all of them. Under `ElmParseOptions.lenient` the
   * diagnostics say what had to be guessed, and the tree is still returned.
   */
  def diagnoseCst(
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): Parse.Outcome[CstModule] =
    Parse.run(options)(ElmParse.cst(source))

  /** Parse Elm source code into an AST, reporting every diagnostic rather than the first. */
  def diagnoseAst(
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): Parse.Outcome[Module] =
    Parse.run(options)(ElmParse.ast(source))

  private def firstDiagnostic[A](outcome: Parse.Outcome[A]): parsley.Result[ParseDiagnostic, A] =
    outcome.value match
      case Some(value) => parsley.Success(value)
      case None        =>
        val reported = outcome.errors.headMaybe.orElse(outcome.diagnostics.headMaybe).getOrElse(
          throw new IllegalStateException("the parse failed without reporting a diagnostic")
        )
        parsley.Failure(reported.diagnostic)
