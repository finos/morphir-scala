package morphir.langkit.elm

import kyo.*

import morphir.langkit.elm.ast.Module
import morphir.langkit.elm.compiler.ParseDiagnostic
import morphir.langkit.elm.cst.{CstModule, CstTrivia}
import morphir.langkit.elm.parser.{
  CommentScanner,
  CstLowering,
  ModuleParser,
  OperatorReassociator,
  ParseDiagnosticErrorBuilder,
  TriviaAssociator
}

/**
 * The Elm parse pipeline, staged in the [[Parse]] effect.
 *
 * [[Elm]] is the plain façade over this — the same stages with the effect already interpreted — for callers who want a
 * tree or a diagnostic and nothing else. Compose the stages here to run parsing alongside your own effects, or to
 * interpret it differently than [[Parse.run]] does.
 *
 * Note what the stages do *not* carry: no options parameter, no diagnostic accumulator, no early-return plumbing. A
 * stage asks for what it needs and says what it found.
 */
object ElmParse:

  /** Parse `source` into a CST: syntax, then operator re-association, then comment association. */
  def cst(source: String)(using Frame): CstModule < Parse =
    for
      parsed <- syntax(source)
      withComments = CstModule(
        parsed.moduleDecl,
        parsed.imports,
        parsed.declarations,
        CstTrivia(CommentScanner.scan(source).toIndexedSeq)
      )(parsed.span)
      shaped <- reassociate(withComments, source)
    yield TriviaAssociator.associate(shaped)

  /** Parse `source` into an AST, by lowering the CST. */
  def ast(source: String)(using Frame): Module < Parse =
    cst(source).map(CstLowering.lowerModule)

  /**
   * The syntax stage: text to a CST, with no interpretation of what it means.
   *
   * Its failure halts rather than reports — a file that does not parse leaves the later stages nothing to work on.
   */
  def syntax(source: String)(using Frame): CstModule < Parse =
    val errorBuilder = ParseDiagnosticErrorBuilder(source)
    Parse.fromResult(ModuleParser.module.parse[ParseDiagnostic](source)(using errorBuilder))

  /**
   * The operator stage: shape every chain by the fixities in scope, reporting what it could not resolve.
   *
   * The rewrite itself is an ordinary pure function over the tree — suspending inside a recursive tree walk would buy
   * nothing, since it makes no requests until it is finished. It hands back what it found, and this stage is where
   * those findings become the pipeline's business.
   */
  def reassociate(module: CstModule, source: String)(using Frame): CstModule < Parse =
    Parse.options.map { options =>
      val (shaped, reported) = OperatorReassociator.reassociate(module, source, options)
      Parse.reportAll(reported).andThen(shaped)
    }
