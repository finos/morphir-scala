package morphir.langkit.elm

import morphir.langkit.elm.compiler.ParseDiagnostic
import morphir.langkit.elm.cst.{CstModule, CstTrivia}
import morphir.langkit.elm.ast.Module
import morphir.langkit.elm.parser.{
  CommentScanner,
  CstLowering,
  ModuleParser,
  OperatorReassociator,
  ParseDiagnosticErrorBuilder,
  TriviaAssociator
}

/**
 * Public API entry point for the Elm dialect parser.
 *
 * Both entry points take an [[ElmParseOptions]], defaulted to `ElmParseOptions.elm`: what `elm/compiler` accepts, and
 * nothing else. Pass `ElmParseOptions.lenient` for best-effort trees, or a tailored one to supply the fixities of
 * operators declared in dependencies.
 */
object Elm:

  /** Parse Elm source code into a CST. */
  def parseCst(
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): parsley.Result[ParseDiagnostic, CstModule] =
    val errorBuilder = ParseDiagnosticErrorBuilder(source)
    ModuleParser.module.parse[ParseDiagnostic](source)(using errorBuilder).flatMap { module =>
      val withComments = CstModule(
        module.moduleDecl,
        module.imports,
        module.declarations,
        CstTrivia(CommentScanner.scan(source).toIndexedSeq)
      )(module.span)
      OperatorReassociator.reassociate(withComments, source, options) match
        case Left(diagnostic)    => parsley.Failure(diagnostic)
        case Right(reassociated) => parsley.Success(TriviaAssociator.associate(reassociated))
    }

  /** Parse Elm source code into an AST (CST lowered). */
  def parseAst(
      source: String,
      options: ElmParseOptions = ElmParseOptions.elm
  ): parsley.Result[ParseDiagnostic, Module] =
    parseCst(source, options).map(CstLowering.lowerModule)
