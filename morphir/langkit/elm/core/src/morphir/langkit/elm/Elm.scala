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

/** Public API entry point for the Elm dialect parser. */
object Elm:

  /** Parse Elm source code into a CST. */
  def parseCst(source: String): parsley.Result[ParseDiagnostic, CstModule] =
    val errorBuilder = ParseDiagnosticErrorBuilder(source)
    ModuleParser.module.parse[ParseDiagnostic](source)(using errorBuilder).map { module =>
      val withComments = CstModule(
        module.moduleDecl,
        module.imports,
        module.declarations,
        CstTrivia(CommentScanner.scan(source).toIndexedSeq)
      )(module.span)
      TriviaAssociator.associate(OperatorReassociator.reassociate(withComments))
    }

  /** Parse Elm source code into an AST (CST lowered). */
  def parseAst(source: String): parsley.Result[ParseDiagnostic, Module] =
    parseCst(source).map(CstLowering.lowerModule)
