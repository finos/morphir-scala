package morphir.langkit.elm.compiler

import morphir.langkit.elm.Elm
import morphir.langkit.elm.ast.Module as AstModule
import morphir.langkit.elm.cst.CstModule
import morphir.langkit.trees.QueryableTree
import morphir.langkit.trees.query.Matcher
import morphir.langkit.trees.query.Query
import morphir.langkit.trees.query.QueryLogic
import morphir.langkit.trees.query.QueryParser
import morphir.langkit.trees.query.QueryPretty

/**
 * Default implementation of [[CompilerComponent]] that wraps the pure [[morphir.langkit.elm.Elm]] parser APIs inside
 * Kyo-backed [[QueryLogic.QueryEffect]] values. Every parse/query failure becomes a structured [[CompileError]]
 * surfaced through the result envelope via [[QueryLogic.failFast]], never an exception.
 */
object ElmCompiler:

  lazy val defaultCompiler: CompilerComponent[Unit] = compiler[Unit]

  /**
   * A [[CompilerComponent]] for any caller-chosen context. The default implementation does not read or write the
   * context itself — it is threaded through for composition with the caller's own stateful effects.
   */
  def compiler[Ctx]: CompilerComponent[Ctx] = new CompilerComponent[Ctx]:
    import CompilerComponent.CompileEff

    def parseCst(source: String): CompileEff[Ctx, CstModule] =
      Elm.parseCst(source) match
        case parsley.Success(m)                           => m
        case parsley.Failure(diagnostic: ParseDiagnostic) =>
          QueryLogic.failFast[Ctx, String, CompileError](
            CompileError.ParseError(phase = "cst", diagnostic = diagnostic)
          )

    def parseAst(source: String): CompileEff[Ctx, AstModule] =
      Elm.parseAst(source) match
        case parsley.Success(m)                           => m
        case parsley.Failure(diagnostic: ParseDiagnostic) =>
          QueryLogic.failFast[Ctx, String, CompileError](
            CompileError.ParseError(phase = "ast", diagnostic = diagnostic)
          )

    def parseQuery(q: String): CompileEff[Ctx, Query] =
      QueryParser.parse(q) match
        case parsley.Success(query) => query
        case parsley.Failure(msg)   =>
          QueryLogic.failFast[Ctx, String, CompileError](
            CompileError.QueryError(message = msg.toString)
          )

    def runQuery[T](q: Query, root: T)(using QueryableTree[T]): CompileEff[Ctx, List[MatchView]] =
      Matcher.matches(q, root).map(MatchView.from(_)).toList

    def prettyQuery(q: Query): String = QueryPretty.render(q)
