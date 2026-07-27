package morphir.langkit.elm.compiler

import kyo.{Frame, Kyo, Tag, <}

import morphir.langkit.elm.{Elm, ElmParseOptions, Parse}
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

  lazy val defaultCompiler: CompilerComponent[Unit] = compiler[Unit]()

  /**
   * A [[CompilerComponent]] for any caller-chosen context. The default implementation does not read or write the
   * context itself — it is threaded through for composition with the caller's own stateful effects.
   *
   * `options` fixes how this compiler parses for its lifetime; it defaults to canonical Elm semantics. A caller that
   * resolves imports can hand in a fuller operator table here rather than at every call site.
   */
  def compiler[Ctx](options: ElmParseOptions = ElmParseOptions.elm)(using
      Tag[Ctx],
      Frame
  ): CompilerComponent[Ctx] =
    new CompilerComponent[Ctx]:
      import CompilerComponent.CompileEff

      def parseCst(source: String): CompileEff[Ctx, CstModule] =
        surface("cst", Elm.diagnoseCst(source, options))

      def parseAst(source: String): CompileEff[Ctx, AstModule] =
        surface("ast", Elm.diagnoseAst(source, options))

      /**
       * Move a parse outcome into the compile envelope, keeping every diagnostic rather than the first.
       *
       * The envelope carries errors alongside a value, so a parse that produced a tree despite reporting something —
       * lenient options, say — surfaces both. A parse that produced nothing records all but its last diagnostic and
       * then fails with that one, which is where `QueryLogic.run` appends it, so the envelope reads in source order.
       */
      private def surface[A](phase: String, outcome: Parse.Outcome[A]): CompileEff[Ctx, A] =
        val errors =
          outcome.diagnostics.toList.map(r => CompileError.ParseError(phase = phase, diagnostic = r.diagnostic))
        outcome.value match
          case Some(value) =>
            record(errors).andThen(value)
          case None =>
            record(errors.dropRight(1)).andThen {
              QueryLogic.failFast[Ctx, String, CompileError](
                errors.lastOption.getOrElse(
                  CompileError.QueryError(message = s"the $phase parse failed without reporting a diagnostic")
                )
              )
            }

      private def record(errors: List[CompileError]): Unit < CompilerComponent.CompileEffects[Ctx] =
        Kyo.foreachDiscard(errors)(QueryLogic.error[Ctx, String, CompileError](_))

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
