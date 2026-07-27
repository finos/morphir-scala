package morphir.langkit.elm.compiler

import morphir.langkit.elm.ast.Module as AstModule
import morphir.langkit.elm.cst.CstModule
import morphir.langkit.trees.QueryableTree
import morphir.langkit.trees.query.Query
import morphir.langkit.trees.query.QueryLogic

/**
 * A framework- and platform-agnostic compiler surface for the Elm langkit, generic on the caller's context type `Ctx`.
 *
 * All effectful operations return a Kyo-backed [[QueryLogic.QueryEffect]] so consumers can compose the compiler
 * alongside their own stateful effects and run everything at the edge of a UI or FFI boundary, receiving a plain
 * [[QueryLogic.Result]] envelope (context, logs, errors, value). The effect type itself never crosses the JS / WASM FFI
 * boundary.
 *
 * @tparam Ctx
 *   the caller-chosen context type threaded through the compile effect. Use `Unit` when no context is needed.
 */
trait CompilerComponent[Ctx]:
  import CompilerComponent.CompileEff

  def parseCst(source: String): CompileEff[Ctx, CstModule]
  def parseAst(source: String): CompileEff[Ctx, AstModule]
  def parseQuery(q: String): CompileEff[Ctx, Query]
  def runQuery[T](q: Query, root: T)(using QueryableTree[T]): CompileEff[Ctx, List[MatchView]]
  def prettyQuery(q: Query): String

object CompilerComponent:

  /** Default log entry type — string for v1, can move to a structured ADT later. */
  type CompileLog = String

  /** Structured error type shared by every phase. */
  type CompileErr = CompileError

  /** The effects every compile operation may use, for implementations that need to name them. */
  type CompileEffects[Ctx] = QueryLogic.QueryEffects[Ctx, CompileLog, CompileErr]

  /** Convenience alias so UI code reads as `CompileEff[Ctx, Query]` rather than a long generic application. */
  type CompileEff[Ctx, A] = QueryLogic.QueryEffect[Ctx, CompileLog, CompileErr, A]

  /** Result envelope returned by [[run]]. */
  type CompileResult[Ctx, A] = QueryLogic.Result[Ctx, CompileLog, CompileErr, A]

  /** Run an effect with an explicit initial context. Call at the edge of a UI or FFI boundary. */
  inline def run[Ctx, A](initialContext: Ctx)(eff: CompileEff[Ctx, A]): CompileResult[Ctx, A] =
    QueryLogic.run[Ctx, CompileLog, CompileErr, A](initialContext)(eff)

  /** Convenience runner for the common case where no caller context is needed. */
  inline def runUnit[A](eff: CompileEff[Unit, A]): CompileResult[Unit, A] =
    run[Unit, A](())(eff)
