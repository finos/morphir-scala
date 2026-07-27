package morphir.langkit.elm.compiler.abi

import scala.util.control.NonFatal

import morphir.langkit.elm.ast.AstNode
import morphir.langkit.elm.compiler.CompileError
import morphir.langkit.elm.compiler.CompilerComponent
import morphir.langkit.elm.compiler.ElmCompiler
import morphir.langkit.elm.compiler.MatchView
import morphir.langkit.elm.cst.CstNode
import morphir.langkit.trees.query.QueryLogic

object InvokeCompiler:

  import InvokeJson.decode
  import InvokeJson.encode
  import InvokeJson.given

  private val compiler = ElmCompiler.defaultCompiler

  def invoke(op: String, inputJson: String): String =
    encode(dispatch(op, inputJson))

  // Matched exhaustively over `InvokeOp` rather than with a wildcard, so declaring a new operation fails to compile
  // until it is dispatched. The wildcard this replaces silently routed four of the five declared operations to
  // "unknown operation".
  private def dispatch(op: String, inputJson: String): InvokeResponse =
    try
      InvokeOp.fromWireName(op) match
        case Some(InvokeOp.ParseCst) =>
          val request = decode[SourceRequest](inputJson)
          val result  = CompilerComponent.runUnit(compiler.parseCst(request.source))
          InvokeResponse.fromCompileResult(result, _.toString)

        case Some(InvokeOp.ParseAst) =>
          val request = decode[SourceRequest](inputJson)
          val result  = CompilerComponent.runUnit(compiler.parseAst(request.source))
          InvokeResponse.fromCompileResult(result, _.toString)

        case Some(InvokeOp.ParseQuery) =>
          val request = decode[QueryRequest](inputJson)
          val result  = CompilerComponent.runUnit(compiler.parseQuery(request.query))
          InvokeResponse.fromCompileResult(result, _.toString)

        case Some(InvokeOp.PrettyQuery) =>
          val request = decode[QueryRequest](inputJson)
          val result  =
            CompilerComponent.runUnit(compiler.parseQuery(request.query).map(compiler.prettyQuery))
          InvokeResponse.fromCompileResult(result, identity)

        case Some(InvokeOp.RunQuery) =>
          val request = decode[RunQueryRequest](inputJson)
          val result  = CompilerComponent.runUnit(runQuery(request))
          InvokeResponse.fromCompileResult(result, _.toString)

        case None =>
          unknownOperation(op)
    catch
      case NonFatal(error) =>
        InvokeResponse.failure(
          errors = Vector(
            InvokeError(
              phase = "internal",
              message = messageOf(error)
            )
          )
        )

  /** Parse `source` into the requested tree, then run the query against it. */
  private def runQuery(request: RunQueryRequest): CompilerComponent.CompileEff[Unit, List[MatchView]] =
    compiler.parseQuery(request.query).map { query =>
      request.treeKind match
        // Annotated at the node type: the QueryableTree instances are defined for CstNode / AstNode, and
        // QueryableTree is invariant, so inference from the module subtype does not find them.
        case "cst" =>
          import morphir.langkit.elm.cst.CstQueryableTree.given
          compiler.parseCst(request.source).map(root => compiler.runQuery[CstNode](query, root))
        case "ast" =>
          import morphir.langkit.elm.ast.AstQueryableTree.given
          compiler.parseAst(request.source).map(root => compiler.runQuery[AstNode](query, root))
        case other =>
          QueryLogic.failFast[Unit, String, CompileError](
            CompileError.QueryError(message = s"""unknown treeKind: "$other"; expected "cst" or "ast"""")
          )
    }

  private def unknownOperation(op: String): InvokeResponse =
    InvokeResponse.failure(
      errors = Vector(
        InvokeError(
          phase = "internal",
          message = s"unknown operation: $op"
        )
      )
    )

  private def messageOf(error: Throwable): String =
    Option(error.getMessage)
      .map(_.trim)
      .filter(_.nonEmpty)
      .getOrElse(error.getClass.getSimpleName)
