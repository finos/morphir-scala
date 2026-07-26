package morphir.langkit.itest

import parsley.{Failure, Result, Success}

import scala.io.Source

import morphir.langkit.elm.Krueger
import morphir.langkit.elm.ast.AstNode
import morphir.langkit.elm.ast.AstQueryableTree.given
import morphir.langkit.elm.ast.Module
import morphir.langkit.elm.compiler.ParseDiagnostic
import morphir.langkit.elm.compiler.abi.InvokeCompiler
import morphir.langkit.elm.cst.CstModule
import morphir.langkit.elm.cst.CstNode
import morphir.langkit.elm.cst.CstQueryableTree.given
import morphir.langkit.trees.QueryableTree
import morphir.langkit.trees.query.*

object TestDriver:

  /**
   * Backends the compiler-api feature exercises.
   *
   * Krueger also carried a `scalajs-node` backend that shelled out to Node against a linked Scala.js facade. That
   * facade has no counterpart here yet — it arrives with the kyo-ui based Morphir web page — so the backend and its
   * `-Dmorphir.langkit.webapp.facade.dir` plumbing are left out until then.
   */
  private val supportedBackends = Set("jvm", "chicory")

  def requireSupportedBackend(backend: String): Unit =
    if !supportedBackends.contains(backend) then
      throw AssertionError(
        s"unsupported compiler backend [$backend]; expected ${supportedBackends.toVector.sorted.mkString(", ")}"
      )

  def invoke(backend: String, op: String, inputJson: String): String =
    requireSupportedBackend(backend)
    backend match
      case "jvm"     => InvokeCompiler.invoke(op, inputJson)
      case "chicory" => ChicorySupportedCompilerHarness.invoke(op, inputJson)

/** Scenario-scoped mutable state shared across step-definition classes via the cucumber-scala DI container. */
final class TestDriver:
  private var source: String                                        = ""
  private var cstResult: Option[Result[ParseDiagnostic, CstModule]] = None
  private var astResult: Option[Result[ParseDiagnostic, Module]]    = None
  private var lastMatchesBuf: Vector[MatchView]                     = Vector.empty
  private var querySource: Option[String]                           = None
  private var canonicalQueryText: Option[String]                    = None

  def setSource(raw: String): Unit =
    source = raw
    cstResult = None
    astResult = None
    lastMatchesBuf = Vector.empty
    querySource = None
    canonicalQueryText = None

  def setSourceFromResource(resourcePath: String): Unit =
    val stream = Option(getClass.getClassLoader.getResourceAsStream(resourcePath)) match
      case Some(stream) => stream
      case None         => throw new AssertionError(s"fixture resource not found: $resourcePath")

    try setSource(Source.fromInputStream(stream, "UTF-8").mkString)
    finally stream.close()

  def parseCst(): Unit = cstResult = Some(Krueger.parseCst(source))
  def parseAst(): Unit = astResult = Some(Krueger.parseAst(source))

  def cst: CstModule = cstResult match
    case Some(Success(m))                           => m
    case Some(Failure(diagnostic: ParseDiagnostic)) =>
      throw new AssertionError(s"CST parse failed: ${diagnostic.message}\nSource:\n$source")
    case None => throw new AssertionError("CST not parsed — missing When step?")

  def ast: Module = astResult match
    case Some(Success(m))                           => m
    case Some(Failure(diagnostic: ParseDiagnostic)) =>
      throw new AssertionError(s"AST parse failed: ${diagnostic.message}\nSource:\n$source")
    case None => throw new AssertionError("AST not parsed — missing When step?")

  /** Parse the CST (if not already parsed), run `queryText` against it, and store the matches. */
  def queryCst(queryText: String): Unit =
    if cstResult.isEmpty then parseCst()
    lastMatchesBuf = runQuery[CstNode](queryText, cst)

  /** Parse the AST (if not already parsed), run `queryText` against it, and store the matches. */
  def queryAst(queryText: String): Unit =
    if astResult.isEmpty then parseAst()
    lastMatchesBuf = runQuery[AstNode](queryText, ast)

  /** Matches collected by the most recent `queryCst` / `queryAst`. */
  def lastMatches: Vector[MatchView] = lastMatchesBuf

  /** Store a raw query string for later canonicalization via [[canonicalizeQuerySource]]. */
  def setQuerySource(queryText: String): Unit =
    querySource = Some(queryText)
    canonicalQueryText = None

  /**
   * Parse the stored query source and compute its canonical S-expression form via [[QueryPretty]].
   *
   * Throws an `AssertionError` if the query fails to parse. The canonical text is then available via
   * [[canonicalQuery]].
   */
  def canonicalizeQuerySource(): Unit =
    val raw   = querySource.getOrElse(throw new AssertionError("query source not set — missing Given step?"))
    val query = QueryParser.parse(raw) match
      case Success(q)   => q
      case Failure(msg) =>
        throw new AssertionError(s"query parse failed: $msg\nQuery: $raw")
    canonicalQueryText = Some(QueryPretty.render(query))

  /** The canonical S-expression text produced by the most recent [[canonicalizeQuerySource]] call. */
  def canonicalQuery: String =
    canonicalQueryText.getOrElse(throw new AssertionError("canonical query not set — missing When step?"))

  /**
   * Assert that [[canonicalQuery]] can itself be parsed successfully.
   *
   * Throws an `AssertionError` if the canonical form does not round-trip.
   */
  def canonicalQueryReparses: Unit =
    val canonical = canonicalQuery
    QueryParser.parse(canonical) match
      case Success(_)   => ()
      case Failure(msg) =>
        throw new AssertionError(s"canonical query failed to parse: $msg\nQuery:\n$canonical")

  private def runQuery[T](queryText: String, root: T)(using qt: QueryableTree[T]): Vector[MatchView] =
    val query = QueryParser.parse(queryText) match
      case Success(q)   => q
      case Failure(msg) =>
        throw new AssertionError(s"query parse failed: $msg\nQuery: $queryText")
    Matcher.matches(query, root).map(MatchView.from(_)).toVector
