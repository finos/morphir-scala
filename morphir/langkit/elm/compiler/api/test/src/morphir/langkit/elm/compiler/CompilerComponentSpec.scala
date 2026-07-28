package morphir.langkit.elm.compiler

import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.elm.cst.CstNode
import morphir.langkit.elm.cst.CstQueryableTree.given

class CompilerComponentSpec extends Test[Any]:

  final private case class Snapshot[A](
      logs: Vector[String],
      errors: Vector[CompileError],
      value: Either[Vector[CompileError], A]
  ) derives CanEqual

  private def run[A](eff: CompilerComponent.CompileEff[Unit, A]): CompilerComponent.CompileResult[Unit, A] =
    CompilerComponent.runUnit(eff)

  private def snapshot[A](r: CompilerComponent.CompileResult[Unit, A]): Snapshot[A] =
    Snapshot(
      logs = r.logs,
      errors = r.errors,
      value = r.value
    )

  private def requireRight[A](either: Either[?, A], clue: String): A =
    either match
      case Right(value) => value
      case Left(_)      => throw new AssertionError(clue)

  private def expectedParseError(phase: String, source: String): CompileError.ParseError =
    Elm.parseCst(source) match
      case parsley.Failure(diagnostic: ParseDiagnostic) =>
        CompileError.ParseError(phase = phase, diagnostic = diagnostic)
      case parsley.Success(_) => throw new AssertionError(s"expected parse failure for: $source")

  private val compiler: CompilerComponent[Unit] = ElmCompiler.compiler[Unit]()

  private val simpleSource =
    """module M exposing (..)
      |
      |x = 1
      |""".stripMargin

  private val malformedSource = "module M exposing (..)\n\nx ="

  private val simpleQuery    = "(CstValueDeclaration) @v"
  private val malformedQuery = "(unbalanced"
  private val zeroMatchQuery = "(nonexistent_node_type) @x"

  private val expectedEmptyQueryMessage =
    List(
      "Query parse failed: (line 1, column 1):",
      "  unexpected end of input",
      "  expected \"(\", \";;\", \"[\", \"_\", or at least one query pattern",
      "  >",
      "   ^"
    ).mkString("\n")

  "CompilerComponent" - {
    "cross-platform fixture snapshots" - {
      "happy path: valid source and query produce the expected MatchView list" in {
        val cst = requireRight(
          run(compiler.parseCst(simpleSource)).value,
          "expected parseCst(simpleSource) to succeed"
        )
        val query = requireRight(
          run(compiler.parseQuery(simpleQuery)).value,
          "expected parseQuery(simpleQuery) to succeed"
        )
        val actual =
          snapshot(run(compiler.runQuery[CstNode](query, cst)))

        val expected = Snapshot(
          logs = Vector.empty,
          errors = Vector.empty,
          value = Right(
            List(
              MatchView(
                rootNodeType = "CstValueDeclaration",
                rootText = None,
                captures = Map(
                  "v" -> CapturedNode(
                    nodeType = "CstValueDeclaration",
                    text = None,
                    childCount = 2
                  )
                )
              )
            )
          )
        )

        assert(actual == expected)
      }
      "negative path: malformed source produces the expected ParseError envelope" in {
        val actual        = snapshot(run(compiler.parseCst(malformedSource)))
        val expectedError = expectedParseError(phase = "cst", source = malformedSource)
        val expected      = Snapshot(
          logs = Vector.empty,
          errors = Vector(expectedError),
          value = Left(Vector(expectedError))
        )

        assert(actual == expected)
      }
      "edge path: zero-match query returns an empty match list with no errors" in {
        val cst = requireRight(
          run(compiler.parseCst(simpleSource)).value,
          "expected parseCst(simpleSource) to succeed"
        )
        val query = requireRight(
          run(compiler.parseQuery(zeroMatchQuery)).value,
          "expected parseQuery(zeroMatchQuery) to succeed"
        )
        val actual =
          snapshot(run(compiler.runQuery[CstNode](query, cst)))
        val expected = Snapshot(
          logs = Vector.empty,
          errors = Vector.empty,
          value = Right(List.empty[MatchView])
        )

        assert(actual == expected)
      }
      "edge path: empty source returns the expected ParseError envelope" in {
        val actual        = snapshot(run(compiler.parseCst("")))
        val expectedError = expectedParseError(phase = "cst", source = "")
        val expected      = Snapshot(
          logs = Vector.empty,
          errors = Vector(expectedError),
          value = Left(Vector(expectedError))
        )

        assert(actual == expected)
      }
      "edge path: empty query returns the expected QueryError envelope" in {
        val actual        = snapshot(run(compiler.parseQuery("")))
        val expectedError = CompileError.QueryError(
          message = expectedEmptyQueryMessage
        )
        val expected = Snapshot(
          logs = Vector.empty,
          errors = Vector(expectedError),
          value = Left(Vector(expectedError))
        )

        assert(actual == expected)
      }
      "determinism: repeated runQuery calls produce identical snapshots" in {
        val cst = requireRight(
          run(compiler.parseCst(simpleSource)).value,
          "expected parseCst(simpleSource) to succeed"
        )
        val query = requireRight(
          run(compiler.parseQuery(simpleQuery)).value,
          "expected parseQuery(simpleQuery) to succeed"
        )
        val first  = snapshot(run(compiler.runQuery[CstNode](query, cst)))
        val second = snapshot(run(compiler.runQuery[CstNode](query, cst)))

        assert(first == second)
      }
    }
    "parseCst" - {
      "happy path: valid source produces a CST with no errors" in {
        val r = run(compiler.parseCst(simpleSource))
        assert(r.errors.isEmpty)
        assert(r.value.isRight)
      }
      "negative: malformed source surfaces a ParseError; no exception thrown" in {
        val r = run(compiler.parseCst(malformedSource))
        assert(r.errors.nonEmpty)
        assert(
          r.errors.forall {
            case _: CompileError.ParseError => true
            case _                          => false
          }
        )
        assert(r.value.isLeft)
      }
      "edge: empty source still returns a well-formed Result (no exception)" in {
        val r = run(compiler.parseCst(""))
        assert(r.logs != null)
        assert(r.errors != null)
      }
    }
    "parseAst" - {
      "happy path: valid source produces an AST with no errors" in {
        val r = run(compiler.parseAst(simpleSource))
        assert(r.errors.isEmpty)
        assert(r.value.isRight)
      }
      "negative: malformed source surfaces a ParseError" in {
        val r = run(compiler.parseAst(malformedSource))
        assert(r.errors.nonEmpty)
        assert(r.value.isLeft)
      }
    }
    "parseQuery" - {
      "happy path: valid query parses to Query" in {
        val r = run(compiler.parseQuery(simpleQuery))
        assert(r.errors.isEmpty)
        assert(r.value.isRight)
      }
      "negative: malformed query surfaces a QueryError" in {
        val r = run(compiler.parseQuery(malformedQuery))
        assert(r.errors.nonEmpty)
        assert(
          r.errors.forall {
            case _: CompileError.QueryError => true
            case _                          => false
          }
        )
        assert(r.value.isLeft)
      }
      "edge: empty query surfaces a QueryError (not a crash)" in {
        val r = run(compiler.parseQuery(""))
        assert(r.errors.nonEmpty)
        assert(r.value.isLeft)
      }
    }
    "runQuery" - {
      "happy path: valid query against parsed CST produces non-empty matches" in {
        val cstResult   = run(compiler.parseCst(simpleSource))
        val queryResult = run(compiler.parseQuery(simpleQuery))
        (cstResult.value, queryResult.value) match
          case (Right(cst), Right(q)) =>
            val r = run(compiler.runQuery[CstNode](q, cst))
            assert(r.errors.isEmpty)
            assert(r.value.toOption.exists(_.nonEmpty))
          case _ =>
            assert(false)
      }
      "edge: query that matches no nodes returns empty list, no errors" in {
        val cstResult   = run(compiler.parseCst(simpleSource))
        val queryResult = run(compiler.parseQuery(zeroMatchQuery))
        (cstResult.value, queryResult.value) match
          case (Right(cst), Right(q)) =>
            val r = run(compiler.runQuery[CstNode](q, cst))
            assert(r.errors.isEmpty)
            assert(r.value == Right(List.empty[MatchView]))
          case _ =>
            assert(false)
      }
      "determinism: repeated runQuery calls produce equal results" in {
        val cstResult   = run(compiler.parseCst(simpleSource))
        val queryResult = run(compiler.parseQuery(simpleQuery))
        (cstResult.value, queryResult.value) match
          case (Right(cst), Right(q)) =>
            val r1 = run(compiler.runQuery[CstNode](q, cst))
            val r2 = run(compiler.runQuery[CstNode](q, cst))
            assert(r1.value == r2.value)
          case _ =>
            assert(false)
      }
    }
    "prettyQuery" - {
      "pure: returns canonical string for a parsed query" in {
        val r = run(compiler.parseQuery(simpleQuery))
        r.value match
          case Right(q) =>
            val pretty = compiler.prettyQuery(q)
            assert(pretty.nonEmpty)
          case Left(_) =>
            assert(false)
      }
    }
  }
