package morphir.langkit.elm

import kyo.test.*

import morphir.langkit.elm.compiler.DiagnosticCode
import morphir.langkit.elm.cst.*

/**
 * Parsing modules that know about each other.
 *
 * An operator's fixity is declared in one module and used in another, which is why a single module cannot always be
 * shaped correctly on its own. These assert on tree *shape*, since the failure mode is a tree that groups the wrong way
 * rather than a parse that fails.
 */
class ElmProjectSpec extends Test[Any]:

  private val declaringModule =
    """module Combine exposing (..)
      |
      |infix right 5 (<%>) = combine
      |
      |combine : Int -> Int -> Int
      |combine a b =
      |    a + b
      |""".stripMargin

  private def usingModule(body: String) =
    s"""module Use exposing (..)
       |
       |import Combine exposing (..)
       |
       |main =
       |    $body
       |""".stripMargin

  private def show(expr: CstExpression): String = expr match
    case n: CstBinaryOp    => s"(${show(n.left)} ${n.operator.value} ${show(n.right)})"
    case n: CstVariableRef => n.name.parts.map(_.value).mkString(".")
    case n: CstIntLiteral  => n.value.toString
    case other             => other.getClass.getSimpleName

  private def mainBody(outcome: ElmProject.Outcome): CstExpression =
    outcome.trees.getOrElse("Use", throw new AssertionError(s"no Use module: ${outcome.modules.keys}"))
      .declarations.collectFirst { case d: CstValueDeclaration if d.name.value == "main" => d.body }
      .getOrElse(throw new AssertionError("no main declaration"))

  "an operator declared in another module of the project" - {
    "is resolved, with its declared associativity" in {
      val outcome = ElmProject.parse(
        Map("Combine.elm" -> declaringModule, "Use.elm" -> usingModule("a <%> b <%> c"))
      )
      assert(outcome.isSuccess)
      assert(show(mainBody(outcome)) == "(a <%> (b <%> c))")
    }

    "is resolved with its declared precedence against a built-in" in {
      val outcome = ElmProject.parse(
        Map("Combine.elm" -> declaringModule, "Use.elm" -> usingModule("a + b <%> c"))
      )
      // `+` is left-associative at 6 and `<%>` right-associative at 5, so `+` binds tighter and takes its operands
      // first — which is only decidable once `<%>`'s declaration has been found in the other module.
      assert(show(mainBody(outcome)) == "((a + b) <%> c)")
    }

    "is still unknown to a module that does not import it" in {
      val standalone =
        """module Alone exposing (..)
          |
          |main =
          |    a <%> b
          |""".stripMargin

      val outcome = ElmProject.parse(Map("Combine.elm" -> declaringModule, "Alone.elm" -> standalone))
      val alone   = outcome.modules("Alone")
      assert(!alone.isSuccess)
      assert(alone.messages.exists(d => DiagnosticCode.unwrap(d.code) == "ELM-P005"))
    }

    "is still unknown when the declaring module is not in the project" in {
      val outcome = ElmProject.parse(Map("Use.elm" -> usingModule("a <%> b")))
      assert(!outcome.modules("Use").isSuccess)
    }
  }

  "a project" - {
    "parses modules that declare nothing unusual" in {
      val outcome = ElmProject.parse(
        Map(
          "A.elm" -> "module A exposing (..)\n\nx = 1 + 2 * 3\n",
          "B.elm" -> "module B exposing (..)\n\nimport A\n\ny = A.x\n"
        )
      )
      assert(outcome.isSuccess)
      assert(outcome.trees.keySet == Set("A", "B"))
    }

    "keeps a module that does not parse out of resolution, and says which it was" in {
      val outcome = ElmProject.parse(
        Map(
          "Combine.elm" -> declaringModule,
          "Broken.elm"  -> "module Broken exposing (..)\n\nx = = =\n",
          "Use.elm"     -> usingModule("a <%> b")
        )
      )
      assert(outcome.unparsed.keySet == Set("Broken.elm"))
      assert(!outcome.isSuccess)
      assert(outcome.trees.contains("Use"))
    }

    "reports a chain conflict per module, not for the project as a whole" in {
      val outcome = ElmProject.parse(
        Map(
          "Good.elm" -> "module Good exposing (..)\n\nx = 1 + 2\n",
          "Bad.elm"  -> "module Bad exposing (..)\n\nx = a == b == c\n"
        )
      )
      assert(outcome.modules("Good").isSuccess)
      assert(!outcome.modules("Bad").isSuccess)
      assert(outcome.modules("Bad").messages.exists(d => DiagnosticCode.unwrap(d.code) == "ELM-P004"))
    }
  }
