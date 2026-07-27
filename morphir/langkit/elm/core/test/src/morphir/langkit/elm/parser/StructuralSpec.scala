package morphir.langkit.elm.parser

import kyo.test.*

import morphir.langkit.elm.{Elm, ElmParseOptions}
import morphir.langkit.elm.compiler.DiagnosticCode
import morphir.langkit.elm.cst.*

/**
 * Constructs that are structure rather than expression: shader blocks, effect module headers, and the limits Elm puts
 * on a tuple.
 *
 * Each of these was a row in the gap ledger of `morphir/langkit/elm/conformance.html`, pinned by an assertion that the
 * construct did *not* parse. Closing them turned those assertions red, which is what the pins are for; the assertions
 * below are what replaced them.
 */
class StructuralSpec extends Test[Any]:

  private def module(source: String): CstModule =
    Elm.parseCst(source).fold(
      diagnostic => throw new AssertionError(s"parse failed: $diagnostic"),
      identity
    )

  private def declaration(body: String): CstDeclaration =
    module(s"module M exposing (..)\n\n$body\n").declarations.head

  private def body(text: String): CstExpression =
    declaration(text) match
      case d: CstValueDeclaration => d.body
      case other                  => throw new AssertionError(s"unexpected declaration: $other")

  "GLSL blocks" - {
    "parse, with their contents verbatim" in {
      body("shader = [glsl| void main() {} |]") match
        case n: CstGlsl => assert(n.code == " void main() {} ")
        case other      => throw new AssertionError(s"expected a GLSL block, got: $other")
    }

    "run to the closing delimiter, taking line breaks and lone bars on the way" in {
      body("shader = [glsl|\nattribute vec3 position;\nfloat f = a | b;\n|]") match
        case n: CstGlsl => assert(n.code.contains("attribute vec3 position;") && n.code.contains("a | b;"))
        case other      => throw new AssertionError(s"expected a GLSL block, got: $other")
    }
  }

  "effect modules" - {
    "carry a `where` clause naming both managers" in {
      val declared = module(
        "effect module M where { command = MyCmd, subscription = MySub } exposing (..)\n\nx = 1\n"
      ).moduleDecl
      assert(declared.moduleType == ModuleType.Effect)
      assert(declared.manager.flatMap(_.command).map(_.value).contains("MyCmd"))
      assert(declared.manager.flatMap(_.subscription).map(_.value).contains("MySub"))
    }

    "take either manager alone" in {
      val commandOnly = module("effect module M where { command = MyCmd } exposing (..)\n\nx = 1\n").moduleDecl
      assert(commandOnly.manager.flatMap(_.command).map(_.value).contains("MyCmd"))
      assert(commandOnly.manager.flatMap(_.subscription).isEmpty)

      val subscriptionOnly =
        module("effect module M where { subscription = MySub } exposing (..)\n\nx = 1\n").moduleDecl
      assert(subscriptionOnly.manager.flatMap(_.subscription).map(_.value).contains("MySub"))
    }

    "take the two in either order" in {
      val reversed = module(
        "effect module M where { subscription = MySub, command = MyCmd } exposing (..)\n\nx = 1\n"
      ).moduleDecl
      assert(reversed.manager.flatMap(_.command).map(_.value).contains("MyCmd"))
      assert(reversed.manager.flatMap(_.subscription).map(_.value).contains("MySub"))
    }

    "reject a key that is neither" in
      assert(Elm.parseCst("effect module M where { manager = MyThing } exposing (..)\n\nx = 1\n").isFailure)

    "leave a plain module without one" in
      assert(module("module M exposing (..)\n\nx = 1\n").moduleDecl.manager.isEmpty)
  }

  "tuple arity" - {
    "accepts two and three entries" in {
      assert(Elm.parseCst("module M exposing (..)\n\npair = ( 1, 2 )\n").isSuccess)
      assert(Elm.parseCst("module M exposing (..)\n\ntriple = ( 1, 2, 3 )\n").isSuccess)
    }

    "rejects four, as Elm does" in {
      val diagnostic = Elm.parseCst("module M exposing (..)\n\nquad = ( 1, 2, 3, 4 )\n").fold(
        identity,
        m => throw new AssertionError(s"expected a failure, parsed: ${m.declarations}")
      )
      assert(DiagnosticCode.unwrap(diagnostic.code) == "ELM-P006")
      assert(diagnostic.message.contains("I only accept tuples with two or three items. This has too many:"))
      assert(diagnostic.message.contains("This one has 4."))
    }

    "applies to types and patterns too" in {
      assert(Elm.parseCst("module M exposing (..)\n\ntype alias T = ( Int, Int, Int, Int )\n").isFailure)
      assert(
        Elm.parseCst("module M exposing (..)\n\nf t =\n    case t of\n        ( a, b, c, d ) ->\n            a\n")
          .isFailure
      )
    }

    "reports every oversized tuple, not just the first" in {
      val outcome = Elm.diagnoseCst(
        "module M exposing (..)\n\nfirst = ( 1, 2, 3, 4 )\n\nsecond = ( 1, 2, 3, 4, 5 )\n"
      )
      assert(outcome.messages.size == 2)
      assert(outcome.messages.forall(d => DiagnosticCode.unwrap(d.code) == "ELM-P006"))
      assert(outcome.value.isEmpty)
    }

    "is accepted, with the tuple kept, when the caller asks" in {
      val outcome = Elm.diagnoseCst("module M exposing (..)\n\nquad = ( 1, 2, 3, 4 )\n", ElmParseOptions.lenient)
      assert(outcome.errors.isEmpty)
      assert(outcome.isSuccess)
    }
  }
