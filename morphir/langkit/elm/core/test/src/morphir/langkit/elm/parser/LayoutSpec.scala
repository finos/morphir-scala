package morphir.langkit.elm.parser

import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.elm.cst.*

/**
 * Elm's layout rules, which are grammar rather than formatting.
 *
 * A top-level declaration begins in column 1, and the items of a `let` or `case` block line up with each other. Both
 * facts are load-bearing: they are how the parser knows where the previous expression ended, so getting them wrong does
 * not produce a formatting complaint, it silently attaches one declaration's tail to the next.
 */
class LayoutSpec extends Test[Any]:

  private def parse(body: String): Option[CstModule] =
    Elm.parseCst(s"module M exposing (..)\n\n$body\n").toOption

  private def declarations(body: String): IndexedSeq[CstDeclaration] =
    parse(body).getOrElse(throw new AssertionError(s"parse failed:\n$body")).declarations

  private def valueBody(body: String, name: String): CstExpression =
    declarations(body).collectFirst { case d: CstValueDeclaration if d.name.value == name => d.body }
      .getOrElse(throw new AssertionError(s"no declaration named $name in:\n$body"))

  "top-level declarations" - {
    "begin in column 1" in
      assert(declarations("main = 1\n\nother = 2").size == 2)

    "are rejected when indented, and said so" in {
      val diagnostic = Elm.parseCst("module M exposing (..)\n\n  main = 1\n").fold(
        identity,
        m => throw new AssertionError(s"expected a failure, parsed: ${m.declarations}")
      )
      assert(diagnostic.message.contains("a top-level declaration has to start in column 1"))
    }

    "end where the next one begins, however far the expression ran" in {
      val decls = declarations("main =\n    f\n        x\n\nother = 2")
      assert(decls.size == 2)
      valueBody("main =\n    f\n        x\n\nother = 2", "main") match
        case CstFunctionApplication(_, args) => assert(args.size == 1)
        case other                           => throw new AssertionError(s"expected an application, got: $other")
    }

    "an operator chain continued on the next line stays with its declaration" in {
      val decls = declarations("main =\n    1\n        + 2\n\nother = 3")
      assert(decls.size == 2)
    }
  }

  "let blocks" - {
    "take every binding that lines up" in {
      valueBody("main =\n    let\n        x = 1\n        y = 2\n    in\n    x", "main") match
        case n: CstLetIn => assert(n.bindings.size == 2)
        case other       => throw new AssertionError(s"expected a let/in, got: $other")
    }

    "are rejected when a binding does not line up" in
      assert(parse("main =\n    let\n        x = 1\n          y = 2\n    in\n    x").isEmpty)

    "end at the `in`, which need not line up with the bindings" in {
      valueBody("main =\n    let\n        x = 1\n    in\n    x", "main") match
        case n: CstLetIn => assert(n.bindings.size == 1)
        case other       => throw new AssertionError(s"expected a let/in, got: $other")
    }

    "allow a binding's body to run onto indented lines" in {
      valueBody(
        "main =\n    let\n        x =\n            f\n                y\n        z = 2\n    in\n    x",
        "main"
      ) match
        case n: CstLetIn => assert(n.bindings.size == 2)
        case other       => throw new AssertionError(s"expected a let/in, got: $other")
    }
  }

  "case blocks" - {
    "take every branch that lines up" in {
      valueBody("main =\n    case xs of\n        A -> 1\n        B -> 2\n        _ -> 3", "main") match
        case n: CstCaseOf => assert(n.branches.size == 3)
        case other        => throw new AssertionError(s"expected a case/of, got: $other")
    }

    "are rejected when a branch does not line up" in
      assert(parse("main =\n    case xs of\n        A -> 1\n          B -> 2").isEmpty)

    "do not swallow the declaration that follows them" in {
      val body  = "main =\n    case xs of\n        A -> 1\n        B -> 2\n\nother = 3"
      val decls = declarations(body)
      assert(decls.size == 2)
      valueBody(body, "main") match
        case n: CstCaseOf => assert(n.branches.size == 2)
        case other        => throw new AssertionError(s"expected a case/of, got: $other")
    }

    "nest, each block aligning on its own column" in {
      val body =
        "main =\n    case xs of\n        A ->\n            case ys of\n                C -> 1\n                D -> 2\n\n        B -> 3"
      valueBody(body, "main") match
        case n: CstCaseOf =>
          assert(n.branches.size == 2)
          n.branches.head.body match
            case inner: CstCaseOf => assert(inner.branches.size == 2)
            case other            => throw new AssertionError(s"expected a nested case, got: $other")
        case other => throw new AssertionError(s"expected a case/of, got: $other")
    }

    "sit inside a let binding without the blocks interfering" in {
      val body =
        "main =\n    let\n        y =\n            case xs of\n                A -> 1\n                B -> 2\n\n        z = 3\n    in\n    y"
      valueBody(body, "main") match
        case n: CstLetIn =>
          assert(n.bindings.size == 2)
          n.bindings.head.body match
            case inner: CstCaseOf => assert(inner.branches.size == 2)
            case other            => throw new AssertionError(s"expected a case in the binding, got: $other")
        case other => throw new AssertionError(s"expected a let/in, got: $other")
    }
  }
