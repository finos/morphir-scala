package morphir.langkit.elm.parser

import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.elm.cst.*

/**
 * Where Elm's grammar cares whether two tokens touch.
 *
 * `a.b` is field access and `a . b` is an error; `f -1` applies `f` to `-1` while `f - 1` subtracts. None of these can
 * be decided by a parser whose tokens swallow their own trailing whitespace, which is why the expression atoms stop at
 * their last character and consume whitespace at one explicit boundary.
 */
class TokenAdjacencySpec extends Test[Any]:

  private def parses(declaration: String): Boolean =
    Elm.parseCst(s"module M exposing (..)\n\n$declaration\n").isSuccess

  private def body(declaration: String): CstExpression =
    Elm.parseCst(s"module M exposing (..)\n\n$declaration\n").fold(
      diagnostic => throw new AssertionError(s"parse failed: $diagnostic"),
      _.declarations.head match
        case d: CstValueDeclaration => d.body
        case other                  => throw new AssertionError(s"unexpected declaration: $other")
    )

  "field access" - {
    "attaches when the dot is adjacent" in {
      body("main = rec.field") match
        case CstFieldAccess(_, field) => assert(field.value == "field")
        case other                    => throw new AssertionError(s"expected field access, got: $other")
    }

    "chains" in {
      body("main = rec.inner.field") match
        case CstFieldAccess(CstFieldAccess(_, inner), field) =>
          assert(inner.value == "inner" && field.value == "field")
        case other => throw new AssertionError(s"expected chained field access, got: $other")
    }

    "reaches inside a parenthesised expression" in {
      body("main = (f x).field") match
        case CstFieldAccess(_: CstParenthesized, field) => assert(field.value == "field")
        case other => throw new AssertionError(s"expected field access on parentheses, got: $other")
    }

    "is rejected when the dot is spaced" in {
      assert(!parses("main = rec . field"))
      assert(!parses("main = rec. field"))
    }

    "`rec .field` is an application of the accessor function, not field access" in {
      // `.field` is a function in Elm, so this applies `rec` to it — a type error later, not a parse error.
      body("main = rec .field") match
        case CstFunctionApplication(_, List(_: CstFieldAccessFunction)) => assert(true)
        case other => throw new AssertionError(s"expected an application of `.field`, got: $other")
    }

    "the `.field` function still stands alone" in {
      body("main = .field") match
        case CstFieldAccessFunction(field) => assert(field.value == "field")
        case other                         => throw new AssertionError(s"expected a field access function, got: $other")
    }

  }

  "qualified names" - {
    "attach when the dots are adjacent" in {
      body("main = List.map") match
        case CstVariableRef(qualified) => assert(qualified.parts.map(_.value) == List("List", "map"))
        case other                     => throw new AssertionError(s"expected a qualified variable, got: $other")
    }

    "are rejected when a dot is spaced" in
      assert(!parses("main = List . map"))

  }

  "negation and subtraction" - {
    "`-x` at the start of a term negates" in {
      body("main = -x") match
        case _: CstNegate => assert(true)
        case other        => throw new AssertionError(s"expected negation, got: $other")
    }

    "`- x` is not negation" in
      assert(!parses("main = - x"))

    "`a - b` subtracts" in {
      body("main = a - b") match
        case CstBinaryOp(_, op, _) => assert(op.value == "-")
        case other                 => throw new AssertionError(s"expected subtraction, got: $other")
    }

    "`a-b` subtracts" in {
      body("main = a-b") match
        case CstBinaryOp(_, op, _) => assert(op.value == "-")
        case other                 => throw new AssertionError(s"expected subtraction, got: $other")
    }

    "`f -1` applies `f` to a negative term" in {
      body("main = f -1") match
        case CstFunctionApplication(_, List(_: CstNegate)) => assert(true)
        case other => throw new AssertionError(s"expected application of a negative term, got: $other")
    }

    "`f x -1` takes the negative term as a second argument" in {
      body("main = f x -1") match
        case CstFunctionApplication(_, List(_, _: CstNegate)) => assert(true)
        case other => throw new AssertionError(s"expected two arguments, got: $other")
    }

    "`a - -1` subtracts a negative term" in {
      body("main = a - -1") match
        case CstBinaryOp(_, op, _: CstNegate) => assert(op.value == "-")
        case other => throw new AssertionError(s"expected subtraction of a negation, got: $other")
    }

  }

  "application" - {
    "requires its arguments to be separated by whitespace" in {
      body("main = f x y") match
        case CstFunctionApplication(_, args) => assert(args.size == 2)
        case other                           => throw new AssertionError(s"expected an application, got: $other")
    }

    "still accepts an argument on a following indented line" in {
      body("main =\n    f\n        x") match
        case CstFunctionApplication(_, args) => assert(args.size == 1)
        case other                           => throw new AssertionError(s"expected an application, got: $other")
    }

  }
