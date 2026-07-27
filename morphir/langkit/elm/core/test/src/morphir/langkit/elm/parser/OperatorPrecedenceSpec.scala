package morphir.langkit.elm.parser

import kyo.test.*
import parsley.{Failure, Success}

import morphir.langkit.elm.Elm
import morphir.langkit.elm.ast
import morphir.langkit.elm.cst.*

/**
 * Tree-shape coverage for binary operator precedence and associativity.
 *
 * The parser builds a flat left-leaning chain and [[OperatorReassociator]] re-shapes it, so every assertion here is
 * about the shape of the resulting tree — a "does it parse" assertion passes either way.
 */
class OperatorPrecedenceSpec extends Test[Any]:

  private def parseModule(source: String): CstModule =
    Elm.parseCst(source) match
      case Success(m)          => m
      case Failure(diagnostic) => throw new AssertionError(s"parse failed: $diagnostic")

  private def parseBody(body: String, extraDeclarations: String = ""): CstExpression =
    val source = s"module M exposing (..)\n$extraDeclarations\nmain = $body\n"
    parseModule(source).declarations.collectFirst {
      case d: CstValueDeclaration if d.name.value == "main" =>
        d.body
    }.getOrElse(throw new AssertionError(s"no `main` declaration parsed from:\n$source"))

  /** Render a tree as fully parenthesised prefix-free text, so an assertion reads like the Elm it came from. */
  private def show(expr: CstExpression): String = expr match
    case n: CstBinaryOp      => s"(${show(n.left)} ${n.operator.value} ${show(n.right)})"
    case n: CstParenthesized => show(n.expr)
    case n: CstIntLiteral    => n.value.toString
    case n: CstVariableRef   => n.name.parts.map(_.value).mkString(".")
    case n: CstListLiteral   => n.elements.map(show).mkString("[", ", ", "]")
    case n: CstNegate        => s"-${show(n.expr)}"
    case other               => other.getClass.getSimpleName

  private def showAst(expr: ast.Expression): String = expr match
    case n: ast.BinaryOp    => s"(${showAst(n.left)} ${n.operator} ${showAst(n.right)})"
    case n: ast.IntLiteral  => n.value.toString
    case n: ast.VariableRef => n.name.fullName
    case other              => other.getClass.getSimpleName

  "precedence" - {
    "`*` binds tighter than `+`" in
      assert(show(parseBody("1 + 2 * 3")) == "(1 + (2 * 3))")

    "a tighter operator on the left still groups first" in
      assert(show(parseBody("1 * 2 + 3")) == "((1 * 2) + 3)")

    "the whole ladder groups by precedence" in
      // `||` 2, `&&` 3, `==` 4, `++` 5, `+` 6, `*` 7, `^` 8
      assert(show(parseBody("a || b && c == d + e * f")) == "(a || (b && (c == (d + (e * f)))))")

    "`|>` binds loosest" in
      assert(show(parseBody("a + b |> f")) == "((a + b) |> f)")

    "parentheses override precedence" in
      assert(show(parseBody("(1 + 2) * 3")) == "((1 + 2) * 3)")
  }

  "associativity" - {
    "left-associative operators at equal precedence associate left" in {
      assert(show(parseBody("1 - 2 - 3")) == "((1 - 2) - 3)")
      assert(show(parseBody("1 / 2 / 3")) == "((1 / 2) / 3)")
    }

    "`::` associates to the right" in
      assert(show(parseBody("a :: b :: rest")) == "(a :: (b :: rest))")

    "`++` associates to the right" in
      assert(show(parseBody("a ++ b ++ c")) == "(a ++ (b ++ c))")

    "`^` associates to the right" in
      assert(show(parseBody("2 ^ 3 ^ 4")) == "(2 ^ (3 ^ 4))")

    "`|>` associates left and `<|` associates right" in {
      assert(show(parseBody("a |> f |> g")) == "((a |> f) |> g)")
      assert(show(parseBody("f <| g <| a")) == "(f <| (g <| a))")
    }

    "`<<` associates left and `>>` associates right" in {
      assert(show(parseBody("f << g << h")) == "((f << g) << h)")
      assert(show(parseBody("f >> g >> h")) == "(f >> (g >> h))")
    }

    "a single operator is unchanged" in
      assert(show(parseBody("1 + 2")) == "(1 + 2)")

    "an expression with no operator is left alone" in
      assert(show(parseBody("42")) == "42")
  }

  "user infix declarations" - {
    "a declared right-associative operator associates right" in {
      val declaration = "\ninfix right 5 (<%>) = combine\n"
      assert(show(parseBody("a <%> b <%> c", declaration)) == "(a <%> (b <%> c))")
    }

    "a declared left-associative operator associates left" in {
      val declaration = "\ninfix left 5 (<%>) = combine\n"
      assert(show(parseBody("a <%> b <%> c", declaration)) == "((a <%> b) <%> c)")
    }

    "a declared precedence outranks a built-in one" in {
      val declaration = "\ninfix left 8 (<%>) = combine\n"
      assert(show(parseBody("a + b <%> c", declaration)) == "(a + (b <%> c))")
    }

    "a declaration may override a built-in operator's fixity" in {
      val declaration = "\ninfix right 6 (+) = add\n"
      assert(show(parseBody("a + b + c", declaration)) == "(a + (b + c))")
    }

    "an undeclared operator falls back to the tightest left-associative fixity" in
      assert(show(parseBody("a <%> b <%> c")) == "((a <%> b) <%> c)")
  }

  "nested positions" - {
    "list elements are re-associated" in
      assert(show(parseBody("[1 + 2 * 3]")) == "[(1 + (2 * 3))]")

    "a lambda body is re-associated" in {
      parseBody("\\x -> 1 + 2 * 3") match
        case n: CstLambda => assert(show(n.body) == "(1 + (2 * 3))")
        case other        => throw new AssertionError(s"expected a lambda, got: $other")
    }

    "an if branch is re-associated" in {
      parseBody("if p then 1 + 2 * 3 else 0") match
        case n: CstIfThenElse => assert(show(n.thenBranch) == "(1 + (2 * 3))")
        case other            => throw new AssertionError(s"expected an if/then/else, got: $other")
    }

    "a let binding body is re-associated" in {
      parseBody("let\n        y = 1 + 2 * 3\n    in\n    y") match
        case n: CstLetIn => assert(show(n.bindings.head.body) == "(1 + (2 * 3))")
        case other       => throw new AssertionError(s"expected a let/in, got: $other")
    }

    "a case branch body is re-associated" in {
      parseBody("case xs of\n        _ -> 1 + 2 * 3") match
        case n: CstCaseOf => assert(show(n.branches.head.body) == "(1 + (2 * 3))")
        case other        => throw new AssertionError(s"expected a case/of, got: $other")
    }
  }

  "spans" - {
    "each operator node spans only its own operands" in {
      val source = "module M exposing (..)\n\nmain = 1 + 2 * 3\n"
      val body   = parseModule(source).declarations.head match
        case d: CstValueDeclaration => d.body
        case other                  => throw new AssertionError(s"unexpected declaration: $other")

      body match
        case root @ CstBinaryOp(_, _, right: CstBinaryOp) =>
          assert(source.substring(root.span.start, root.span.end).trim == "1 + 2 * 3")
          assert(source.substring(right.span.start, right.span.end).trim == "2 * 3")
        case other => throw new AssertionError(s"expected a nested binary op, got: $other")
    }
  }

  "lowering" - {
    "the lowered AST keeps the re-associated shape" in {
      Elm.parseAst("module M exposing (..)\n\nmain = 1 + 2 * 3\n") match
        case Success(m) =>
          m.declarations.head match
            case d: ast.ValueDeclaration => assert(showAst(d.body) == "(1 + (2 * 3))")
            case other                   => throw new AssertionError(s"unexpected declaration: $other")
        case Failure(diagnostic) => throw new AssertionError(s"parse failed: $diagnostic")
    }
  }
