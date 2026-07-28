package morphir.langkit.elm.parser

import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.elm.cst.*
import morphir.langkit.elm.lexer.ElmLexer

/**
 * Which character sequences may be an operator at all, per `elm/compiler`'s `Parse.Symbol`.
 *
 * The operator character set and the reserved sequences are the boundary between "this is a binary operator" and "this
 * is structure". Getting it wrong does not usually stop a file parsing — it makes a malformed expression swallow the
 * next declaration and report the failure somewhere else.
 */
class OperatorTokenSpec extends Test[Any]:

  private def parses(declaration: String): Boolean =
    Elm.parseCst(s"module M exposing (..)\n\n$declaration\n").isSuccess

  "the operator character set" - {
    "matches elm/compiler's binopCharSet" in
      assert(ElmLexer.operatorCharacters == "+-/*=.<>:&|^?%!".toSet)

    "excludes `~`, which is not an Elm operator character" in {
      assert(!ElmLexer.operatorCharacters.contains('~'))
      assert(!parses("main = a ~ b"))
    }

    "excludes `\\`, which belongs to lambda syntax" in {
      assert(!ElmLexer.operatorCharacters.contains('\\'))
      assert(parses("main = \\x -> x"))
    }
  }

  "reserved sequences" - {
    "are the five elm/compiler reserves" in
      assert(ElmLexer.reservedOperators == Set(".", "|", "->", "=", ":"))

    "`.` is not a binary operator, and a spaced dot is not field access either" in {
      assert(ElmLexer.reservedOperators.contains("."))
      assert(!parses("main = a . b"))
    }

    "`|` is not a binary operator" in
      assert(!parses("main = a | b"))

    "`=` is not a binary operator" in
      assert(!parses("main = a = b"))

    "`->` is not a binary operator" in
      assert(!parses("main = a -> b"))
  }

  "sequences that remain valid" - {
    "`.` still forms field access without spaces" in {
      val body = Elm.parseCst("module M exposing (..)\n\nmain = rec.field\n").fold(
        diagnostic => throw new AssertionError(s"parse failed: $diagnostic"),
        _.declarations.head match
          case d: CstValueDeclaration => d.body
          case other                  => throw new AssertionError(s"unexpected declaration: $other")
      )
      body match
        case CstFieldAccess(_, field) => assert(field.value == "field")
        case other                    => throw new AssertionError(s"expected field access, got: $other")
    }

    "`|` still separates custom type constructors" in
      assert(parses("type T = A | B"))

    "`=` still separates a declaration from its body" in
      assert(parses("main = 1"))

    "`->` still separates a case branch from its result" in
      assert(parses("main = case xs of\n        _ -> 1"))

    "`::` is a binary operator, not a reserved sequence" in
      assert(parses("main = a :: rest"))
  }
