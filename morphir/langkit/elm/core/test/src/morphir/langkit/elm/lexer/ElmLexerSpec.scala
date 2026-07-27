package morphir.langkit.elm.lexer

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.elm.lexer.ElmLexer.*

class ElmLexerSpec extends Test[Any]:

  private def ok[A](r: parsley.Result[String, A]): Boolean = r match
    case Success(_) => true
    case Failure(_) => false

  private def valueOf[A](r: parsley.Result[String, A]): A = r match
    case Success(a)   => a
    case Failure(msg) => throw new AssertionError(s"lex failed: $msg")

  "ElmLexer" - {
    "identifiers" - {
      "lowerIdentifier parses lowercase names" in
        assert(ok(fully(lowerIdentifier).parse("foo")))
      "lowerIdentifier accepts underscore prefix" in
        assert(ok(fully(lowerIdentifier).parse("_bar")))
      "lowerIdentifier rejects uppercase names" in
        assert(!ok(fully(lowerIdentifier).parse("Foo")))
      "upperIdentifier parses capitalised names" in
        assert(ok(fully(upperIdentifier).parse("Foo")))
      "upperIdentifier rejects lowercase names" in
        assert(!ok(fully(upperIdentifier).parse("foo")))
      "identifier rejects keyword 'module'" in
        assert(!ok(fully(identifier).parse("module")))
    }
    "operators" - {
      "operator parses a user-defined operator" in
        assert(ok(fully(operator).parse(":=:")))
    }
    "keywords and symbols" - {
      "keyword matches a declared keyword" in
        assert(ok(fully(keyword("module")).parse("module")))
      "symbol matches a declared operator" in
        assert(ok(fully(symbol("->")).parse("->")))
    }
    "numeric literals" - {
      "intLiteral parses decimals" in
        assert(valueOf(fully(intLiteral).parse("42")) == 42L)
      "floatLiteral parses decimal floats" in {
        val v = valueOf(fully(floatLiteral).parse("3.14"))
        assert(math.abs(v - 3.14) < 1e-9)
      }
    }
    "text literals" - {
      "stringLiteral parses quoted text" in
        assert(valueOf(fully(stringLiteral).parse("\"hello\"")) == "hello")
      "charLiteral parses quoted characters" in
        assert(valueOf(fully(charLiteral).parse("'x'")) == 'x')
    }
    "enclosers" - {
      "parens wraps inner parser" in
        assert(ok(fully(parens(intLiteral)).parse("(1)")))
      "brackets wraps inner parser" in
        assert(ok(fully(brackets(intLiteral)).parse("[1]")))
      "braces wraps inner parser" in
        assert(ok(fully(braces(intLiteral)).parse("{1}")))
    }
    "comma separators" - {
      "commaSep parses an empty list" in
        assert(valueOf(fully(commaSep(intLiteral)).parse("")) == List.empty[Long])
      "commaSep parses multiple items" in
        assert(valueOf(fully(commaSep(intLiteral)).parse("1, 2, 3")) == List(1L, 2L, 3L))
      "commaSep1 requires at least one item" in
        assert(!ok(fully(commaSep1(intLiteral)).parse("")))
    }
    "whitespace and comments" - {
      "line comments are skipped" in
        assert(valueOf(fully(intLiteral).parse("42 -- trailing comment\n")) == 42L)
      "nested block comments are skipped" in
        assert(valueOf(fully(intLiteral).parse("42 {- outer {- inner -} still outer -}")) == 42L)
    }
  }
