package morphir.langkit.elm.parser

import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.elm.cst.*

/**
 * Literals, per `elm/compiler`'s `Parse.String` and `Parse.Number`.
 *
 * The assertions are on the *value* a literal parses to, not on whether it parses. An escape that is accepted but
 * decoded wrongly is worse than one that is rejected: it produces a tree that compiles and is quietly incorrect.
 */
class LexicalSpec extends Test[Any]:

  private def parses(declaration: String): Boolean =
    Elm.parseCst(s"module M exposing (..)\n\n$declaration\n").isSuccess

  private def body(declaration: String): CstExpression =
    Elm.parseCst(s"module M exposing (..)\n\n$declaration\n").fold(
      diagnostic => throw new AssertionError(s"parse failed: $diagnostic"),
      _.declarations.head match
        case d: CstValueDeclaration => d.body
        case other                  => throw new AssertionError(s"unexpected declaration: $other")
    )

  private def stringValue(declaration: String): String =
    body(declaration) match
      case n: CstStringLiteral => n.value
      case other               => throw new AssertionError(s"expected a string literal, got: $other")

  private def charValue(declaration: String): Char =
    body(declaration) match
      case n: CstCharLiteral => n.value
      case other             => throw new AssertionError(s"expected a character literal, got: $other")

  private def intValue(declaration: String): Long =
    body(declaration) match
      case n: CstIntLiteral => n.value
      case other            => throw new AssertionError(s"expected an integer literal, got: $other")

  private def floatValue(declaration: String): Double =
    body(declaration) match
      case n: CstFloatLiteral => n.value
      case other              => throw new AssertionError(s"expected a float literal, got: $other")

  "string escapes" - {
    "the six Elm knows decode to their characters" in {
      assert(stringValue("s = \"a\\nb\"") == "a\nb")
      assert(stringValue("s = \"a\\rb\"") == "a\rb")
      assert(stringValue("s = \"a\\tb\"") == "a\tb")
      assert(stringValue("s = \"a\\\"b\"") == "a\"b")
      assert(stringValue("s = \"a\\'b\"") == "a'b")
      assert(stringValue("s = \"a\\\\b\"") == "a\\b")
    }

    "an unknown escape is rejected rather than passed through" in {
      assert(!parses("""s = "a\qb""""))
      assert(!parses("""s = "a\0b""""))
    }

    "a raw line break does not belong in a single-quoted string" in
      assert(!parses("s = \"first\nsecond\""))

    "an unterminated string is rejected" in
      assert(!parses("""s = "unterminated"""))
  }

  "unicode escapes" - {
    "decode a code point in the basic plane" in {
      assert(stringValue("s = \"\\u{0041}\"") == "A")
      assert(stringValue("s = \"\\u{00e9}\"") == "é")
    }

    "decode a code point outside it" in
      assert(stringValue("s = \"\\u{1F600}\"") == "😀")

    "need between four and six digits, as Elm requires" in {
      assert(!parses("""s = "\u{41}""""))
      assert(!parses("""s = "\u{0000041}""""))
      assert(!parses("""s = "\u{}""""))
    }

    "reject a code point that is not one" in
      assert(!parses("""s = "\u{110000}""""))
  }

  "triple-quoted strings" - {
    "take line breaks as content" in
      assert(stringValue("s = \"\"\"first\nsecond\"\"\"") == "first\nsecond")

    "take a lone quote as content" in
      assert(stringValue("s = \"\"\"a \" b\"\"\"") == "a \" b")

    "still honour escapes" in
      assert(stringValue("s = \"\"\"a\\tb\"\"\"") == "a\tb")

    "may be empty" in
      assert(stringValue("s = \"\"\"\"\"\"") == "")
  }

  "character literals" - {
    "hold a plain character" in
      assert(charValue("c = 'a'") == 'a')

    "hold an escape" in {
      assert(charValue("""c = '\n'""") == '\n')
      assert(charValue("""c = '\''""") == '\'')
      assert(charValue("""c = '\\'""") == '\\')
    }

    "hold a unicode escape" in
      assert(charValue("""c = '\u{0041}'""") == 'A')

    "reject an empty one" in
      assert(!parses("c = ''"))
  }

  "integer literals" - {
    "are decimal by default" in
      assert(intValue("x = 42") == 42L)

    "may be hexadecimal" in {
      assert(intValue("x = 0x1F") == 31L)
      assert(intValue("x = 0xff") == 255L)
    }

    "reject the bases Elm does not have" in {
      assert(!parses("x = 0o17"))
      assert(!parses("x = 0b1010"))
    }

    "reject a leading zero, as Elm does" in {
      assert(!parses("x = 007"))
      assert(intValue("x = 0") == 0L)
    }
  }

  "float literals" - {
    "carry a fraction" in
      assert(floatValue("x = 1.5") == 1.5)

    "carry an exponent" in {
      assert(floatValue("x = 1.5e3") == 1500.0)
      assert(floatValue("x = 1.5E3") == 1500.0)
      assert(floatValue("x = 1.5e-3") == 0.0015)
    }

    "need digits either side of the dot" in {
      assert(!parses("x = 1."))
      assert(!parses("x = .5"))
    }
  }
