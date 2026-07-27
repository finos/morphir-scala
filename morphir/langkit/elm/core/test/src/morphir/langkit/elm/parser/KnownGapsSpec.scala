package morphir.langkit.elm.parser

import kyo.test.*

import morphir.langkit.elm.Elm

/**
 * Elm the parser does not accept yet.
 *
 * Every case here is valid Elm that `elm/compiler` parses and we reject. They are pinned rather than left implicit so
 * that the gap is a fact in the suite rather than a surprise in the field, and so that closing one turns a green
 * assertion red and forces this file to be updated.
 *
 * All of them are lexical, and belong to W6 in
 * [[https://github.com/finos/morphir-scala/blob/main/.dev/.sdlc/elm-parser-conformance/PLAN.md the conformance plan]].
 */
class KnownGapsSpec extends Test[Any]:

  private def parses(declaration: String): Boolean =
    Elm.parseCst(s"module M exposing (..)\n\n$declaration\n").isSuccess

  "string and character escapes" - {
    "an escaped quote is not recognised" in
      assert(!parses("""s = "with \"escapes\"" """))

    "an escaped newline is not recognised" in
      assert(!parses("""s = "line\nbreak" """))

    "an escaped tab is not recognised" in
      assert(!parses("""s = "tab\there" """))

    "an escape in a character literal is not recognised" in
      assert(!parses("""c = '\n'"""))

    "plain text is fine, which is why this went unnoticed" in {
      assert(parses("""s = "plain text" """))
      assert(parses("""s = "" """))
      assert(parses("""c = 'c'"""))
    }
  }

  "triple-quoted strings" - {
    "are not recognised" in
      assert(!parses("s = \"\"\"triple quoted\"\"\""))
  }

  "numeric literals" - {
    "hexadecimal is not recognised" in
      assert(!parses("x = 0x1F"))

    "decimal and exponent forms are fine" in {
      assert(parses("x = 42"))
      assert(parses("x = 1.5"))
      assert(parses("x = 1.5e3"))
    }
  }

  "GLSL blocks" - {
    "have a CST node but no production, so nothing parses into one" in
      assert(!parses("shader = [glsl| void main() {} |]"))
  }
