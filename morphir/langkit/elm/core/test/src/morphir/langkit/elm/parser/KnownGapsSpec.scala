package morphir.langkit.elm.parser

import kyo.test.*

import morphir.langkit.elm.Elm

/**
 * Elm the parser does not accept yet, and Elm it accepts too readily.
 *
 * Every case here is a divergence from `elm/compiler` that is known, deliberate for now, and listed in the gap ledger
 * of
 * [[https://github.com/finos/morphir-scala/blob/main/.dev/.sdlc/elm-parser-conformance/PLAN.md the conformance plan]].
 * Pinning them means the suite states the divergence rather than hiding it, and closing one turns a green assertion red
 * — which forces both this file and the ledger to be updated rather than quietly drifting.
 *
 * This file shrinking is the point. It has already lost string escapes, unicode escapes, triple-quoted strings and
 * hexadecimal literals to W6.
 */
class KnownGapsSpec extends Test[Any]:

  private def parses(declaration: String): Boolean =
    Elm.parseCst(s"module M exposing (..)\n\n$declaration\n").isSuccess

  "GLSL blocks (W7)" - {
    "have a CST node but no production, so nothing parses into one" in
      assert(!parses("shader = [glsl| void main() {} |]"))
  }

  "astral code points in character literals (W7)" - {
    "are rejected, because the CST models a character as a JVM Char" in {
      // Elm accepts any code point here. `CstCharLiteral` holds a `Char`, which cannot represent one outside the
      // basic multilingual plane, so the parser refuses rather than truncating to a lone surrogate. Fixing this
      // means widening the CST node, not the lexer.
      assert(!parses("""c = '\u{1F600}'"""))
      assert(parses("""c = '\u{0041}'"""))
    }
  }

  "effect modules (W7)" - {
    "parse their header but not their `where` clause" in {
      val plainEffectModule = Elm.parseCst("effect module M exposing (..)\n\nx = 1\n").isSuccess
      val withWhereClause   = Elm.parseCst(
        "effect module M where { command = MyCmd, subscription = MySub } exposing (..)\n\nx = 1\n"
      ).isSuccess
      assert(plainEffectModule)
      assert(!withWhereClause)
    }
  }

  "tuple arity (W7)" - {
    "beyond three is accepted, where Elm stops at three" in
      assert(parses("t = ( 1, 2, 3, 4 )"))
  }
