package morphir.langkit.elm.lexer

import kyo.test.*

import morphir.langkit.elm.compiler.{CompileError, DiagnosticCode}

class ElmTokenizerSpec extends Test[Any]:

  private def valueOf(result: ElmTokenizer.TokenizeResult[Vector[ElmToken]]): Vector[ElmToken] =
    result.value match
      case Right(tokens) => tokens
      case Left(errors)  => throw new AssertionError(s"expected tokens, got errors: $errors")

  "ElmTokenizer" - {
    "tokenizes keywords, identifiers, operators, literals, punctuation, and spans" in {
      val tokens = valueOf(ElmTokenizer.run("""module Main exposing (main = "hi")"""))

      assert(
        tokens.map(_.kind) == Vector(
          ElmTokenKind.Keyword,
          ElmTokenKind.UpperIdentifier,
          ElmTokenKind.Keyword,
          ElmTokenKind.Punctuation,
          ElmTokenKind.LowerIdentifier,
          ElmTokenKind.Operator,
          ElmTokenKind.StringLiteral,
          ElmTokenKind.Punctuation
        )
      )
      assert(
        tokens.map(t => (t.lexeme, t.start, t.end)) == Vector(
          ("module", 0, 6),
          ("Main", 7, 11),
          ("exposing", 12, 20),
          ("(", 21, 22),
          ("main", 22, 26),
          ("=", 27, 28),
          ("\"hi\"", 29, 33),
          (")", 33, 34)
        )
      )
    }
    "excludes trivia by default and includes whitespace, newlines, and comments when configured" in {
      val source        = "main = 1 -- greeting\nnext = 2"
      val defaultTokens = valueOf(ElmTokenizer.run(source))
      val triviaTokens  =
        valueOf(ElmTokenizer.run(source, ElmTokenizerConfig(includeTrivia = true, recoverUnknown = true)))

      assert(
        !defaultTokens.exists(t =>
          t.kind == ElmTokenKind.Whitespace || t.kind == ElmTokenKind.Newline || t.kind == ElmTokenKind.Comment
        )
      )
      assert(triviaTokens.exists(t => t.kind == ElmTokenKind.Whitespace && t.lexeme == " "))
      assert(triviaTokens.exists(t => t.kind == ElmTokenKind.Comment && t.lexeme == "-- greeting"))
      assert(triviaTokens.exists(t => t.kind == ElmTokenKind.Newline && t.lexeme == "\n"))
    }
    "matches longest operators before shorter prefixes" in {
      val tokens = valueOf(ElmTokenizer.run("a -> b |> c"))

      assert(tokens.filter(_.kind == ElmTokenKind.Operator).map(_.lexeme) == Vector("->", "|>"))
    }
    "recovers unknown input with a token and diagnostic log when configured" in {
      val result = ElmTokenizer.run("main @ value")
      val tokens = valueOf(result)

      assert(tokens.exists(t => t.kind == ElmTokenKind.Unknown && t.lexeme == "@"))
      assert(result.logs.exists(_.contains("Recovered unknown token '@' at 5")))
    }
    "uses the error channel for unrecovered unknown input" in {
      val result =
        ElmTokenizer.run("main @ value", ElmTokenizerConfig(includeTrivia = false, recoverUnknown = false))

      assert(result.value.isLeft)
      assert(result.errors.exists {
        case CompileError.ParseError("tokenize", diagnostic) =>
          diagnostic.code == DiagnosticCode.TokenizerUnexpectedCharacter &&
          diagnostic.message.contains("I ran into an unexpected character") &&
          diagnostic.message.contains("@") &&
          diagnostic.span.start == 5 &&
          diagnostic.span.end == 6
        case _ => false
      })
    }
  }
