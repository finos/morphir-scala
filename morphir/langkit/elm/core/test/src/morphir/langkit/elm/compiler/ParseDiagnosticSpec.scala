package morphir.langkit.elm.compiler

import kyo.test.*

import morphir.langkit.elm.parser.{DiagnosticBody, ParseDiagnosticErrorBuilder}

class ParseDiagnosticSpec extends Test[Any]:

  "ParseDiagnostic" - {
    "classifies unexpected end of input as ELM-P001" in {
      val diagnostic = ParseDiagnostic.unexpectedEndOfInput(
        source = "module M",
        line = 1,
        column = 9,
        expected = List("exposing", "where")
      )
      assert(diagnostic.code == DiagnosticCode.UnexpectedEndOfInput)
      assert(diagnostic.span.line == 1)
      assert(diagnostic.span.column == 9)
      assert(diagnostic.span.start == 8)
      assert(diagnostic.span.end == 8)
      assert(diagnostic.expected == List("exposing", "where"))
      assert(diagnostic.suggestion.isEmpty)
    }
    "classifies unexpected token as ELM-P002" in {
      val diagnostic = ParseDiagnostic.unexpectedToken(
        source = "x = @",
        line = 1,
        column = 5,
        width = 1,
        unexpected = "@",
        expected = List("identifier", "digit")
      )
      assert(diagnostic.code == DiagnosticCode.UnexpectedToken)
      assert(diagnostic.span.start == 4)
      assert(diagnostic.span.end == 5)
      assert(diagnostic.message.contains("unexpected"))
    }
    "tokenizer unexpected character uses ELM-T001" in {
      val diagnostic = ParseDiagnostic.tokenizerUnexpectedCharacter(
        source = "main @",
        offset = 5,
        lexeme = "@"
      )
      assert(diagnostic.code == DiagnosticCode.TokenizerUnexpectedCharacter)
      assert(diagnostic.span.start == 5)
      assert(diagnostic.span.end == 6)
      assert(diagnostic.span.line == 1)
      assert(diagnostic.span.column == 6)
      assert(diagnostic.message.contains("I ran into an unexpected character"))
      assert(diagnostic.message.contains("@"))
    }
    "suggestion helper recognizes missing in after let" in {
      val diagnostic = ParseDiagnosticErrorBuilder("module M\n\nx = 1").format(
        (3, 1),
        (),
        DiagnosticBody.Vanilla(
          unexpected = Some("end of input"),
          expected = Set("in", "identifier"),
          reasons = Nil,
          errorWidth = 0
        )
      )
      assert(diagnostic.suggestion.contains("Did you forget `in` after a `let` binding?"))
    }
  }
