package morphir.langkit.elm.compiler

import kyo.test.*

class DiagnosticMessageFormatterSpec extends Test[Any]:

  private val malformedSource = "module M exposing (..)\n\nx ="

  "DiagnosticMessageFormatter" - {
    "formats unexpected end of input with surrounding context and expected tokens" in {
      val formatted = DiagnosticMessageFormatter.format(
        source = malformedSource,
        code = DiagnosticCode.UnexpectedEndOfInput,
        line = 3,
        column = 4,
        unexpected = Some("end of input"),
        expected = List("identifier", "digit"),
        reasons = Nil,
        suggestion = None,
        errorWidth = 0
      )
      assert(formatted.message == """-- PARSE ERROR (ELM-P001) at line 3, column 4

I ran into the end of the file unexpectedly.

I was expecting one of the following:

    identifier
    digit

1| module M exposing (..)
2| 
3| x =
      ^""")
      assert(formatted.contextLines.map(_.line) == List(1, 2, 3))
      assert(formatted.contextLines.count(_.isErrorLine) == 1)
      assert(formatted.contextLines.find(_.isErrorLine).exists(_.text == "x ="))
    }
    "formats tokenizer unexpected character errors with surrounding context" in {
      val formatted = DiagnosticMessageFormatter.format(
        source = "main @",
        code = DiagnosticCode.TokenizerUnexpectedCharacter,
        line = 1,
        column = 6,
        unexpected = Some("@"),
        expected = Nil,
        reasons = Nil,
        suggestion = None,
        errorWidth = 1
      )
      assert(formatted.message == """-- TOKENIZE ERROR (ELM-T001) at line 1, column 6

I ran into an unexpected character:

    @

1| main @
        ^""")
      assert(formatted.contextLines == List(DiagnosticContextLine(1, "main @", isErrorLine = true)))
    }
    "appends hints when provided" in {
      val formatted = DiagnosticMessageFormatter.format(
        source = "let x = 1",
        code = DiagnosticCode.UnexpectedEndOfInput,
        line = 1,
        column = 10,
        unexpected = Some("end of input"),
        expected = List("in"),
        reasons = Nil,
        suggestion = Some("Did you forget `in` after a `let` binding?"),
        errorWidth = 0
      )
      assert(formatted.message.endsWith("Hint: Did you forget `in` after a `let` binding?"))
      assert(formatted.message.contains("I was expecting one of the following:"))
    }
  }
