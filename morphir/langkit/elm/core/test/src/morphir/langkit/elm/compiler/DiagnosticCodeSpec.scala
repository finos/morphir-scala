package morphir.langkit.elm.compiler

import kyo.test.*

class DiagnosticCodeSpec extends Test[Any]:

  "DiagnosticCode" - {
    "validation" - {
      "accepts stable parse and tokenizer codes" in {
        assert(DiagnosticCode.make("ELM-P001").isRight)
        assert(DiagnosticCode.make("ELM-P002").isRight)
        assert(DiagnosticCode.make("ELM-P003").isRight)
        assert(DiagnosticCode.make("ELM-T001").isRight)
      }
      "rejects arbitrary strings" in {
        assert(DiagnosticCode.make("").isLeft)
        assert(DiagnosticCode.make("PARSE_ERROR").isLeft)
        assert(DiagnosticCode.make("ELM-X001").isLeft)
        assert(DiagnosticCode.make("ELM-P01").isLeft)
      }
    }
    "known codes" - {
      "exposes the current stable diagnostic codes" in {
        assert(DiagnosticCode.unwrap(DiagnosticCode.UnexpectedEndOfInput) == "ELM-P001")
        assert(DiagnosticCode.unwrap(DiagnosticCode.UnexpectedToken) == "ELM-P002")
        assert(DiagnosticCode.unwrap(DiagnosticCode.SpecialisedParseFailure) == "ELM-P003")
        assert(DiagnosticCode.unwrap(DiagnosticCode.TokenizerUnexpectedCharacter) == "ELM-T001")
      }
      "classifies tokenizer codes for message formatting" in {
        assert(DiagnosticCode.isTokenizer(DiagnosticCode.TokenizerUnexpectedCharacter))
        assert(!DiagnosticCode.isTokenizer(DiagnosticCode.UnexpectedEndOfInput))
      }
    }
  }
