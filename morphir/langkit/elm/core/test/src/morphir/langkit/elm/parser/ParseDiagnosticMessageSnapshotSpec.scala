package morphir.langkit.elm.parser

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.elm.compiler.{DiagnosticCode, ParseDiagnostic}
import morphir.langkit.elm.lexer.{ElmTokenizer, ElmTokenizerConfig}

class ParseDiagnosticMessageSnapshotSpec extends Test[Any]:

  "ParseDiagnosticMessageSnapshot" - {
    "malformed module value documents the friendly end-of-input message shape" in {
      val source = "module M exposing (..)\n\nx ="
      Elm.parseCst(source) match
        case Failure(diagnostic: ParseDiagnostic) =>
          assert(diagnostic.code == DiagnosticCode.UnexpectedEndOfInput)
          assert(diagnostic.message.startsWith("-- PARSE ERROR (ELM-P001)"))
          assert(diagnostic.message.contains("I ran into the end of the file unexpectedly."))
          assert(diagnostic.message.contains("I was expecting one of the following:"))
          assert(diagnostic.message.contains("1| module M exposing (..)"))
          assert(diagnostic.message.contains("3| x ="))
          assert(diagnostic.message.linesIterator.toList.last == "      ^")
          assert(diagnostic.contextLines.nonEmpty)
          assert(diagnostic.contextLines.count(_.isErrorLine) == 1)
        case Success(_) => assert(false)
    }
    "an expectation that merely spells `in` does not suggest a missing `let` binding" in {
      // "string literal" contains the letters of `in`, which an earlier substring check took for the keyword and
      // hinted about a `let` that was nowhere in the source.
      val source = "module M exposing (..)\n\nx ="
      Elm.parseCst(source) match
        case Failure(diagnostic: ParseDiagnostic) =>
          assert(diagnostic.message.contains("string literal"))
          assert(!diagnostic.message.contains("Did you forget `in` after a `let` binding?"))
        case Success(_) => assert(false)
    }
    "a genuinely missing `in` is still suggested" in {
      val source = "module M exposing (..)\n\nx =\n    let\n        y = 1"
      Elm.parseCst(source) match
        case Failure(diagnostic: ParseDiagnostic) =>
          assert(diagnostic.message.contains("Did you forget `in` after a `let` binding?"))
        case Success(_) => assert(false)
    }
    "tokenizer failure documents the friendly unexpected-character message shape" in {
      val result = ElmTokenizer.run("main @", ElmTokenizerConfig(includeTrivia = false, recoverUnknown = false))
      assert(result.errors.exists {
        case morphir.langkit.elm.compiler.CompileError.ParseError("tokenize", diagnostic) =>
          diagnostic.code == DiagnosticCode.TokenizerUnexpectedCharacter &&
          diagnostic.message.startsWith("-- TOKENIZE ERROR (ELM-T001)") &&
          diagnostic.message.contains("I ran into an unexpected character:") &&
          diagnostic.message.contains("1| main @") &&
          diagnostic.message.linesIterator.toList.last == "        ^" &&
          diagnostic.contextLines.count(_.isErrorLine) == 1
        case _ => false
      })
    }
  }
