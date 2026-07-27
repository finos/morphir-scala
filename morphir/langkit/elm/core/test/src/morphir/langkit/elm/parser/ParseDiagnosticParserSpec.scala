package morphir.langkit.elm.parser

import parsley.{Failure, Success}
import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.elm.compiler.DiagnosticCode
import morphir.langkit.elm.compiler.ParseDiagnostic

class ParseDiagnosticParserSpec extends Test[Any]:

  private val malformedSource = "module M exposing (..)\n\nx ="

  "ParseDiagnosticParser" - {
    "happy path: valid source produces zero diagnostics" in {
      val source = "module M exposing (..)\n\nx = 1\n"
      Elm.parseCst(source) match
        case Success(_) => assert(true)
        case Failure(_) => assert(false)
    }
    "malformed source produces ELM-P001 with span and expected tokens" in {
      Elm.parseCst(malformedSource) match
        case Failure(diagnostic: ParseDiagnostic) =>
          assert(diagnostic.code == DiagnosticCode.UnexpectedEndOfInput)
          assert(diagnostic.span.line == 3)
          assert(diagnostic.span.column == 4)
          assert(diagnostic.span.start == 27)
          assert(diagnostic.span.end == 27)
          assert(diagnostic.expected.nonEmpty)
          assert(diagnostic.message.contains("I ran into the end of the file unexpectedly"))
          assert(diagnostic.message.contains("1| module M exposing (..)"))
          assert(diagnostic.message.contains("3| x ="))
          assert(diagnostic.message.contains("^"))
          assert(diagnostic.message.contains("I was expecting one of the following:"))
          assert(diagnostic.contextLines.nonEmpty)
          assert(diagnostic.contextLines.count(_.isErrorLine) == 1)
        case Success(_) => assert(false)
    }
    "empty source produces ELM-P001 at start of file" in {
      Elm.parseCst("") match
        case Failure(diagnostic: ParseDiagnostic) =>
          assert(diagnostic.code == DiagnosticCode.UnexpectedEndOfInput)
          assert(diagnostic.span.line == 1)
          assert(diagnostic.span.column == 1)
          assert(diagnostic.span.start == 0)
          assert(diagnostic.span.end == 0)
          assert(diagnostic.expected.contains("module"))
          assert(diagnostic.message.contains("I ran into the end of the file unexpectedly"))
          assert(diagnostic.message.contains("1|"))
          assert(diagnostic.message.contains("^"))
          assert(diagnostic.message.contains("I was expecting one of the following:"))
          assert(diagnostic.contextLines.nonEmpty)
        case Success(_) => assert(false)
    }
  }
