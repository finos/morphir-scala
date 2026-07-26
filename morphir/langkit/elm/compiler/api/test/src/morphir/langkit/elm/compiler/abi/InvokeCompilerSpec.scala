package morphir.langkit.elm.compiler.abi

import kyo.test.*

import morphir.langkit.elm.Elm
import morphir.langkit.elm.compiler.CompileError
import morphir.langkit.elm.compiler.ParseDiagnostic

import InvokeJson.decode
import InvokeJson.given

class InvokeCompilerSpec extends Test[Any]:

  private val validSource =
    """module Demo exposing (..)
      |
      |main = 42
      |""".stripMargin

  private val malformedSource = "module Demo exposing (..)\n\nmain ="

  private def expectedParseInvokeError(source: String): InvokeError =
    Elm.parseCst(source) match
      case parsley.Failure(diagnostic: ParseDiagnostic) =>
        InvokeError.fromCompileError(CompileError.ParseError(phase = "cst", diagnostic = diagnostic))
      case parsley.Success(_) =>
        throw new AssertionError(s"expected parse failure for: $source")

  private def invoke(op: String, source: String): InvokeResponse =
    decode[InvokeResponse](InvokeCompiler.invoke(op, source))

  "InvokeCompiler" - {
    "happy path: parseCst returns a structured success envelope" in {
      val response = invoke("parseCst", s"""{"source":${stringLiteral(validSource)}}""")

      assert(response.ok)
      assert(response.logs.isEmpty)
      assert(response.errors.isEmpty)
      assert(response.value.exists(value => value.startsWith("CstModule(")))
    }
    "failure path: malformed source returns a structured parse error envelope" in {
      val response = invoke("parseCst", s"""{"source":${stringLiteral(malformedSource)}}""")
      val expected = expectedParseInvokeError(malformedSource)

      assert(!response.ok)
      assert(response.value.isEmpty)
      assert(response.logs.isEmpty)
      assert(response.errors == Vector(expected))
      assert(response.errors.head.contextLines.nonEmpty)
      assert(response.errors.head.contextLines.count(_.isErrorLine) == 1)
    }
    "edge path: unknown operation returns a structured internal error envelope" in {
      val response = invoke("wat", "{}")

      assert(!response.ok)
      assert(response.value.isEmpty)
      assert(response.logs.isEmpty)
      assert(
        response.errors == Vector(
          InvokeError(
            phase = "internal",
            message = "unknown operation: wat",
            span = None
          )
        )
      )
    }
    "determinism path: the same parseCst input returns byte-identical JSON twice" in {
      val input  = s"""{"source":${stringLiteral(validSource)}}"""
      val first  = InvokeCompiler.invoke("parseCst", input)
      val second = InvokeCompiler.invoke("parseCst", input)

      assert(first == second)
    }
  }

  private def stringLiteral(value: String): String =
    "\"" + value.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    } + "\""
