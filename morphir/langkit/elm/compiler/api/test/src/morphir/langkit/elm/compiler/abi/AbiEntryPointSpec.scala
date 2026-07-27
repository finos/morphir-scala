package morphir.langkit.elm.compiler.abi

import java.nio.charset.StandardCharsets

import kyo.test.*

import InvokeJson.decode
import InvokeJson.given

class AbiEntryPointSpec extends Test[Any]:

  private val utf8 = StandardCharsets.UTF_8

  "AbiEntryPoint" - {
    "happy path: round-trips a UTF-8 parseCst request to a JSON envelope" in {
      val bytes = AbiEntryPoint.invokeUtf8(
        "parseCst".getBytes(utf8),
        """{"source":"module Demo exposing (..)\n\nmain = 42\n"}""".getBytes(utf8)
      )
      val response = decode[InvokeResponse](String(bytes, utf8))

      assert(response.ok)
      assert(response.errors.isEmpty)
      assert(response.value.exists(_.startsWith("CstModule(")))
    }
    "failure path: returns a structured error for malformed UTF-8 JSON payloads" in {
      val bytes = AbiEntryPoint.invokeUtf8(
        "parseCst".getBytes(utf8),
        Array[Byte](0xff.toByte)
      )
      val response = decode[InvokeResponse](String(bytes, utf8))

      assert(!response.ok)
      assert(response.value.isEmpty)
      assert(response.errors.exists(error => error.phase == "internal"))
    }
    "determinism path: identical UTF-8 requests return byte-identical JSON" in {
      val op      = "parseCst".getBytes(utf8)
      val payload = """{"source":"module Demo exposing (..)\n\nmain = 42\n"}""".getBytes(utf8)

      assert(AbiEntryPoint.invokeUtf8(op, payload).toVector == AbiEntryPoint.invokeUtf8(op, payload).toVector)
    }
  }
