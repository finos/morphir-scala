package morphir.langkit.itest

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

final class ChicoryCompilerWasmBackendTest:

  @Test
  def `supported Chicory driver parses valid Elm source through the compiler API`(): Unit =
    val json = ChicorySupportedCompilerHarness.invoke(
      op = "parseCst",
      inputJson = """{"source":"module Demo exposing (..)\n\nmain = 42\n"}"""
    )

    assertTrue(json.contains(""""ok":true"""), json)
    assertTrue(json.contains(""""value":"CstModule("""))

  @Test
  def `supported Chicory driver returns structured compiler errors`(): Unit =
    val json = ChicorySupportedCompilerHarness.invoke(
      op = "parseCst",
      inputJson = """{"source":"module Demo exposing (..)\n\nmain =\n"}"""
    )

    assertTrue(json.contains(""""ok":false"""), json)
    assertTrue(json.contains(""""errors":[{"""), json)

  @Test
  def `supported Chicory driver is deterministic for repeated compiler calls`(): Unit =
    val input = """{"source":"module Demo exposing (..)\n\nmain = 42\n"}"""
    val a     = ChicorySupportedCompilerHarness.invoke(op = "parseCst", inputJson = input)
    val b     = ChicorySupportedCompilerHarness.invoke(op = "parseCst", inputJson = input)

    assertEquals(a, b)
