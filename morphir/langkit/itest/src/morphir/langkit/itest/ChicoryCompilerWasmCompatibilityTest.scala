package morphir.langkit.itest

import java.nio.file.Files
import java.nio.file.Path

import scala.jdk.CollectionConverters.*

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Assertions.fail
import org.junit.jupiter.api.Test

import com.dylibso.chicory.runtime.Instance
import com.dylibso.chicory.wasm.Parser
import com.dylibso.chicory.wasm.UnlinkableException

final class ChicoryCompilerWasmCompatibilityTest:
  private val artifactDirProperty   = "morphir.langkit.elm.compiler.api.wasm.dir"
  private val chicoryRuntimeVersion = "1.7.5"

  @Test
  def `Mill injects the compiler-api wasm artifact path into itest JVMs`(): Unit =
    val dir = artifactDir()
    assertTrue(Files.isDirectory(dir), dir.toString)
    assertTrue(Files.isRegularFile(dir.resolve("main.wasm")), dir.resolve("main.wasm").toString)

  @Test
  def `compiler-api wasm artifact exposes Scala js host imports and zero raw exports`(): Unit =
    val module  = parseModule()
    val imports = module
      .importSection()
      .stream()
      .iterator()
      .asScala
      .map(i => s"${i.module()}:${i.name()}")
      .toVector

    val context =
      s"artifact=${artifactPath()} chicory=$chicoryRuntimeVersion imports=${module.importSection().importCount()}"
    assertTrue(imports.contains("__scalaJSHelpers:JSTag"), s"$context\n${imports.mkString("\n")}")
    assertTrue(imports.exists(_.startsWith("wasm:js-string:")), s"$context\n${imports.mkString("\n")}")
    assertEquals(0, module.exportSection().exportCount(), context)

  @Test
  def `Chicory reports the missing Scala js host import when instantiation is attempted`(): Unit =
    val error =
      org.junit.jupiter.api.Assertions.assertThrows(
        classOf[UnlinkableException],
        () =>
          val _ = Instance.builder(parseModule()).build()
          ()
      )

    val context = s"artifact=${artifactPath()} chicory=$chicoryRuntimeVersion"
    assertTrue(error.getMessage.contains("unknown import"), s"$context\n${error.getMessage}")
    assertTrue(error.getMessage.contains("__scalaJSHelpers.JSTag"), s"$context\n${error.getMessage}")

  private def parseModule() =
    Parser.parse(artifactPath())

  private def artifactPath(): Path =
    artifactDir().resolve("main.wasm")

  private def artifactDir(): Path =
    sys.props.get(artifactDirProperty) match
      case Some(path) => Path.of(path)
      case None       =>
        fail(
          s"missing system property $artifactDirProperty; run this suite through Mill so it can inject the linked compiler-api.wasm artifact path"
        )
