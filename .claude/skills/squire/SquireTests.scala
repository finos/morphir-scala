//| scalaVersion: 3.8.4
//| mainClass: kyo.test.runner.Cli
//| resources: [test-resources]
//| moduleDeps: [squire.scala]
//| mvnDeps:
//| - io.getkyo::kyo-test-api:1.0.0-RC6
//| - io.getkyo::kyo-test-runner:1.0.0-RC6

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import kyo.test.*

class SquireCliSpec extends Test[Any]:
  "commands" - {
    "expose the complete unified command tree" in {
      val expected = Set(
        List("ai", "env", "info"),
        List("doctor"),
        List("cellar", "get"),
        List("cellar", "search"),
        List("cellar", "deps"),
        List("reference", "repo", "add"),
        List("reference", "repo", "list"),
        List("reference", "repo", "status"),
        List("reference", "repo", "remove"),
        List("branch", "refresh"),
        List("tracking", "status"),
        List("tracking", "sync"),
        List("tracking", "doctor"),
        List("spec", "sync"),
        List("spec", "export"),
        List("schemas", "build"),
        List("schemas", "compare"),
        List("schemas", "validate")
      )

      assert(SquireApp.commands.flatMap(_.names).toSet == expected)
    }
  }

class SquireMetaSpec extends Test[Any]:
  private val skillDirectory = java.nio.file.Paths.get(System.getProperty("user.dir"))

  private def read(name: String): String =
    Files.readString(skillDirectory.resolve(name), StandardCharsets.UTF_8)

  "launchers" - {
    "run the single-file application without a Mill server or ticker" in {
      assert(read("squire").contains("--no-server --ticker false squire.scala"))
      assert(read("squire.bat").contains("--no-server --ticker false squire.scala"))
    }
  }

  "Mill version" - {
    "matches the repository version" in {
      val repositoryVersion =
        Files.readString(skillDirectory.resolve("../../../.mill-version"), StandardCharsets.UTF_8).trim
      assert(read(".mill-version").trim == repositoryVersion)
    }
  }

  "suite registry" - {
    "lists every suite declared by this test file" in {
      val registry = read("test-resources/META-INF/services/kyo.test.Test")
        .linesIterator
        .map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("#"))
        .toSet
      assert(registry == Set("SquireCliSpec", "SquireMetaSpec"))
    }
  }
