//| scalaVersion: 3.8.4
//| mainClass: kyo.test.runner.Cli
//| resources: [test-resources]
//| moduleDeps: [squire.scala]
//| mvnDeps:
//| - io.getkyo::kyo-test-api:1.0.0-RC6
//| - io.getkyo::kyo-test-runner:1.0.0-RC6

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import kyo.*
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
  private val skillDirectory = java.nio.file.Paths.get(java.lang.System.getProperty("user.dir"))

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
      assert(registry == Set("SquireCliSpec", "SquireMetaSpec", "SquireModelSpec", "SquireProcessSpec"))
    }
  }

class SquireModelSpec extends Test[Any]:
  "JSON" - {
    "pretty JSON preserves field order and escapes control characters" in {
      val value = Structure.Value.Record(
        Chunk(
          "z" -> Structure.Value.Str("line\n\"quoted\""),
          "a" -> Structure.Value.Sequence(
            Chunk(Structure.Value.Integer(1), Structure.Value.Bool(true))
          )
        )
      )
      assert(
        SquireJson.pretty(value) == "{\n  \"z\": \"line\\n\\\"quoted\\\"\",\n  \"a\": [\n    1,\n    true\n  ]\n}\n"
      )
    }

    "rejects Bytes as non-deterministic JSON" in {
      assert(isRejectedAsNonDeterministicJson(Structure.Value.Bytes(Span.from(Array[Byte](1, 2, 3)))))
    }

    "rejects Instant as non-deterministic JSON" in {
      assert(isRejectedAsNonDeterministicJson(Structure.Value.Instant(java.time.Instant.EPOCH)))
    }

    "rejects Duration as non-deterministic JSON" in {
      assert(isRejectedAsNonDeterministicJson(Structure.Value.Duration(java.time.Duration.ZERO)))
    }
  }

  "paths" - {
    "resolveUnder rejects a sibling prefix" in {
      val base    = Path("/tmp/squire-path-test/.refs")
      val sibling = Path("/tmp/squire-path-test/.refs-escaped/repo")
      assert(SquirePaths.resolveUnder(sibling, base).isFailure)
    }

    "resolveUnder rejects an in-base symlink that escapes" in {
      for
        root    <- SquireFixtures.scratch("path")
        base    = root / ".refs"
        outside = root / "outside"
        link    = base / "link-outside"
        _ <- Sync.defer {
          Files.createDirectories(base.toJava)
          Files.createDirectories(outside.toJava)
          Files.createSymbolicLink(link.toJava, outside.toJava)
        }
        result = SquirePaths.resolveUnder(link / "repo", base)
      yield assert(result.isFailure)
    }
  }

  private def isRejectedAsNonDeterministicJson(value: Structure.Value): Boolean =
    try
      SquireJson.pretty(value)
      false
    catch
      case SquireError.Failure(area, message, _) =>
        area == "json" && message == "value cannot be represented as deterministic JSON"

class SquireProcessSpec extends Test[Any]:
  "process runner" - {
    "recording runner preserves argv cwd stdout stderr and exit" in {
      val request  = ProcessRequest(Chunk("git", "status"), Present(Path("/repo")))
      val expected = ProcessResult(request, 7, "out", "err")
      val runner   = RecordingRunner(Chunk(expected))
      runner.run(request).map(result => assert(result == expected && runner.requests == Chunk(request)))
    }

    "live runner captures stdout and stderr separately" in {
      val outputBytes = 128 * 1024
      for
        root   <- SquireFixtures.scratch("process")
        source = root / "ProcessProbe.java"
        _ <- source.write(
          s"class ProcessProbe { public static void main(String[] a) { String out = \"o\".repeat($outputBytes); String err = \"e\".repeat($outputBytes); System.out.print(out); System.err.print(err); System.exit(7); } }"
        )
        outcome <- Abort.run[SquireError | Timeout](
          Async.timeout(5.seconds)(
            LiveProcessRunner.run(ProcessRequest(Chunk(SquireFixtures.javaExecutable, SquirePaths.render(source))))
          )
        )
        result = outcome match
          case Result.Success(value) =>
            value.exitCode == 7 &&
              value.stdout == "o".repeat(outputBytes) &&
              value.stderr == "e".repeat(outputBytes)
          case Result.Failure(_) => false
      yield assert(result)
    }
  }

object SquireFixtures:
  val javaExecutable: String =
    java.nio.file.Path.of(java.lang.System.getProperty("java.home"), "bin", "java").toString

  def scratch(name: String): Path < Sync =
    Sync.defer(Path(java.nio.file.Files.createTempDirectory(s"squire-$name-").toString))

final class RecordingRunner(responses: Chunk[ProcessResult]) extends ProcessRunner:
  private var index = 0
  var requests: Chunk[ProcessRequest] = Chunk.empty

  def run(request: ProcessRequest): ProcessResult < (Async & Abort[SquireError]) =
    requests = requests.append(request)
    val response = responses(index)
    index += 1
    response
