package morphir.connector.github.internal

import java.nio.file.Files
import kyo.*
import kyo.test.*
import morphir.connector.github.GitHubException

class PlatformGhAuthTests extends Test[Any]:

  private def run[A](effect: A < (Abort[GitHubException] & Async)): Result[GitHubException, A] < Async =
    Abort.run[GitHubException](effect)

  "PlatformGhAuth" - {
    "does not expose command output when gh exits unsuccessfully" in {
      val stdoutSentinel = "gh-stdout-token-sentinel"
      val stderrSentinel = "gh-stderr-token-sentinel"
      val program        = Files.createTempFile("morphir-gh-test-", ".sh")
      Files.writeString(
        program,
        s"""#!/bin/sh
           |printf '%s' '$stdoutSentinel'
           |printf '%s' '$stderrSentinel' >&2
           |exit 23
           |""".stripMargin
      )
      assert(program.toFile.setExecutable(true))

      run(PlatformGhAuth.forProgram(program.toString).stdout(Chunk("auth", "token"))).map { result =>
        Files.deleteIfExists(program)
        result match
          case Result.Failure(error @ GitHubException.Unauthorized(detail)) =>
            assert(detail == "gh auth token exited 23")
            assert(!error.getMessage.contains(stdoutSentinel))
            assert(!error.getMessage.contains(stderrSentinel))
            assert(!Render.asString(error).contains(stdoutSentinel))
            assert(!Render.asString(error).contains(stderrSentinel))
            assert(!result.toString.contains(stdoutSentinel))
            assert(!result.toString.contains(stderrSentinel))
          case _ => assert(false)
      }
    }
  }
