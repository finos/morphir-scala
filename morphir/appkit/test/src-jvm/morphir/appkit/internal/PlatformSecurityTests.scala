package morphir.appkit.internal

import java.nio.file.Files
import kyo.*
import kyo.test.*
import morphir.appkit.SecretException

class PlatformSecurityTests extends Test[Any]:

  private def run[A](effect: A < (Abort[SecretException] & Async)): Result[SecretException, A] < Async =
    Abort.run[SecretException](effect)

  "PlatformSecurity" - {
    "does not expose command output when security exits unsuccessfully" in {
      val stdoutSentinel = "security-stdout-token-sentinel"
      val stderrSentinel = "security-stderr-token-sentinel"
      val program        = Files.createTempFile("morphir-security-test-", ".sh")
      Files.writeString(
        program,
        s"""#!/bin/sh
           |printf '%s' '$stdoutSentinel'
           |printf '%s' '$stderrSentinel' >&2
           |exit 23
           |""".stripMargin
      )
      assert(program.toFile.setExecutable(true))

      run(PlatformSecurity.forProgram(program.toString).findGenericPassword("service", "account")).map { result =>
        Files.deleteIfExists(program)
        result match
          case Result.Failure(error @ SecretException.LookupFailed(detail)) =>
            assert(detail == "security exited 23")
            assert(!error.getMessage.contains(stdoutSentinel))
            assert(!error.getMessage.contains(stderrSentinel))
            assert(!error.toString.contains(stdoutSentinel))
            assert(!error.toString.contains(stderrSentinel))
            assert(!result.toString.contains(stdoutSentinel))
            assert(!result.toString.contains(stderrSentinel))
          case _ => assert(false)
      }
    }
  }
