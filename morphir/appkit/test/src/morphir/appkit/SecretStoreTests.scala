package morphir.appkit

import kyo.*
import kyo.test.*
import morphir.MorphirException
import morphir.appkit.internal.KeyringAccess
import morphir.appkit.internal.PlatformSecurity
import morphir.appkit.internal.SecurityCli

class SecretStoreTests extends Test[Any]:

  private def run[A](effect: A < (Abort[SecretException] & Async)): Result[SecretException, A] < Async =
    Abort.run[SecretException](effect)

  "SecretException" - {
    "is catchable as MorphirException" in {
      val error: Throwable = SecretException.LookupFailed("denied")
      val caught           =
        try throw error
        catch
          case _: MorphirException => true
          case _                   => false
      assert(caught)
    }
  }

  "Secret" - {
    "redacts its stored value and compares by value" in {
      val raw = "secret-value"
      Secret.fromStored(raw) match
        case Present(secret) =>
          assert(secret.toString == "Secret(redacted)")
          assert(!secret.toString.contains(raw))
          assert(secret.hashCode == 0)
          assert(secret == Secret.fromStored(raw).get)
        case Absent => assert(false)
    }
    "preserves leading and trailing whitespace" in {
      val raw = "  secret value  "
      Secret.fromStored(raw) match
        case Present(expected) =>
          val store = SecretStore.const(("gh", "morphir", raw))
          run(store.get("gh", "morphir")).map {
            case Result.Success(Present(got)) => assert(got == expected)
            case _                            => assert(false)
          }
        case Absent => assert(false)
    }
    "rejects only an empty stored value" in {
      assert(Secret.fromStored("").isEmpty)
      assert(Secret.fromStored(" ").nonEmpty)
    }
  }

  "SecretStore.const" - {
    "returns a stored secret" in {
      val store = SecretStore.const(("gh", "morphir", "secret"))
      run(store.get("gh", "morphir")).map {
        case Result.Success(Present(got)) => assert(got == Secret.fromStored("secret").get)
        case _                            => assert(false)
      }
    }
    "returns Absent when the entry is missing" in {
      val store = SecretStore.const()
      run(store.get("gh", "morphir")).map {
        case Result.Success(Absent) => assert(true)
        case _                      => assert(false)
      }
    }
  }

  "SecretStore.javaKeychain" - {
    "is constructible without reading the OS store" in {
      val _ = SecretStore.javaKeychain
      assert(true)
    }
    "yields a password from the keyring seam" in {
      val store = SecretStore.javaKeychain(KeyringAccess.fake(("gh", "morphir", "from-keyring")))
      run(store.get("gh", "morphir")).map {
        case Result.Success(Present(got)) => assert(got == Secret.fromStored("from-keyring").get)
        case _                            => assert(false)
      }
    }
    "returns Absent when the keyring has no entry" in {
      val store = SecretStore.javaKeychain(KeyringAccess.fake())
      run(store.get("gh", "morphir")).map {
        case Result.Success(Absent) => assert(true)
        case _                      => assert(false)
      }
    }
  }

  "SecretStore.macOsKeychain" - {
    "is constructible without spawning security" in {
      val _ = SecretStore.macOsKeychain
      assert(true)
    }
    "removes only the security line ending" in {
      assert(PlatformSecurity.stripTrailingLineEnding("  secret  \r\n") == "  secret  ")
      assert(PlatformSecurity.stripTrailingLineEnding("  secret  \n") == "  secret  ")
    }
    "maps a missing security executable to NotAvailable" in
      run(
        PlatformSecurity
          .forProgram("morphir-security-command-that-does-not-exist")
          .findGenericPassword("morphir-test-missing", "morphir-test-missing")
      ).map {
        case Result.Failure(SecretException.NotAvailable(detail)) =>
          assert(detail.contains("not installed") || detail.contains("could not be started"))
        case _ => assert(false)
      }
    "yields a password from the security seam" in {
      val store = SecretStore.macOsKeychain(SecurityCli.succeed("from-security"))
      run(store.get("gh", "morphir")).map {
        case Result.Success(Present(got)) => assert(got == Secret.fromStored("from-security").get)
        case _                            => assert(false)
      }
    }
    "returns Absent when security has no item" in {
      val store = SecretStore.macOsKeychain(SecurityCli.missing)
      run(store.get("gh", "morphir")).map {
        case Result.Success(Absent) => assert(true)
        case _                      => assert(false)
      }
    }
    "fails LookupFailed when security fails" in {
      val store = SecretStore.macOsKeychain(SecurityCli.fail(SecretException.LookupFailed("denied")))
      run(store.get("gh", "morphir")).map {
        case Result.Failure(SecretException.LookupFailed(detail)) =>
          assert(detail.contains("denied"))
        case _ => assert(false)
      }
    }
    "spawns security rather than failing as an unlinked process floor" in
      run(SecretStore.macOsKeychain.get("morphir-test-missing", "morphir-test-missing")).map {
        case Result.Failure(SecretException.NotAvailable(detail)) =>
          assert(!detail.contains("not linked"))
        case Result.Failure(SecretException.LookupFailed(_)) => assert(true)
        case Result.Success(_)                               => assert(true)
        case _                                               => assert(false)
      }
  }
