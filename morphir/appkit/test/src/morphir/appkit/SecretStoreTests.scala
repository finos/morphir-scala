package morphir.appkit

import kyo.*
import kyo.test.*
import morphir.appkit.internal.KeyringGet
import morphir.appkit.internal.SecurityCli

class SecretStoreTests extends Test[Any]:

  private def run[A](effect: A < (Abort[SecretError] & Async)): Result[SecretError, A] < Async =
    Abort.run[SecretError](effect)

  "SecretStore.const" - {
    "returns a stored secret" in {
      val store = SecretStore.const(("gh", "morphir", "secret"))
      run(store.get("gh", "morphir")).map {
        case Result.Success(Present(got)) => assert(got == "secret")
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
      val store = SecretStore.javaKeychain(KeyringGet.succeed("from-keyring"))
      run(store.get("gh", "morphir")).map {
        case Result.Success(Present(got)) => assert(got == "from-keyring")
        case _                            => assert(false)
      }
    }
    "returns Absent when the keyring has no entry" in {
      val store = SecretStore.javaKeychain(KeyringGet.missing)
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
    "yields a password from the security seam" in {
      val store = SecretStore.macOsKeychain(SecurityCli.succeed("from-security"))
      run(store.get("gh", "morphir")).map {
        case Result.Success(Present(got)) => assert(got == "from-security")
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
      val store = SecretStore.macOsKeychain(SecurityCli.fail(SecretError.LookupFailed("denied")))
      run(store.get("gh", "morphir")).map {
        case Result.Failure(SecretError.LookupFailed(detail)) =>
          assert(detail.contains("denied"))
        case _ => assert(false)
      }
    }
  }
