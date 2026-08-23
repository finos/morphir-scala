package morphir.appkit

import kyo.*
import kyo.test.*
import morphir.appkit.internal.KeyringAccess

class SecretVaultTests extends Test[Any]:

  private def run[A](effect: A < (Abort[SecretException] & Async)): Result[SecretException, A] < Async =
    Abort.run[SecretException](effect)

  private def makeVault(entries: (String, String, String)*): (SecretVault, KeyringAccess.Fake) =
    val keyring = KeyringAccess.fake(entries*)
    (SecretVault.system(keyring), keyring)

  "SecretVault.system" - {
    "reads a stored secret without exposing its value" in {
      val (vault, keyring) = makeVault(("morphir", "github.com", "stored-token"))
      run(vault.get("morphir", "github.com")).map {
        case Result.Success(Present(secret)) =>
          assert(secret.toString == "Secret(redacted)")
          assert(!secret.toString.contains("stored-token"))
          assert(keyring.calls == Seq(KeyringAccess.Call.Get("morphir", "github.com")))
        case _ => assert(false)
      }
    }

    "returns Absent for a missing entry" in {
      val (vault, keyring) = makeVault()
      run(vault.get("morphir", "github.com")).map {
        case Result.Success(Absent) =>
          assert(keyring.calls == Seq(KeyringAccess.Call.Get("morphir", "github.com")))
        case _ => assert(false)
      }
    }

    "writes a secret and keeps stable service and account keys" in {
      val (vault, keyring) = makeVault()
      Secret.fromStored("remembered-token") match
        case Present(secret) =>
          run(vault.put("io.finos.morphir", "github.com", secret)).map {
            case Result.Success(()) =>
              run(vault.get("io.finos.morphir", "github.com")).map {
                case Result.Success(Present(stored)) =>
                  assert(stored == secret)
                  assert(stored.toString == "Secret(redacted)")
                  assert(
                    keyring.calls == Seq(
                      KeyringAccess.Call.Set("io.finos.morphir", "github.com"),
                      KeyringAccess.Call.Get("io.finos.morphir", "github.com")
                    )
                  )
                case _ => assert(false)
              }
            case _ => assert(false)
          }
        case Absent => assert(false)
    }

    "redacts a secret from recorded write calls while retaining it in the vault" in {
      val sentinel         = "call-log-sentinel-secret"
      val (vault, keyring) = makeVault()
      Secret.fromStored(sentinel) match
        case Present(secret) =>
          run(vault.put("morphir", "github.com", secret)).map {
            case Result.Success(()) =>
              run(vault.get("morphir", "github.com")).map {
                case Result.Success(Present(stored)) =>
                  assert(stored == secret)
                  assert(!keyring.calls.toString.contains(sentinel))
                case _ => assert(false)
              }
            case _ => assert(false)
          }
        case Absent => assert(false)
    }

    "removes a stored secret" in {
      val (vault, keyring) = makeVault(("morphir", "github.com", "remembered-token"))
      run(vault.remove("morphir", "github.com")).map {
        case Result.Success(()) =>
          run(vault.get("morphir", "github.com")).map {
            case Result.Success(Absent) =>
              assert(
                keyring.calls == Seq(
                  KeyringAccess.Call.Delete("morphir", "github.com"),
                  KeyringAccess.Call.Get("morphir", "github.com")
                )
              )
            case _ => assert(false)
          }
        case _ => assert(false)
      }
    }

    "maps a failed read without exposing the backend message" in {
      val (vault, keyring) = makeVault()
      keyring.fail(KeyringAccess.Operation.Get, new RuntimeException("backend read detail"))
      run(vault.get("morphir", "github.com")).map {
        case Result.Failure(SecretException.LookupFailed(detail)) =>
          assert(!detail.contains("backend read detail"))
          assert(keyring.calls == Seq(KeyringAccess.Call.Get("morphir", "github.com")))
        case _ => assert(false)
      }
    }

    "keeps a keyring unavailability failure typed as NotAvailable" in {
      val (vault, keyring) = makeVault()
      keyring.fail(KeyringAccess.Operation.Get, SecretException.NotAvailable("not linked"))
      run(vault.get("morphir", "github.com")).map {
        case Result.Failure(SecretException.NotAvailable("not linked")) => assert(true)
        case _                                                          => assert(false)
      }
    }

    "maps a failed write without exposing the backend message" in {
      val (vault, keyring) = makeVault()
      keyring.fail(KeyringAccess.Operation.Set, new RuntimeException("backend write detail"))
      Secret.fromStored("remembered-token") match
        case Present(secret) =>
          run(vault.put("morphir", "github.com", secret)).map {
            case Result.Failure(error @ SecretException.MutationFailed("set")) =>
              assert(!error.getMessage.contains("backend write detail"))
              assert(keyring.calls == Seq(KeyringAccess.Call.Set("morphir", "github.com")))
            case _ => assert(false)
          }
        case Absent => assert(false)
    }

    "maps a failed delete without exposing the backend message" in {
      val (vault, keyring) = makeVault()
      keyring.fail(KeyringAccess.Operation.Delete, new RuntimeException("backend delete detail"))
      run(vault.remove("morphir", "github.com")).map {
        case Result.Failure(error @ SecretException.MutationFailed("delete")) =>
          assert(!error.getMessage.contains("backend delete detail"))
          assert(keyring.calls == Seq(KeyringAccess.Call.Delete("morphir", "github.com")))
        case _ => assert(false)
      }
    }
  }
