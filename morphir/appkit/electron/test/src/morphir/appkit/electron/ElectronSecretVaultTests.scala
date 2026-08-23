package morphir.appkit.electron

import kyo.*
import kyo.test.*
import morphir.appkit.*

class ElectronSecretVaultTests extends Test[Any]:

  private def secret(raw: String): Secret =
    Secret.fromStored(raw) match
      case Present(value) => value
      case Absent         => throw new IllegalArgumentException("test secrets must be non-empty")

  private def run[A](effect: A < (Abort[SecretException] & Async)): Result[SecretException, A] < Async =
    Abort.run[SecretException](effect)

  private final class FakeCipher(var corrupt: Boolean = false) extends SecretCipher:
    def available(): Boolean < Async = true

    def encrypt(value: Secret): Span[Byte] < (Async & Abort[SecretException]) =
      Span.from(value.unsafeReveal.reverse.getBytes("UTF-8"))

    def decrypt(bytes: Span[Byte]): Secret < (Async & Abort[SecretException]) =
      if corrupt then Abort.fail(SecretException.LookupFailed("Ciphertext could not be decrypted"))
      else secret(new String(bytes.toArray, "UTF-8").reverse)

  private final class FakeBlobs extends SecretBlobs:
    val entries   = scala.collection.mutable.Map.empty[(String, String), Span[Byte]]
    var failRead  = false
    var failWrite = false

    def read(service: String, account: String): Maybe[Span[Byte]] < (Async & Abort[SecretException]) =
      if failRead then Abort.fail(SecretException.LookupFailed("Blob lookup failed"))
      else Maybe.fromOption(entries.get((service, account)))

    def writeAtomically(
        service: String,
        account: String,
        bytes: Span[Byte]
    ): Unit < (Async & Abort[SecretException]) =
      if failWrite then Abort.fail(SecretException.MutationFailed("write"))
      else entries((service, account)) = Span.from(bytes.toArray)

    def delete(service: String, account: String): Unit < (Async & Abort[SecretException]) =
      entries.remove((service, account))
      ()

  private final class FakeSafeStorage(
      var isAvailable: Boolean = true,
      var shouldReEncrypt: Boolean = false
  ) extends SecretCipher.SafeStorageApi:
    val encryptions = scala.collection.mutable.ListBuffer.empty[String]

    def asyncEncryptionAvailable(): Result[Throwable, Boolean] < Async =
      Result.succeed(isAvailable)

    def encryptString(value: String): Result[Throwable, Span[Byte]] < Async =
      encryptions += value
      Result.succeed(Span.from(s"new:$value".getBytes("UTF-8")))

    def decryptString(bytes: Span[Byte]): Result[Throwable, SecretCipher.DecryptedString] < Async =
      Result.succeed(SecretCipher.DecryptedString("remembered-token", shouldReEncrypt))

  private final class FakeFileSystem extends SecretBlobs.FileSystem:
    val files                         = scala.collection.mutable.Map.empty[String, Span[Byte]]
    val events                        = scala.collection.mutable.ListBuffer.empty[String]
    var failWrite                     = false
    var failSync                      = false
    var failClose                     = false
    var failReplace                   = false
    var failDelete                    = false
    private var opened: Maybe[String] = Absent

    def read(path: String): Maybe[Span[Byte]] = Maybe.fromOption(files.get(path))

    def siblingTemporary(path: String): String = s"$path.tmp"

    def openForWrite(path: String): Int =
      events += s"open:$path"
      opened = Present(path)
      files(path) = Span.empty
      7

    def write(handle: Int, bytes: Span[Byte]): Unit =
      events += s"write:$handle"
      if failWrite then throw new RuntimeException("write failed")
      opened match
        case Present(path) => files(path) = Span.from(bytes.toArray)
        case Absent        => throw new IllegalStateException("no open temporary file")

    def sync(handle: Int): Unit =
      events += s"sync:$handle"
      if failSync then throw new RuntimeException("sync failed")

    def close(handle: Int): Unit =
      events += s"close:$handle"
      if failClose then throw new RuntimeException("close failed")
      opened = Absent

    def replace(from: String, to: String): Unit =
      events += s"replace:$from->$to"
      if failReplace then throw new RuntimeException("replace failed")
      files(to) = files(from)
      val _ = files.remove(from)

    def delete(path: String): Unit =
      events += s"delete:$path"
      if failDelete then throw new RuntimeException("delete failed")
      files.remove(path)
      ()

  "ElectronSecretVault" - {

    "round trips a remembered secret through encrypted blobs" in {
      val cipher = FakeCipher()
      val blobs  = FakeBlobs()
      ElectronSecretVault.available(cipher, blobs, isLinux = false, selectedBackend = () => "unknown").map {
        case Present(vault) =>
          run(vault.put("org.finos.morphir", "github.com", secret("remembered-token"))).map {
            case Result.Success(()) =>
              run(vault.get("org.finos.morphir", "github.com")).map {
                case Result.Success(Present(stored)) =>
                  assert(stored == secret("remembered-token"))
                  assert(stored.toString == "Secret(redacted)")
                case _ => assert(false)
              }
            case _ => assert(false)
          }
        case Absent => assert(false)
      }
    }

    "does not offer persistence when asynchronous encryption is unavailable" in {
      val cipher = SecretCipher.safeStorage(FakeSafeStorage(isAvailable = false))
      ElectronSecretVault.available(cipher, FakeBlobs(), isLinux = false, selectedBackend = () => "unknown").map {
        vault => assert(vault == Absent)
      }
    }

    "rejects basic_text and unknown backends on Linux" in {
      val cipher = FakeCipher()
      ElectronSecretVault.available(cipher, FakeBlobs(), isLinux = true, selectedBackend = () => "basic_text").map {
        basicText =>
          ElectronSecretVault.available(cipher, FakeBlobs(), isLinux = true, selectedBackend = () => "unknown").map {
            unknown => assert(basicText == Absent && unknown == Absent)
          }
      }
    }

    "does not apply the synchronous backend proxy outside Linux" in
      ElectronSecretVault
        .available(FakeCipher(), FakeBlobs(), isLinux = false, selectedBackend = () => "basic_text")
        .map {
          case Present(_) => assert(true)
          case Absent     => assert(false)
        }

    "maps failed and corrupt blobs through the typed lookup channel" in {
      val cipher = FakeCipher(corrupt = true)
      val blobs  = FakeBlobs()
      blobs.entries(("org.finos.morphir", "github.com")) = Span(1.toByte)
      blobs.failRead = true
      ElectronSecretVault.available(cipher, blobs, isLinux = false, selectedBackend = () => "unknown").map {
        case Present(vault) =>
          run(vault.get("org.finos.morphir", "github.com")).map { failedRead =>
            blobs.failRead = false
            run(vault.get("org.finos.morphir", "github.com")).map { corrupt =>
              assert(failedRead.failure.exists(_.isInstanceOf[SecretException.LookupFailed]))
              assert(corrupt.failure.exists(_.isInstanceOf[SecretException.LookupFailed]))
            }
          }
        case Absent => assert(false)
      }
    }

    "rewrites ciphertext atomically when Electron requests re-encryption" in {
      val safeStorage = FakeSafeStorage(shouldReEncrypt = true)
      val blobs       = FakeBlobs()
      blobs.entries(("org.finos.morphir", "github.com")) = Span.from("old:ciphertext".getBytes("UTF-8"))
      val cipher = SecretCipher.safeStorage(safeStorage)
      ElectronSecretVault.available(cipher, blobs, isLinux = false, selectedBackend = () => "unknown").map {
        case Present(vault) =>
          run(vault.get("org.finos.morphir", "github.com")).map {
            case Result.Success(Present(stored)) =>
              assert(stored == secret("remembered-token"))
              assert(safeStorage.encryptions.toList == List("remembered-token"))
              assert(
                new String(blobs.entries(("org.finos.morphir", "github.com")).toArray, "UTF-8") ==
                  "new:remembered-token"
              )
            case _ => assert(false)
          }
        case Absent => assert(false)
      }
    }

    "fails a write without replacing an existing blob" in {
      val cipher = FakeCipher()
      val blobs  = FakeBlobs()
      blobs.entries(("org.finos.morphir", "github.com")) = Span.from("existing".getBytes("UTF-8"))
      blobs.failWrite = true
      ElectronSecretVault.available(cipher, blobs, isLinux = false, selectedBackend = () => "unknown").map {
        case Present(vault) =>
          run(vault.put("org.finos.morphir", "github.com", secret("replacement"))).map { result =>
            assert(result.failure.exists(_.isInstanceOf[SecretException.MutationFailed]))
            assert(new String(blobs.entries(("org.finos.morphir", "github.com")).toArray, "UTF-8") == "existing")
          }
        case Absent => assert(false)
      }
    }

    "removes remembered ciphertext" in {
      val cipher = FakeCipher()
      val blobs  = FakeBlobs()
      blobs.entries(("org.finos.morphir", "github.com")) = Span.from("stored".getBytes("UTF-8"))
      ElectronSecretVault.available(cipher, blobs, isLinux = false, selectedBackend = () => "unknown").map {
        case Present(vault) =>
          run(vault.remove("org.finos.morphir", "github.com")).map { result =>
            assert(result.isSuccess)
            assert(!blobs.entries.contains(("org.finos.morphir", "github.com")))
          }
        case Absent => assert(false)
      }
    }
  }

  "SecretBlobs.writeAtomically" - {

    "syncs and closes a sibling temporary file before replacing the target" in {
      val fs    = FakeFileSystem()
      val blobs = SecretBlobs.fileSystem("/vault", fs)
      run(blobs.writeAtomically("org.finos.morphir", "github.com", Span(1.toByte, 2.toByte))).map { result =>
        val writeIndex   = fs.events.indexWhere(_.startsWith("write:"))
        val syncIndex    = fs.events.indexWhere(_.startsWith("sync:"))
        val closeIndex   = fs.events.indexWhere(_.startsWith("close:"))
        val replaceIndex = fs.events.indexWhere(_.startsWith("replace:"))
        assert(result.isSuccess)
        assert(writeIndex >= 0 && writeIndex < syncIndex && syncIndex < closeIndex && closeIndex < replaceIndex)
        assert(fs.files.keys.count(_.endsWith(".tmp")) == 0)
        assert(fs.files.values.exists(_.toArray.toSeq == Seq(1.toByte, 2.toByte)))
      }
    }

    "cleans the sibling temporary file when replacement fails" in {
      val fs    = FakeFileSystem()
      val blobs = SecretBlobs.fileSystem("/vault", fs)
      fs.failReplace = true
      run(blobs.writeAtomically("org.finos.morphir", "github.com", Span(1.toByte))).map { result =>
        assert(result.failure.exists(_.isInstanceOf[SecretException.MutationFailed]))
        assert(fs.files.keys.count(_.endsWith(".tmp")) == 0)
        assert(fs.events.exists(event => event.startsWith("delete:") && event.endsWith(".tmp")))
      }
    }

    "closes and cleans the sibling temporary file when writing fails" in {
      val fs    = FakeFileSystem()
      val blobs = SecretBlobs.fileSystem("/vault", fs)
      fs.failWrite = true
      run(blobs.writeAtomically("org.finos.morphir", "github.com", Span(1.toByte))).map { result =>
        assert(result == Result.Failure(SecretException.MutationFailed("write")))
        assert(fs.events.exists(_.startsWith("close:")))
        assert(fs.events.exists(_.startsWith("delete:")))
        assert(fs.files.keys.count(_.endsWith(".tmp")) == 0)
      }
    }

    "closes and cleans the sibling temporary file when syncing fails" in {
      val fs    = FakeFileSystem()
      val blobs = SecretBlobs.fileSystem("/vault", fs)
      fs.failSync = true
      run(blobs.writeAtomically("org.finos.morphir", "github.com", Span(1.toByte))).map { result =>
        assert(result == Result.Failure(SecretException.MutationFailed("write")))
        assert(fs.events.exists(_.startsWith("close:")))
        assert(fs.events.exists(_.startsWith("delete:")))
        assert(fs.files.keys.count(_.endsWith(".tmp")) == 0)
      }
    }

    "attempts sibling temporary cleanup when closing fails" in {
      val fs    = FakeFileSystem()
      val blobs = SecretBlobs.fileSystem("/vault", fs)
      fs.failClose = true
      run(blobs.writeAtomically("org.finos.morphir", "github.com", Span(1.toByte))).map { result =>
        assert(result == Result.Failure(SecretException.MutationFailed("write")))
        assert(fs.events.count(_.startsWith("close:")) >= 2)
        assert(fs.events.exists(_.startsWith("delete:")))
      }
    }

    "keeps the write failure primary when temporary cleanup also fails" in {
      val fs    = FakeFileSystem()
      val blobs = SecretBlobs.fileSystem("/vault", fs)
      fs.failWrite = true
      fs.failDelete = true
      run(blobs.writeAtomically("org.finos.morphir", "github.com", Span(1.toByte))).map { result =>
        assert(result == Result.Failure(SecretException.MutationFailed("write")))
        assert(fs.events.exists(_.startsWith("delete:")))
        assert(fs.files.keys.count(_.endsWith(".tmp")) == 1)
      }
    }

    "deletes a stored blob" in {
      val fs    = FakeFileSystem()
      val blobs = SecretBlobs.fileSystem("/vault", fs)
      run(blobs.writeAtomically("org.finos.morphir", "github.com", Span(9.toByte))).map { written =>
        run(blobs.delete("org.finos.morphir", "github.com")).map { deleted =>
          run(blobs.read("org.finos.morphir", "github.com")).map { read =>
            assert(written.isSuccess && deleted.isSuccess && read == Result.Success(Absent))
          }
        }
      }
    }
  }
end ElectronSecretVaultTests
