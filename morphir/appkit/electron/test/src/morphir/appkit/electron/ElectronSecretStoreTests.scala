package morphir.appkit.electron

import kyo.*
import kyo.test.*
import morphir.appkit.*

class ElectronSecretStoreTests extends Test[Any]:

  val cipher = new SecretCipher:
    def decrypt(bytes: Chunk[Byte]): String < (Abort[SecretException] & Async) =
      new String(bytes.toArray, "UTF-8").reverse

  def blobsOf(entries: ((String, String), String)*): SecretBlobs =
    new SecretBlobs:
      def read(service: String, account: String): Maybe[Chunk[Byte]] < (Abort[SecretException] & Async) =
        Maybe.fromOption(entries.toMap.get((service, account)).map(s => Chunk.from(s.getBytes("UTF-8"))))

  "ElectronSecretStore" - {

    "decrypts a present blob into a Secret" in {
      val store = ElectronSecretStore(cipher, blobsOf(("github", "damian") -> "nekot"))
      store.get("github", "damian").map { result =>
        assert(result.map(_ => "present") == Maybe("present"))
      }
    }

    "a cipher that throws fails through the typed channel, not as a panic" in {
      val throwing = new SecretCipher:
        def decrypt(bytes: Chunk[Byte]): String < (Abort[SecretException] & Async) =
          Abort.catching[Throwable](failure => SecretException.LookupFailed(failure.getMessage))(
            Sync.defer(throw new RuntimeException("key changed"))
          )
      val store = ElectronSecretStore(throwing, blobsOf(("github", "damian") -> "nekot"))
      Abort.run[SecretException](store.get("github", "damian")).map { result =>
        assert(result.failure.exists(_.isInstanceOf[SecretException.LookupFailed]))
      }
    }

    "a missing blob is Absent, not an error" in {
      val store = ElectronSecretStore(cipher, blobsOf())
      store.get("github", "damian").map(result => assert(result == Absent))
    }
  }
end ElectronSecretStoreTests
