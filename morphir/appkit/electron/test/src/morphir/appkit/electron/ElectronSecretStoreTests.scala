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

    "a missing blob is Absent, not an error" in {
      val store = ElectronSecretStore(cipher, blobsOf())
      store.get("github", "damian").map(result => assert(result == Absent))
    }
  }
end ElectronSecretStoreTests
