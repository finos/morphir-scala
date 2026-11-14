package morphir.spikes.wasmhost.tools

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.security.MessageDigest
import java.time.Duration

/** Exact upstream guest bytes, kept outside host executable build inputs. */
object Guests:
  private val revision = "661e770e7af0d40b0f7cb44909e3d18e6c11b7cf"
  private val inputs   = Seq(
    (
      "sdk.wasm",
      s"https://raw.githubusercontent.com/finos/morphir-moonbit/$revision/apps/morphir/fixtures/sdk-guest/morphir-sdk-fixture-guest.wasm",
      "f5e1c9cd7a3472b6261c2c3ebfe2c39aeae14da5287dac6cd6ca87ce854901f1"
    ),
    (
      "python.wasm",
      "https://github.com/finos/morphir-python/releases/download/extension/python/v0.6.0/morphir-python-binding-0.6.0.wasm",
      "ce0cba07f0395ff8179b42eb021ade05c68197f69b8e9c0c8bc369d2a035727c"
    ),
    (
      "python-qualification.tar.gz",
      "https://github.com/finos/morphir-python/releases/download/extension/python/v0.6.0/extension-host-qualification-0.6.0.tar.gz",
      "2a19d566508e7cbbea6dc6ea05dae3d0b6c560ed5b5cab3f0ec04c8d0ccfb06b"
    )
  )

  def main(args: Array[String]): Unit =
    val output = os.Path(java.nio.file.Path.of(args(0)).toAbsolutePath)
    os.makeDir.all(output)
    val client   = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()
    val receipts = inputs.map { (name, url, expected) =>
      val request  = HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofMinutes(3)).GET().build()
      val response = client.send(request, HttpResponse.BodyHandlers.ofByteArray())
      require(response.statusCode() == 200, s"guest acquisition failed for $name: ${response.statusCode()}")
      val bytes = response.body()
      val hash  = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
      require(hash == expected, s"guest digest mismatch: $name")
      os.write(output / name, bytes)
      ujson.Obj("file" -> name, "url" -> url, "sha256" -> hash, "bytes" -> bytes.length)
    }
    os.proc("tar", "-xzf", output / "python-qualification.tar.gz", "-C", output).call()
    val licenses = Seq(
      "sdk"    -> s"https://raw.githubusercontent.com/finos/morphir-moonbit/$revision/LICENSE",
      "python" ->
        "https://raw.githubusercontent.com/finos/morphir-python/560a6503b8ffcab87ea667391579b747757bec0d/LICENSE"
    ).map { (name, url) =>
      val response =
        client.send(HttpRequest.newBuilder(URI.create(url)).GET().build(), HttpResponse.BodyHandlers.ofByteArray())
      require(response.statusCode() == 200, s"license acquisition failed: $name")
      os.write(output / s"$name-LICENSE", response.body())
      ujson.Obj("guest" -> name, "source" -> url, "sha256" -> Distribution.hash(output / s"$name-LICENSE"))
    }
    os.write(output / "licenses.json", ujson.Arr.from(licenses).render(indent = 2))
    os.write(output / "inventory.json", ujson.Arr.from(receipts).render(indent = 2))
