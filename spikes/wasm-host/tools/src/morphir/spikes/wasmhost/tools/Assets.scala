package morphir.spikes.wasmhost.tools

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.security.MessageDigest
import java.time.Duration

/** Acquires exactly one verified upstream release; runtime loading never searches ambient library paths. */
object Assets:
  private val digest = "a6b1bcdecea3fcb0bcb20908a992d32e4c1f714683d15771735eaa7ef47d6aae"
  private val url    =
    "https://github.com/extism/extism/releases/download/v1.41.0/libextism-aarch64-apple-darwin-v1.41.0.tar.gz"

  def main(args: Array[String]): Unit =
    require(args.length == 1, "assets requires an output directory")
    require(
      System.getProperty("os.name") == "Mac OS X" && System.getProperty("os.arch") == "aarch64",
      "the currently pinned native artifact targets macOS aarch64"
    )
    val output = os.Path(java.nio.file.Path.of(args(0)).toAbsolutePath)
    os.makeDir.all(output)
    val client   = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()
    val request  = HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofMinutes(3)).GET().build()
    val response = client.send(request, HttpResponse.BodyHandlers.ofByteArray())
    require(response.statusCode() == 200, s"native archive download returned ${response.statusCode()}")
    val bytes  = response.body()
    val actual = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
    require(actual == digest, "native archive digest mismatch")
    val archive = output / "libextism.tar.gz"
    os.write(archive, bytes)
    os.proc("tar", "-xzf", archive, "-C", output).call()
    val licenseRequest = HttpRequest.newBuilder(URI.create(
      "https://raw.githubusercontent.com/extism/extism/051b765bb6abe4fe95d5e4f489f03ad7b137edbc/LICENSE"
    )).GET().build()
    val license = client.send(licenseRequest, HttpResponse.BodyHandlers.ofByteArray())
    require(license.statusCode() == 200, "Extism license acquisition failed")
    os.write(output / "LICENSE", license.body())
    os.write(output / "dependencies.txt", os.proc("otool", "-L", output / "libextism.dylib").call().out.text())
    os.write(output / "target.txt", os.proc("file", output / "libextism.dylib").call().out.text())
    val inventory = os.walk(output).filter(os.isFile(_)).map { file =>
      val hash = MessageDigest.getInstance("SHA-256").digest(os.read.bytes(file)).map(b => f"${b & 0xff}%02x").mkString
      ujson.Obj("path" -> file.relativeTo(output).toString, "bytes" -> os.size(file), "sha256" -> hash)
    }
    os.write(
      output / "inventory.json",
      ujson.Obj(
        "release"       -> "v1.41.0",
        "url"           -> url,
        "archiveSha256" -> actual,
        "files"         -> ujson.Arr.from(inventory)
      ).render(indent = 2)
    )
