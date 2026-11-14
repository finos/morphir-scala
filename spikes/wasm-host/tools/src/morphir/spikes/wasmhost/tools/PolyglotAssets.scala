package morphir.spikes.wasmhost.tools

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Path
import java.time.Duration

/** An isolated, verified compiler-proof JDK; never changes the contributor's Java selection. */
object PolyglotAssets:
  def main(args: Array[String]): Unit =
    require(
      System.getProperty("os.name") == "Mac OS X" && System.getProperty("os.arch") == "aarch64",
      "pinned JDK targets macOS arm64"
    )
    val output = os.Path(Path.of(args(0)).toAbsolutePath)
    os.makeDir.all(output)
    val url =
      "https://github.com/graalvm/graalvm-ce-builds/releases/download/graal-25.5.5.0.3/graalvm-community-jdk-25i5-25.0.5.0.3_macos-aarch64_bin.tar.gz"
    val digest   = "54369d4a5931a5a2101c7c52444a4e7e6647a3a4e041335053cda6fd22295e0c"
    val archive  = output / "graalvm.tar.gz"
    val request  = HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofMinutes(5)).GET().build()
    val response = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()
      .send(request, HttpResponse.BodyHandlers.ofFile(archive.toNIO))
    require(response.statusCode() == 200 && Distribution.hash(archive) == digest, "compiler JDK download/digest failed")
    val unpacked = output / "jdk"
    os.makeDir.all(unpacked)
    os.proc("tar", "-xzf", archive, "-C", unpacked).call()
    val home = os.walk(unpacked).find(path => path.last == "release" && os.isFile(path / os.up / "bin" / "java")).get /
      os.up
    val release = os.read(home / "release")
    require(release.contains("25.0.5.0.3") && release.contains("aarch64"), "compiler JDK release/target mismatch")
    os.write(output / "java-home.txt", home.toNIO.toRealPath().toString)
    os.write(output / "java-version.txt", os.proc(home / "bin" / "java", "--version").call().out.text())
    os.write(output / "native-image-version.txt", os.proc(home / "bin" / "native-image", "--version").call().out.text())
    os.write(
      output / "receipt.json",
      ujson.Obj(
        "graalRelease"   -> "25.5.5.0.3",
        "jdkVersion"     -> "25.0.5.0.3",
        "url"            -> url,
        "archiveSha256"  -> digest,
        "archiveBytes"   -> os.size(archive),
        "home"           -> home.toString,
        "licenses"       -> "LICENSE and legal/ inside the verified JDK distribution",
        "originalMatrix" -> "unchanged; this JDK belongs only to the polyglot compiler proof"
      ).render(indent = 2)
    )
