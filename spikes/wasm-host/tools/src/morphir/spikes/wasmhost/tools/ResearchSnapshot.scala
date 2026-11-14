package morphir.spikes.wasmhost.tools

import java.nio.file.Path

/** Publishes a text-only evidence snapshot on an explicitly exploratory branch. */
object ResearchSnapshot:
  def main(args: Array[String]): Unit =
    val source = os.Path(Path.of(args(0)).toRealPath())
    val target = os.Path(Path.of(args(1)).toAbsolutePath.normalize())
    require(!os.exists(target), "snapshot destination already exists; choose a fresh directory")
    val workspace = source.toString.take(source.toString.indexOf("/.dev/"))
    require(workspace.nonEmpty, "expected retained evidence under the workspace .dev directory")
    val textExtensions = Set("html", "json", "jsonl", "csv", "log", "txt", "xml", "h", "mbt", "wat")
    val localHome      = "/Users/[^/\\s\"<>]+".r
    val credentials    =
      "(?i)(github_pat_[a-z0-9_]{20,}|gh[pousr]_[a-z0-9]{30,}|-----BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY-----)".r
    val included = Vector.newBuilder[ujson.Value]
    val omitted  = Vector.newBuilder[ujson.Value]
    os.makeDir.all(target)
    Distribution.files(source).foreach { file =>
      val relative  = file.relativeTo(source)
      val name      = relative.toString
      val inventory = ujson.Obj("path" -> name, "bytes" -> os.size(file), "originalSha256" -> Distribution.hash(file))
      val payload   = name.startsWith("polyglot-compiler/compiler-payload/")
      val delivery  = name.startsWith("polyglot-compiler/distribution/polyglot-jvm/") ||
        name.startsWith("polyglot-compiler/distribution/polyglot-native/")
      val keep = !payload && !delivery &&
        (textExtensions(file.ext.toLowerCase(java.util.Locale.ROOT)) || Set("LICENSE", "CORE-LICENSE")(file.last))
      if keep then
        val original  = os.read(file)
        val published = localHome.replaceAllIn(original.replace(workspace, "WORKSPACE"), "HOME")
          .replaceAll("(?m)[ \\t]+$", "")
        require(credentials.findFirstIn(published).isEmpty, s"credential-like content in $relative")
        val destination = target / relative
        os.write(destination, published, createFolders = true)
        inventory("snapshotBytes") = os.size(destination)
        inventory("snapshotSha256") = Distribution.hash(destination)
        inventory("textTransformed") = (published != original)
        included += inventory
      else
        inventory("reason") =
          "downloaded payload, executable, archive or non-report binary; reproduce through pinned Mill tasks"
        omitted += inventory
    }
    val files = included.result()
    val links = "(?:href|src)=[\"']([^\"']+)[\"']".r
    files.filter(_("path").str.endsWith(".html")).foreach { entry =>
      val page = target / os.RelPath(entry("path").str)
      links.findAllMatchIn(os.read(page)).foreach { matched =>
        val link = matched.group(1)
        if link.nonEmpty && !link.startsWith("#") && !link.contains(":") then
          val path = link.takeWhile(char => char != '#' && char != '?')
          require(os.exists(page / os.up / os.RelPath(path)), s"missing report link ${entry("path").str}: $link")
      }
    }
    os.write(
      target / "snapshot-manifest.json",
      ujson.Obj(
        "scope"      -> "Exploration-only evidence snapshot; not a release or merge candidate",
        "textPolicy" ->
          "Absolute workspace and macOS home prefixes replaced by WORKSPACE and HOME; trailing line whitespace removed; measured values unchanged",
        "hashPolicy" ->
          "Original hashes identify retained local evidence; snapshot hashes identify the published text after path and whitespace normalization",
        "binaryPolicy" ->
          "Downloaded runtimes, JARs, compiler payloads, executables and generated Wasm omitted; inventories retain identities and sizes",
        "included"      -> ujson.Arr.from(files),
        "omitted"       -> ujson.Arr.from(omitted.result()),
        "snapshotBytes" -> files.map(value => Benchmarks.number(value("snapshotBytes"))).sum
      ).render(indent = 2)
    )
    println(s"Published ${files.size} report files; relative HTML links and credential checks passed")
