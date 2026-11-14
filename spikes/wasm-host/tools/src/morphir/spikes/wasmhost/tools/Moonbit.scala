package morphir.spikes.wasmhost.tools

import java.nio.file.Path

/** Source-build acquisition when the immutable MoonBit revision has no portable release asset. */
object Moonbit:
  def main(args: Array[String]): Unit =
    val output = os.Path(Path.of(args(0)).toAbsolutePath)
    os.makeDir.all(output)
    val source   = output / "source"
    val revision = "661e770e7af0d40b0f7cb44909e3d18e6c11b7cf"
    os.proc("git", "init", source).call()
    os.proc("git", "-C", source, "remote", "add", "origin", "https://github.com/finos/morphir-moonbit.git").call()
    os.proc("git", "-C", source, "fetch", "--depth", "1", "origin", revision).call(timeout = 180000)
    os.proc("git", "-C", source, "checkout", "--detach", "FETCH_HEAD").call()
    require(
      os.proc("git", "-C", source, "rev-parse", "HEAD").call().out.text().trim == revision,
      "source revision mismatch"
    )
    // The upstream workspace resolves host-only dependencies even for a portable guest build.
    // Select the extension's local module closure without changing any module or guest sources.
    val moduleName = "(?m)^name = \"([^\"]+)\"".r
    val dependency = "\"([^\"@]+)@[^\"]+\"".r
    val modules    = os.walk(source).filter(path => os.isFile(path) && path.last == "moon.mod").flatMap { path =>
      val text = os.read(path)
      moduleName.findFirstMatchIn(text).map(m =>
        m.group(1) -> (path / os.up, dependency.findAllMatchIn(text).map(_.group(1)).toSet)
      )
    }.toMap
    @annotation.tailrec
    def closure(names: Set[String]): Set[String] =
      val expanded = names ++ names.flatMap(name => modules(name)._2.intersect(modules.keySet))
      if expanded == names then names else closure(expanded)
    val selected = closure(Set("finos/morphir-extension"))
    os.copy.over(source / "moon.work", output / "workspace-original.txt")
    os.write.over(
      source / "moon.work",
      "members = [\n" + selected.toVector.sorted.map(name =>
        "  \"./" + modules(name)._1.relativeTo(source).toString + "\",\n"
      ).mkString + "]\n"
    )
    os.copy.over(source / "moon.work", output / "workspace-build.txt")
    val identity = os.proc("moon", "version", "--all").call().out.text()
    val build    = os.proc("moon", "build", "--target", "wasm", "--release", "apps/morphir-extension/portable")
      .call(cwd = source, timeout = 180000, check = false, mergeErrIntoOut = true)
    os.write(output / "build.log", build.out.text())
    os.write(output / "toolchain.txt", identity)
    val artifacts = if os.exists(source / "_build") then
      os.walk(source / "_build").filter(path => os.isFile(path) && path.ext == "wasm")
    else Seq.empty
    val result = ujson.Obj(
      "sourceRevision" -> revision,
      "toolchain"      -> identity,
      "exit"           -> build.exitCode,
      "status"         -> (if build.exitCode == 0 && artifacts.nonEmpty then "built" else "blocked"),
      "artifacts"      -> ujson.Arr.from(artifacts.map(path =>
        ujson.Obj(
          "path"   -> path.toString,
          "sha256" -> Distribution.hash(path),
          "bytes"  -> os.size(path)
        )
      ))
    )
    os.write(output / "receipt.json", result.render(indent = 2))
