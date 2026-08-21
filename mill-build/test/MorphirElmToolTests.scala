//| moduleDeps: ["//mill-build/src/org/finos/millmorphir/elm/MorphirElmCommand.scala", "//mill-plugins/morphir/elm/src/org/finos/morphir/mill/elm/morphir/MorphirElmTool.scala"]

import org.finos.millmorphir.elm.*

def assertEquals[A](actual: A, expected: A): Unit =
  assert(actual == expected, s"Expected $expected, got $actual")

@main def runMorphirElmToolTests(): Unit = {
  val toolDirectory = os.pwd / "mill-plugins" / "morphir" / "elm" / "test-tools" / "morphir-elm"
  val manifest      = ujson.read(os.read(toolDirectory / "package.json"))
  val lock          = ujson.read(os.read(toolDirectory / "package-lock.json"))
  val lockedPackage = lock("packages")("node_modules/morphir-elm")

  assertEquals(MorphirElmTool.Version, "2.99.0")
  assertEquals(
    os.read(toolDirectory / "package.json").trim,
    """{"name":"morphir-scala-morphir-elm-tool","private":true,"dependencies":{"morphir-elm":"2.99.0"},"overrides":{"uuid":"^14.0.0"}}"""
  )
  assertEquals(manifest("name").str, "morphir-scala-morphir-elm-tool")
  assert(manifest("private").bool)
  assertEquals(manifest("dependencies")("morphir-elm").str, MorphirElmTool.Version)

  assertEquals(lock("lockfileVersion").num.toInt, 3)
  assertEquals(lock("packages")("")("dependencies")("morphir-elm").str, MorphirElmTool.Version)
  assertEquals(lockedPackage("version").str, MorphirElmTool.Version)
  assertEquals(MorphirElmTool.Resolved, "https://registry.npmjs.org/morphir-elm/-/morphir-elm-2.99.0.tgz")
  assertEquals(MorphirElmTool.Sha1, "064c70fc7df67d2cb554e6242cd09005225817d7")
  assertEquals(
    MorphirElmTool.Integrity,
    "sha512-Ped2tjlJVr4mN/AmKo+TcjYgdV2DKJPB0hOVSktvPG50HLOl90/ik+mP6AvW6B0W9WeFm0ubsjVy0/9wbQPMug=="
  )
  assertEquals(lockedPackage("resolved").str, MorphirElmTool.Resolved)
  assertEquals(lockedPackage("integrity").str, MorphirElmTool.Integrity)
  lock("packages").obj.foreach { case (path, metadata) =>
    if (path.nonEmpty) {
      assert(metadata.obj.contains("resolved"), s"Locked package lacks resolved URL: $path")
      assert(metadata.obj.contains("integrity"), s"Locked package lacks integrity: $path")
    }
  }

  val node    = os.Path("/toolchain/node", os.pwd)
  val npmCli  = os.Path("/toolchain/npm-cli.js", os.pwd)
  val cache   = os.Path("/task/npm-cache", os.pwd)
  val install = os.Path("/task/install", os.pwd)

  assertEquals(
    MorphirElmCommand.npmCi(node, npmCli, cache),
    Seq(
      node.toString,
      npmCli.toString,
      "ci",
      "--ignore-scripts",
      "--no-audit",
      "--no-fund",
      "--cache",
      cache.toString
    )
  )
  assertEquals(
    MorphirElmCommand.cli(node, install, Seq("make", "--project-dir", "/project")),
    Seq(
      node.toString,
      (install / "node_modules" / "morphir-elm" / "cli" / "morphir-elm.js").toString,
      "make",
      "--project-dir",
      "/project"
    )
  )
}
