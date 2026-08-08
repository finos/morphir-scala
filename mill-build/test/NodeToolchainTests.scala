//| moduleDeps: ["//mill-plugins/morphir/javascript/src/org/finos/morphir/mill/javascript/node/NodeDistribution.scala", "//mill-plugins/morphir/toolchain/src/org/finos/morphir/mill/toolchain/AcquisitionSettings.scala"]

import org.finos.morphir.mill.javascript.node.NodeDistribution

def legacyPath(target: String): java.nio.file.Path = {
  val workspace = java.nio.file.Paths.get(System.getProperty("user.dir"))
  val result = os.proc(
    workspace.resolve("mill").toString,
    "--no-daemon",
    "--no-build-lock",
    "--ticker",
    "false",
    "show",
    target
  ).call(stdout = os.Pipe, stderr = os.Inherit)
  val reference  = ujson.read(result.out.text().linesIterator.filter(_.nonEmpty).toSeq.last).str
  val marker     = "../mill-workspace/"
  val normalized = reference.replace('\\', '/')
  assert(normalized.contains(marker), s"Unexpected Mill PathRef for $target: $reference")
  workspace.resolve(normalized.substring(normalized.indexOf(marker) + marker.length))
}

@main def runNodeToolchainTests(): Unit = {
  assert(NodeDistribution.Version == "24.19.0")
  val distribution = NodeDistribution
    .resolve(System.getProperty("os.name"), System.getProperty("os.arch"))
    .fold(message => throw new java.lang.AssertionError(message), identity)
  val home       = legacyPath("toolchains.node.nodeHome")
  val executable = legacyPath("toolchains.node.nodeExecutable")
  val npmCli     = legacyPath("toolchains.node.npmCli")
  assert(executable == home.resolve(distribution.nodeRelativePath.toString))
  assert(npmCli == home.resolve(distribution.npmCliRelativePath.toString))
  assert(java.nio.file.Files.isRegularFile(executable))
  assert(java.nio.file.Files.isRegularFile(npmCli))
  val version = os.proc(executable.toString, "--version").call(stdout = os.Pipe).out.text().trim
  assert(version == s"v${NodeDistribution.Version}", s"Unexpected Node version: $version")
}
