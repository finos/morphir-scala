//| moduleDeps: ["//mill-plugins/morphir/javascript/src/org/finos/morphir/mill/javascript/node/NodeDistribution.scala", "//mill-plugins/morphir/toolchain/src/org/finos/morphir/mill/toolchain/AcquisitionSettings.scala"]

import org.finos.morphir.mill.javascript.node.NodeDistribution

@main def runNodeToolchainTests(): Unit = {
  assert(NodeDistribution.Version == "24.19.0")
}
