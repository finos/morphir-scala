//| moduleDeps: ["//mill-plugins/morphir/elm/src/org/finos/morphir/mill/elm/morphir/MorphirElmTool.scala"]

package org.finos.millmorphir.elm

object MorphirElmTool {
  val Version   = org.finos.morphir.mill.elm.morphir.MorphirElmTool.Version
  val Resolved  = org.finos.morphir.mill.elm.morphir.MorphirElmTool.Resolved
  val Sha1      = org.finos.morphir.mill.elm.morphir.MorphirElmTool.Sha1
  val Integrity = org.finos.morphir.mill.elm.morphir.MorphirElmTool.Integrity
}

object MorphirElmCommand {
  def npmCi(node: os.Path, npmCli: os.Path, cache: os.Path): Seq[String] =
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

  def cli(node: os.Path, install: os.Path, args: Seq[String]): Seq[String] =
    Seq(
      node.toString,
      (install / "node_modules" / "morphir-elm" / "cli" / "morphir-elm.js").toString
    ) ++ args
}
