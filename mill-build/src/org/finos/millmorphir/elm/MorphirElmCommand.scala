package org.finos.millmorphir.elm

object MorphirElmTool {
  val Version   = "2.89.0"
  val Resolved  = "https://registry.npmjs.org/morphir-elm/-/morphir-elm-2.89.0.tgz"
  val Sha1      = "120f53263928077575b2be8f12c9d668b561c1f1"
  val Integrity =
    "sha512-ZXvRC4YvGrbYhaC/rKbJ2wvqEN3RpjbRMuAozpvlSWA+dsuAPoK5rNEtG3Wj1zgBqVtUsE9Jo6G6vsQRVpoa1A=="
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
