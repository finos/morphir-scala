package org.finos.morphir.mill

private[mill] object PluginLayout {
  val artifacts: Seq[(String, String)] = Seq(
    "mill-morphir-toolchain"   -> "org.finos.morphir.mill.toolchain",
    "mill-morphir-javascript"  -> "org.finos.morphir.mill.javascript",
    "mill-morphir-elm-tooling" -> "org.finos.morphir.mill.elm",
    "mill-morphir-core"        -> "org.finos.morphir.mill",
    "mill-morphir-elm"         -> "org.finos.morphir.mill.elm.morphir"
  )
}
