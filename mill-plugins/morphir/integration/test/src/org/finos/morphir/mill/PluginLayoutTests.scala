package org.finos.morphir.mill

import utest.*

object PluginLayoutTests extends TestSuite {
  val tests = Tests {
    test("publishes the intended artifact and package boundaries") {
      val expected = Seq(
        "mill-morphir-toolchain"   -> "org.finos.morphir.mill.toolchain",
        "mill-morphir-javascript"  -> "org.finos.morphir.mill.javascript",
        "mill-morphir-elm-tooling" -> "org.finos.morphir.mill.elm",
        "mill-morphir-core"        -> "org.finos.morphir.mill",
        "mill-morphir-elm"         -> "org.finos.morphir.mill.elm.morphir"
      )

      assert(PluginLayout.artifacts == expected)
    }
  }
}
