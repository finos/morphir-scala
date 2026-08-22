package millbuild

import utest.*

object JsTestSelectorsTests extends TestSuite {
  val tests = Tests {
    test("partition splits desktop module roots from platform") {
      val resolved = Seq(
        "morphir.ui.js.compile",
        "morphir.ui.js.test.compile",
        "morphir.ui.wasm.test",
        "morphir.desktop.main.js.compile",
        "morphir.desktop.main.js.test",
        "morphir.appkit.electron.js.compile",
        "morphir.appkit.electron.js.publishArtifacts",
        "morphir.appkit.js.compile",
        "morphir.langkit.core.js.compile",
        "morphir.langkit.core.js.publishArtifacts",
        "morphir.prelude.js.compile",
        "morphir.prelude.wasm.test"
      )

      val split = JsTestSelectors.partition(resolved)
      assert(
        split.desktop == Seq(
          "morphir.ui.js.compile",
          "morphir.ui.js.test.compile",
          "morphir.ui.wasm.test",
          "morphir.desktop.main.js.compile",
          "morphir.desktop.main.js.test",
          "morphir.appkit.electron.js.compile",
          "morphir.appkit.electron.js.publishArtifacts"
        )
      )
      assert(
        split.platform == Seq(
          "morphir.appkit.js.compile",
          "morphir.langkit.core.js.compile",
          "morphir.langkit.core.js.publishArtifacts",
          "morphir.prelude.js.compile",
          "morphir.prelude.wasm.test"
        )
      )

      assert(split.desktop.size + split.platform.size == resolved.size)
      assert((split.desktop.toSet & split.platform.toSet).isEmpty)
      assert((split.desktop.toSet ++ split.platform.toSet) == resolved.toSet)
    }

    test("isDesktopTask uses dotted-segment prefix match") {
      assert(!JsTestSelectors.isDesktopTask("morphir.uiThing.js.compile"))
      assert(!JsTestSelectors.isDesktopTask("morphir.appkit.js.compile"))
      assert(JsTestSelectors.isDesktopTask("morphir.appkit.electron.js.compile"))
      assert(JsTestSelectors.isDesktopTask("morphir.ui.js.compile"))
    }

    test("selectGroup returns buckets or fails loudly") {
      val resolved = Seq(
        "morphir.ui.js.compile",
        "morphir.appkit.js.compile",
        "morphir.prelude.js.compile"
      )
      val split = JsTestSelectors.partition(resolved)

      assert(JsTestSelectors.selectGroup(resolved, "desktop", "test") == Right(split.desktop))
      assert(JsTestSelectors.selectGroup(resolved, "platform", "test") == Right(split.platform))

      val unknownGroup = JsTestSelectors.selectGroup(resolved, "bogus", "ci.testJs")
      assert(unknownGroup.isLeft)
      assert(unknownGroup.swap.exists(_.contains("unknown group 'bogus'")))

      val emptyDesktop = JsTestSelectors.selectGroup(
        Seq("morphir.appkit.js.compile", "morphir.prelude.js.compile"),
        "desktop",
        "ci.testJs"
      )
      assert(emptyDesktop.isLeft)
      assert(emptyDesktop.swap.exists(_.contains("no targets remain for group 'desktop'")))
    }
  }
}
