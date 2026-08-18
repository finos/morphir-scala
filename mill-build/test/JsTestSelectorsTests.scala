//| moduleDeps: ["//mill-build/src/millbuild/JsTestSelectors.scala"]

import millbuild.JsTestSelectors

def assertEquals[A](actual: A, expected: A): Unit =
  assert(actual == expected, s"Expected $expected, got $actual")

@main def runJsTestSelectorsTests(): Unit =
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

  // Real module roots go to the desktop bucket; everything else stays on the platform bucket.
  val split = JsTestSelectors.partition(resolved)
  assertEquals(
    split.desktop,
    Seq(
      "morphir.ui.js.compile",
      "morphir.ui.js.test.compile",
      "morphir.ui.wasm.test",
      "morphir.desktop.main.js.compile",
      "morphir.desktop.main.js.test",
      "morphir.appkit.electron.js.compile",
      "morphir.appkit.electron.js.publishArtifacts"
    )
  )
  assertEquals(
    split.platform,
    Seq(
      "morphir.appkit.js.compile",
      "morphir.langkit.core.js.compile",
      "morphir.langkit.core.js.publishArtifacts",
      "morphir.prelude.js.compile",
      "morphir.prelude.wasm.test"
    )
  )

  // Exhaustive and disjoint: every input selector appears in exactly one bucket, and the two
  // buckets recombine (as sets) to the original input.
  assertEquals(split.desktop.size + split.platform.size, resolved.size)
  assertEquals((split.desktop.toSet & split.platform.toSet), Set.empty)
  assertEquals((split.desktop.toSet ++ split.platform.toSet), resolved.toSet)

  // A dotted-segment prefix match, not a plain substring one: neither a look-alike module name nor
  // a sibling of morphir.appkit.electron's parent should be swept into the desktop bucket.
  assertEquals(JsTestSelectors.isDesktopTask("morphir.uiThing.js.compile"), false)
  assertEquals(JsTestSelectors.isDesktopTask("morphir.appkit.js.compile"), false)
  assertEquals(JsTestSelectors.isDesktopTask("morphir.appkit.electron.js.compile"), true)
  assertEquals(JsTestSelectors.isDesktopTask("morphir.ui.js.compile"), true)

  // selectGroup: known groups return their bucket ...
  assertEquals(JsTestSelectors.selectGroup(resolved, "desktop", "test"), Right(split.desktop))
  assertEquals(JsTestSelectors.selectGroup(resolved, "platform", "test"), Right(split.platform))

  // ... an unknown group name fails loudly ...
  val unknownGroup = JsTestSelectors.selectGroup(resolved, "bogus", "ci.testJs")
  assert(unknownGroup.isLeft, s"Expected an unknown group to fail, got $unknownGroup")
  assert(
    unknownGroup.swap.exists(_.contains("unknown group 'bogus'")),
    s"Expected an unknown-group error, got $unknownGroup"
  )

  // ... and an empty bucket fails loudly rather than silently running zero targets.
  val emptyDesktop = JsTestSelectors.selectGroup(
    Seq("morphir.appkit.js.compile", "morphir.prelude.js.compile"),
    "desktop",
    "ci.testJs"
  )
  assert(emptyDesktop.isLeft, s"Expected an empty desktop bucket to fail, got $emptyDesktop")
  assert(
    emptyDesktop.swap.exists(_.contains("no targets remain for group 'desktop'")),
    s"Expected a no-targets error, got $emptyDesktop"
  )
