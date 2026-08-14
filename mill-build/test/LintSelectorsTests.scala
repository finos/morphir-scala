//| moduleDeps: ["//mill-build/src/millbuild/LintSelectors.scala"]

import millbuild.LintSelectors

def assertEquals[A](actual: A, expected: A): Unit =
  assert(actual == expected, s"Expected $expected, got $actual")

@main def runLintSelectorsTests(): Unit =
  val resolved = Seq(
    "morphir.langkit.jvm.sources",
    "morphir.prelude.jvm.sources",
    "morphir.tests.jvm.test.sources",
    "morphir.langkit.jvm.compile"
  )
  val modules = LintSelectors.modulesFromResolved(resolved)
  assertEquals(
    modules,
    Seq("morphir.langkit.jvm", "morphir.prelude.jvm", "morphir.tests.jvm.test")
  )

  assertEquals(LintSelectors.excludeMatching(modules, ""), Right(modules))
  assertEquals(LintSelectors.excludeMatching(modules, "   "), Right(modules))
  assertEquals(
    LintSelectors.excludeMatching(modules, "langkit"),
    Right(Seq("morphir.prelude.jvm", "morphir.tests.jvm.test"))
  )
  assertEquals(
    LintSelectors.excludeMatching(modules, "\\.test$"),
    Right(Seq("morphir.langkit.jvm", "morphir.prelude.jvm"))
  )
  assertEquals(LintSelectors.excludeMatching(modules, "morphir\\."), Right(Seq.empty))

  val invalid = LintSelectors.excludeMatching(modules, "(")
  assert(invalid.isLeft, s"Expected invalid regex to fail, got $invalid")
  assert(
    invalid.swap.exists(_.contains("ci.lint --exclude is not a valid regex")),
    s"Expected a ci.lint regex error, got $invalid"
  )

  interceptSpace()

def interceptSpace(): Unit =
  try
    LintSelectors.modulesFromResolved(Seq("morphir.jvm extra.sources"))
    assert(false, "expected space in a resolved name to fail")
  catch
    case _: IllegalArgumentException => ()
