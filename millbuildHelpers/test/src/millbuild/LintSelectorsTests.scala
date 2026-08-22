package millbuild

import utest.*

object LintSelectorsTests extends TestSuite {
  val tests = Tests {
    test("modulesFromResolved keeps .sources paths and strips the suffix") {
      val resolved = Seq(
        "morphir.langkit.jvm.sources",
        "morphir.prelude.jvm.sources",
        "morphir.tests.jvm.test.sources",
        "morphir.langkit.jvm.compile"
      )
      val modules = LintSelectors.modulesFromResolved(resolved)
      assert(modules == Seq("morphir.langkit.jvm", "morphir.prelude.jvm", "morphir.tests.jvm.test"))
    }

    test("excludeMatching blank keeps modules; regex filters; invalid regex fails") {
      val modules = Seq("morphir.langkit.jvm", "morphir.prelude.jvm", "morphir.tests.jvm.test")

      assert(LintSelectors.excludeMatching(modules, "") == Right(modules))
      assert(LintSelectors.excludeMatching(modules, "   ") == Right(modules))
      assert(
        LintSelectors.excludeMatching(modules, "langkit") ==
          Right(Seq("morphir.prelude.jvm", "morphir.tests.jvm.test"))
      )
      assert(
        LintSelectors.excludeMatching(modules, "\\.test$") ==
          Right(Seq("morphir.langkit.jvm", "morphir.prelude.jvm"))
      )
      assert(LintSelectors.excludeMatching(modules, "morphir\\.") == Right(Seq.empty))

      val invalid = LintSelectors.excludeMatching(modules, "(")
      assert(invalid.isLeft)
      assert(invalid.swap.exists(_.contains("ci.lint --exclude is not a valid regex")))
    }

    test("modulesFromResolved rejects spaces in resolved names") {
      try
        LintSelectors.modulesFromResolved(Seq("morphir.jvm extra.sources"))
        assert(false)
      catch
        case error: IllegalArgumentException =>
          assert(error.getMessage.contains("unexpected space"))
    }
  }
}
