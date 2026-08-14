package org.finos.morphir.mill.publish

import utest.*

object PublishSelectorsTests extends TestSuite {
  private val all = Seq(
    "morphir.jvm",
    "morphir.naming.jvm",
    "morphir.mill.plugin.core",
    "morphir.foo.integration.bar",
    "mill-plugins.morphir.core.x",
    "other.thing"
  )

  val tests = Tests {
    test("dropExcluded removes integration modules") {
      val kept = PublishSelectors.dropExcluded(all, Seq(".integration."))
      assert(!kept.contains("morphir.foo.integration.bar"))
      assert(kept.contains("morphir.jvm"))
    }

    test("libraryModules keeps prefix matches minus exclusions and plugin roots") {
      val libraries = PublishSelectors.libraryModules(
        allPublishModules = PublishSelectors.dropExcluded(all, Seq(".integration.")),
        libraryModulePrefix = "morphir.",
        libraryExcludedPrefixes = Seq("morphir.naming."),
        pluginRoots = Seq("morphir.mill.plugin")
      )
      assert(libraries == Seq("morphir.jvm"))
    }

    test("libraryModules rejects an empty prefix") {
      try {
        PublishSelectors.libraryModules(all, "", Seq.empty, Seq.empty)
        assert(false)
      } catch {
        case _: IllegalArgumentException => ()
      }
    }

    test("isUnderPluginRoot matches the root and nested modules") {
      val roots = Seq("mill-plugins.morphir.core")
      assert(PublishSelectors.isUnderPluginRoot("mill-plugins.morphir.core", roots))
      assert(PublishSelectors.isUnderPluginRoot("mill-plugins.morphir.core.1.2.0", roots))
      assert(!PublishSelectors.isUnderPluginRoot("mill-plugins.morphir.elm", roots))
    }

    test("allKinds unions without duplicates") {
      assert(
        PublishSelectors.allKinds(Seq("morphir.jvm", "plug.a"), Seq("plug.a", "plug.b")) ==
          Seq("morphir.jvm", "plug.a", "plug.b")
      )
    }
  }
}
