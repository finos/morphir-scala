package org.finos.morphir.mill.publish.version

import utest.*

object TagStreamTests extends TestSuite {
  private val libraries = TagStream(None)
  private val plugins   = TagStream(Some("mill-plugins"))
  private val desktop   = TagStream(Some("desktop"))

  val tests = Tests {
    test("an absent namespace is the bare v stream") {
      assert(libraries.pattern == "v*")
      assert(libraries.tagFor("0.6.0-M01") == "v0.6.0-M01")
    }

    test("a namespace prefixes both the pattern and the tag") {
      assert(plugins.pattern == "mill-plugins/v*")
      assert(plugins.tagFor("0.1.0") == "mill-plugins/v0.1.0")
      assert(desktop.pattern == "desktop/v*")
      assert(desktop.tagFor("0.3.0") == "desktop/v0.3.0")
    }

    test("versionFromTag round-trips its own tags") {
      assert(libraries.versionFromTag("v0.6.0-M01") == Some("0.6.0-M01"))
      assert(desktop.versionFromTag("desktop/v0.3.0") == Some("0.3.0"))
    }

    test("a stream does not claim another stream's tag") {
      // The bare v stream must not swallow desktop/v0.3.0 — this is the property that
      // keeps a desktop release from becoming the libraries' nearest tag.
      assert(libraries.versionFromTag("desktop/v0.3.0") == None)
      assert(desktop.versionFromTag("v0.6.0-M01") == None)
      assert(desktop.versionFromTag("mill-plugins/v0.1.0") == None)
    }
  }
}
