package millbuild

import utest.*

object FormatSelectionTests extends TestSuite {
  val tests = Tests {
    test("FormatKind.parse") {
      assert(FormatKind.parse("all") == Right(FormatKind.All))
      assert(FormatKind.parse("scala") == Right(FormatKind.Scala))
      assert(FormatKind.parse("elm") == Right(FormatKind.Elm))
      assert(FormatKind.parse("ALL") == Right(FormatKind.All))
      assert(FormatKind.parse(" rust ").isLeft)
      assert(FormatKind.parse("rust").isLeft)
    }

    test("routeByExtension splits scala/mill/elm/ignored") {
      val routed = FormatSelection.routeByExtension(
        Seq(
          os.RelPath("a.scala"),
          os.RelPath("b.mill"),
          os.RelPath("c.elm"),
          os.RelPath("d.md"),
          os.RelPath("nested") / "e.scala"
        )
      )
      assert(routed.scalaPaths.map(_.toString) == Seq("a.scala", "b.mill", "nested/e.scala"))
      assert(routed.elmPaths.map(_.toString) == Seq("c.elm"))
      assert(routed.ignored.map(_.toString) == Seq("d.md"))
    }

    test("kind enables the right extension families") {
      assert(FormatSelection.scalaExtensions(FormatKind.All))
      assert(FormatSelection.elmExtensions(FormatKind.All))
      assert(FormatSelection.scalaExtensions(FormatKind.Scala))
      assert(!FormatSelection.elmExtensions(FormatKind.Scala))
      assert(FormatSelection.elmExtensions(FormatKind.Elm))
      assert(!FormatSelection.scalaExtensions(FormatKind.Elm))
    }

    test("filterChanged keeps paths matching the selected kind") {
      val paths = Seq(
        os.RelPath("a.scala"),
        os.RelPath("b.mill"),
        os.RelPath("c.elm"),
        os.RelPath("d.md")
      )
      assert(
        FormatSelection.filterChanged(paths, FormatKind.All).map(_.toString) ==
          Seq("a.scala", "b.mill", "c.elm")
      )
      assert(
        FormatSelection.filterChanged(paths, FormatKind.Scala).map(_.toString) ==
          Seq("a.scala", "b.mill")
      )
      assert(
        FormatSelection.filterChanged(paths, FormatKind.Elm).map(_.toString) ==
          Seq("c.elm")
      )
    }

    test("gitStatusPaths parses porcelain modified/staged/untracked and skips deletes") {
      val porcelain =
        """| M staged-mod.scala
           |M  index-mod.mill
           |MM both.scala
           |A  added.elm
           |?? untracked.scala
           | D deleted.scala
           |D  deleted-index.elm
           |R  old.scala -> renamed.scala
           |!! ignored.scala
           |""".stripMargin

      val paths = FormatSelection.gitStatusPaths(porcelain).map(_.toString).toSet
      assert(paths.contains("staged-mod.scala"))
      assert(paths.contains("index-mod.mill"))
      assert(paths.contains("both.scala"))
      assert(paths.contains("added.elm"))
      assert(paths.contains("untracked.scala"))
      assert(paths.contains("renamed.scala"))
      assert(!paths.contains("deleted.scala"))
      assert(!paths.contains("deleted-index.elm"))
      assert(!paths.contains("old.scala"))
      assert(!paths.contains("ignored.scala"))
    }

    test("build mill discovery includes build.mill, ci/, mill-plugins/, mill-build/") {
      val workspace = os.temp.dir(prefix = "format-selection-mills-")
      try
        os.write(workspace / "build.mill", "// root\n", createFolders = true)
        os.write(workspace / "ci" / "MorphirCi.mill", "// ci\n", createFolders = true)
        os.write(
          workspace / "mill-plugins" / "morphir" / "package.mill",
          "// plugins\n",
          createFolders = true
        )
        os.write(workspace / "mill-build" / "build.mill", "// metabuild\n", createFolders = true)
        os.write(workspace / "other" / "skip.mill", "// skip\n", createFolders = true)
        os.write(workspace / "README.md", "docs\n", createFolders = true)

        val mills = FormatSelection.discoverBuildMillFiles(workspace).map(_.toString).toSet
        assert(mills.contains("build.mill"))
        assert(mills.exists(_.startsWith("ci/")))
        assert(mills.exists(_.startsWith("mill-plugins/")))
        assert(mills.exists(_.startsWith("mill-build/")))
        assert(!mills.contains("other/skip.mill"))
      finally os.remove.all(workspace)
    }
  }
}
