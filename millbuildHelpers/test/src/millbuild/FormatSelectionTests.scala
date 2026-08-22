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
        os.RelPath("d.md"),
        os.RelPath("examples") / "src"
      )
      assert(
        FormatSelection.filterChanged(paths, FormatKind.All).map(_.toString) ==
          Seq("a.scala", "b.mill", "c.elm", "examples/src")
      )
      assert(
        FormatSelection.filterChanged(paths, FormatKind.Scala).map(_.toString) ==
          Seq("a.scala", "b.mill", "examples/src")
      )
      assert(
        FormatSelection.filterChanged(paths, FormatKind.Elm).map(_.toString) ==
          Seq("c.elm", "examples/src")
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

    test("build mill discovery walks the workspace and skips generated roots") {
      val workspace = os.temp.dir(prefix = "format-selection-mills-")
      try
        os.write(workspace / "build.mill", "// root\n", createFolders = true)
        os.write(workspace / "testing.mill", "// testing\n", createFolders = true)
        os.write(workspace / "ci" / "MorphirCi.mill", "// ci\n", createFolders = true)
        os.write(
          workspace / "mill-plugins" / "morphir" / "package.mill",
          "// plugins\n",
          createFolders = true
        )
        os.write(workspace / "mill-build" / "build.mill", "// metabuild\n", createFolders = true)
        os.write(workspace / "format" / "MorphirFormat.mill", "// format\n", createFolders = true)
        os.write(
          workspace / "millbuildHelpers" / "package.mill",
          "// helpers\n",
          createFolders = true
        )
        os.write(
          workspace / "morphir" / "desktop" / "dist" / "package.mill",
          "// desktop\n",
          createFolders = true
        )
        os.write(workspace / "out" / "generated.mill", "// out\n", createFolders = true)
        os.write(workspace / ".git" / "hooks.mill", "// git\n", createFolders = true)
        os.write(workspace / "README.md", "docs\n", createFolders = true)

        val mills = FormatSelection.discoverBuildMillFiles(workspace).map(_.toString).toSet
        assert(mills.contains("build.mill"))
        assert(mills.contains("testing.mill"))
        assert(mills.contains("format/MorphirFormat.mill"))
        assert(mills.contains("millbuildHelpers/package.mill"))
        assert(mills.contains("morphir/desktop/dist/package.mill"))
        assert(mills.exists(_.startsWith("ci/")))
        assert(mills.exists(_.startsWith("mill-plugins/")))
        assert(mills.exists(_.startsWith("mill-build/")))
        assert(!mills.contains("out/generated.mill"))
        assert(!mills.contains(".git/hooks.mill"))
      finally os.remove.all(workspace)
    }
  }
}
