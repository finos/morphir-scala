//| moduleDeps: ["//mill-build/src/org/finos/millmorphir/elm/MorphirElmProjectSandbox.scala", "//mill-build/src/org/finos/millmorphir/api/MorphirProjectConfig.scala", "//mill-plugins/morphir/elm-tooling/src/org/finos/morphir/mill/elm/ElmProcessEnvironment.scala"]
//| mvnDeps: ["com.lihaoyi::mill-libs:$MILL_VERSION"]

package org.finos.millmorphir.elm

import java.nio.file.Files

import mill.PathRef
import org.finos.millmorphir.api.MorphirProjectConfig
import upickle.default.*

def assertEquals[A](actual: A, expected: A): Unit =
  assert(actual == expected, s"Expected $expected, got $actual")

def withTempDir[A](f: os.Path => A): A = {
  val directory = os.Path(Files.createTempDirectory("morphir-elm-sandbox-test"))
  try f(directory)
  finally os.remove.all(directory)
}

def writeProject(project: os.Path, sourceDirectory: String = "src"): MorphirProjectConfig = {
  val sourcePath = project / os.RelPath(sourceDirectory)
  val config = MorphirProjectConfig(
    name = "Example.Project",
    sourceDirectory = sourceDirectory,
    exposedModules = List("Example"),
    dependencies = List("elm/core"),
    localDependencies = List("old/source-tree/path.json")
  )
  os.makeDir.all(sourcePath)
  os.write(sourcePath / "Example.elm", "module Example exposing (..)")
  os.write(project / "morphir.json", write(config, indent = 2))
  os.write(project / "elm.json", "{\"type\":\"application\"}")
  config
}

@main def runMorphirElmProjectSandboxTests(): Unit = {
  val safeIds = Seq("morphir-elm.sdks.morphir-unit-test", "example-project", "a_b.c-1")
  safeIds.foreach { moduleId =>
    assertEquals(
      MorphirElmProjectSandbox.dependencyRelativePath(moduleId),
      Right(os.rel / ".morphir-deps" / moduleId / "morphir-ir.json")
    )
  }

  Seq("", ".", "..", "../escape", "a/b", "a\\b", "/absolute", "C:/escape", "C:\\escape").foreach {
    moduleId =>
      assert(
        MorphirElmProjectSandbox.dependencyRelativePath(moduleId).isLeft,
        s"Expected unsafe dependency module ID to be rejected: $moduleId"
      )
  }

  val baseConfig = MorphirProjectConfig("Example.Project", "src", List("Example"))
  assertEquals(baseConfig.dependencies, Nil)
  assertEquals(baseConfig.localDependencies, Nil)
  assertEquals(
    baseConfig.withLocalDependencies(List("one", "two")).localDependencies,
    List("one", "two")
  )

  withTempDir { temp =>
    val first  = temp / "first.json"
    val second = temp / "second.json"
    os.write(first, "first")
    os.write(second, "second")
    val dependencies = Seq(
      MorphirDependencyArtifact("dep.one", PathRef(first)),
      MorphirDependencyArtifact("dep-two", PathRef(second))
    )
    val rewritten = MorphirElmProjectSandbox
      .rewrittenConfig(baseConfig, dependencies)
      .fold(message => throw new AssertionError(message), identity)
    assertEquals(
      rewritten.localDependencies,
      List(
        ".morphir-deps/dep.one/morphir-ir.json",
        ".morphir-deps/dep-two/morphir-ir.json"
      )
    )

    val duplicate = dependencies :+ MorphirDependencyArtifact("dep.one", PathRef(second))
    assert(MorphirElmProjectSandbox.rewrittenConfig(baseConfig, duplicate).isLeft)
    assert(
      MorphirElmProjectSandbox
        .rewrittenConfig(baseConfig, Seq(MorphirDependencyArtifact("../escape", PathRef(first))))
        .isLeft
    )
  }

  Seq("src", "src/elm").foreach { sourceDirectory =>
    withTempDir { temp =>
      val project = temp / "project"
      writeProject(project, sourceDirectory)
      val dependency = temp / "dependency-ir.json"
      os.write(dependency, "dependency")
      val sandbox = temp / "task-dest" / "project"

      val staged = MorphirElmProjectSandbox
        .stage(
          sandbox,
          project / "morphir.json",
          Some(project / "elm.json"),
          project / os.RelPath(sourceDirectory),
          Seq(MorphirDependencyArtifact("dep.module", PathRef(dependency)))
        )
        .fold(message => throw new AssertionError(message), identity)

      assertEquals(staged.projectDir.path, sandbox)
      assertEquals(staged.output, sandbox / "morphir-ir.json")
      assertEquals(os.read(sandbox / os.RelPath(sourceDirectory) / "Example.elm"), "module Example exposing (..)")
      assertEquals(os.read(sandbox / "elm.json"), "{\"type\":\"application\"}")
      assertEquals(os.read(sandbox / ".morphir-deps" / "dep.module" / "morphir-ir.json"), "dependency")

      val stagedConfig = read[MorphirProjectConfig](os.read(sandbox / "morphir.json"))
      assertEquals(stagedConfig.sourceDirectory, sourceDirectory)
      assertEquals(stagedConfig.localDependencies, List(".morphir-deps/dep.module/morphir-ir.json"))
    }
  }

  withTempDir { temp =>
    val project = temp / "project"
    writeProject(project)
    val dependency = temp / "dependency-ir.json"
    os.write(dependency, "dependency")
    val sandbox = temp / "task-dest" / "project"
    val outside = temp / "escape" / "morphir-ir.json"

    val result = MorphirElmProjectSandbox.stage(
      sandbox,
      project / "morphir.json",
      Some(project / "elm.json"),
      project / "src",
      Seq(MorphirDependencyArtifact("../escape", PathRef(dependency)))
    )
    assert(result.isLeft)
    assert(!os.exists(outside), "Sandbox staging must never write outside its root")
    assert(!os.exists(sandbox), "Invalid dependency IDs must be rejected before staging begins")
  }

  withTempDir { temp =>
    val project = temp / "project"
    writeProject(project)
    val unsafeConfig = MorphirProjectConfig("Unsafe.Project", "../escape", List("Unsafe"))
    os.write.over(project / "morphir.json", write(unsafeConfig))
    val sandbox = temp / "task-dest" / "project"

    val result = MorphirElmProjectSandbox.stage(
      sandbox,
      project / "morphir.json",
      Some(project / "elm.json"),
      project / "src",
      Seq.empty
    )
    assert(result.isLeft)
    assert(!os.exists(sandbox), "Unsafe source directories must be rejected before staging begins")
  }

  withTempDir { temp =>
    val project = temp / "project"
    writeProject(project)
    val sentinel = temp / "external-sentinel.txt"
    os.write(sentinel, "must-not-be-staged")
    Files.createSymbolicLink((project / "src" / "external-link.txt").toNIO, sentinel.toNIO)
    val sandbox = temp / "task-dest" / "project"

    val result = MorphirElmProjectSandbox.stage(
      sandbox,
      project / "morphir.json",
      Some(project / "elm.json"),
      project / "src",
      Seq.empty
    )
    assert(result.isLeft)
    assert(!os.exists(sandbox), "A source-tree symlink must be rejected before staging begins")
    assertEquals(os.read(sentinel), "must-not-be-staged")
  }

  withTempDir { temp =>
    val project = temp / "project"
    writeProject(project)
    val dependency = temp / "dependency.json"
    os.write(dependency, "dependency")
    val dependencyLink = temp / "dependency-link.json"
    Files.createSymbolicLink(dependencyLink.toNIO, dependency.toNIO)
    val configLink = temp / "morphir-link.json"
    Files.createSymbolicLink(configLink.toNIO, (project / "morphir.json").toNIO)
    val elmLink = temp / "elm-link.json"
    Files.createSymbolicLink(elmLink.toNIO, (project / "elm.json").toNIO)

    assert(
      MorphirElmProjectSandbox
        .stage(temp / "config-sandbox", configLink, Some(project / "elm.json"), project / "src", Seq.empty)
        .isLeft
    )
    assert(
      MorphirElmProjectSandbox
        .stage(temp / "elm-sandbox", project / "morphir.json", Some(elmLink), project / "src", Seq.empty)
        .isLeft
    )
    assert(
      MorphirElmProjectSandbox
        .stage(
          temp / "dependency-sandbox",
          project / "morphir.json",
          Some(project / "elm.json"),
          project / "src",
          Seq(MorphirDependencyArtifact("dependency", PathRef(dependencyLink)))
        )
        .isLeft
    )
  }

  withTempDir { temp =>
    val project = temp / "project"
    writeProject(project)
    val sentinelDirectory = temp / "external-sources"
    os.makeDir.all(sentinelDirectory)
    os.write(sentinelDirectory / "Sentinel.elm", "sentinel")

    Seq(
      ujson.Obj("type" -> "application", "source-directories" -> ujson.Arr("../external-sources")),
      ujson.Obj("type" -> "application", "source-directories" -> ujson.Arr(sentinelDirectory.toString)),
      ujson.Obj("type" -> "application", "source-directories" -> ujson.Arr("C:\\external-sources")),
      ujson.Obj("type" -> "application", "source-directories" -> ujson.Arr("C:/external-sources")),
      ujson.Obj("type" -> "application", "source-directories" -> ujson.Arr("\\\\server\\share")),
      ujson.Obj("type" -> "application", "source-directories" -> ujson.Arr("src/C:\\external-sources"))
    ).zipWithIndex.foreach { case (elmConfig, index) =>
      os.write.over(project / "elm.json", elmConfig.render())
      val sandbox = temp / s"elm-source-sandbox-$index"
      assert(
        MorphirElmProjectSandbox
          .stage(sandbox, project / "morphir.json", Some(project / "elm.json"), project / "src", Seq.empty)
          .isLeft
      )
      assert(!os.exists(sandbox))
      assertEquals(os.read(sentinelDirectory / "Sentinel.elm"), "sentinel")
    }
  }

  withTempDir { temp =>
    val first  = temp / "first.json"
    val second = temp / "second.json"
    os.write(first, "first")
    os.write(second, "second")
    assert(
      MorphirElmProjectSandbox
        .rewrittenConfig(
          baseConfig,
          Seq(
            MorphirDependencyArtifact("Dependency.One", PathRef(first)),
            MorphirDependencyArtifact("dependency.one", PathRef(second))
          )
        )
        .isLeft,
      "Dependency paths must not collide on case-insensitive filesystems"
    )
    Seq("CON", "con.txt", "PRN", "AUX", "NUL", "COM1", "com9.json", "LPT1", "lpt9.txt").foreach {
      moduleId =>
        assert(
          MorphirElmProjectSandbox.dependencyRelativePath(moduleId).isLeft,
          s"Expected Windows reserved dependency module ID to be rejected: $moduleId"
        )
    }
  }

  withTempDir { temp =>
    val poisoned = temp / "poisoned-ambient-home"
    val environment = MorphirElmProcessEnvironment.create(
      temp / "make-task",
      Map(
        "HOME" -> poisoned.toString,
        "USERPROFILE" -> poisoned.toString,
        "ELM_HOME" -> (poisoned / "elm").toString,
        "XDG_CACHE_HOME" -> (poisoned / "cache").toString,
        "HTTPS_PROXY" -> "https://proxy.example.test",
        "UNRELATED_SECRET" -> "must-not-propagate"
      )
    )

    assertEquals(environment("HOME"), (temp / "make-task" / "home").toString)
    assertEquals(environment("USERPROFILE"), (temp / "make-task" / "home").toString)
    assertEquals(environment("ELM_HOME"), (temp / "make-task" / "elm-home").toString)
    assertEquals(environment("XDG_CACHE_HOME"), (temp / "make-task" / "cache" / "xdg").toString)
    assertEquals(environment("HTTPS_PROXY"), "https://proxy.example.test")
    assert(!environment.contains("UNRELATED_SECRET"))
    assert(!environment.values.exists(_.contains(poisoned.toString)))

    if (!scala.util.Properties.isWin) {
      val observed = os.proc("/usr/bin/env").call(env = environment, propagateEnv = false).out.lines().toSet
      assert(observed.contains(s"HOME=${temp / "make-task" / "home"}"))
      assert(observed.contains(s"ELM_HOME=${temp / "make-task" / "elm-home"}"))
      assert(!observed.exists(_.startsWith("UNRELATED_SECRET=")))
      assert(!observed.exists(_.contains(poisoned.toString)))
    }
  }

  withTempDir { temp =>
    val staged = StagedMorphirProject(PathRef(temp), temp / "morphir-ir.json")
    assertEquals(
      MorphirElmProjectSandbox.withOutputFilename(staged, "custom-ir.json"),
      Right(staged.copy(output = temp / "custom-ir.json"))
    )
    Seq("", "../escape.json", "nested/output.json", "/absolute.json", "C:\\escape.json").foreach { filename =>
      assert(MorphirElmProjectSandbox.withOutputFilename(staged, filename).isLeft)
    }
  }

  withTempDir { temp =>
    def fixture(name: String): PathRef = {
      val path = temp / name
      os.write(path, name)
      PathRef(path)
    }

    val fixtures = ClassicRuntimeFixtureSet(
      evaluator = fixture("evaluator.json"),
      defaults = fixture("defaults.json"),
      unitTestFramework = fixture("unit-test-framework.json"),
      unitTestExample = fixture("unit-test-example.json"),
      unitTestFailing = fixture("unit-test-failing.json"),
      unitTestPassing = fixture("unit-test-passing.json"),
      unitTestIncomplete = fixture("unit-test-incomplete.json")
    )
    val rendered = ClassicRuntimeFixtureSource.render(fixtures)

    assertEquals(rendered, ClassicRuntimeFixtureSource.render(fixtures))
    assert(rendered.contains("package org.finos.morphir.runtime.fixtures"))
    assert(rendered.contains("object GeneratedRuntimeFixtures"))
    Seq(
      "evaluator",
      "defaults",
      "unitTestFramework",
      "unitTestExample",
      "unitTestFailing",
      "unitTestPassing",
      "unitTestIncomplete"
    ).foreach { field =>
      assert(rendered.contains(s"val $field: java.nio.file.Path"), s"Missing generated Path field $field")
    }

    def sha256(path: os.Path): String = {
      val digest = java.security.MessageDigest.getInstance("SHA-256")
      digest.digest(os.read.bytes(path)).map(byte => f"${byte & 0xff}%02x").mkString
    }

    val identities = Seq(
      fixtures.evaluator,
      fixtures.defaults,
      fixtures.unitTestFramework,
      fixtures.unitTestExample,
      fixtures.unitTestFailing,
      fixtures.unitTestPassing,
      fixtures.unitTestIncomplete
    ).map(path => sha256(path.path))
    identities.foreach { identity =>
      assert(rendered.contains(identity), s"Missing fixture content identity $identity")
    }

    os.write.over(fixtures.evaluator.path, "changed evaluator content")
    val changed = ClassicRuntimeFixtureSource.render(fixtures)
    assert(changed != rendered, "Changing fixture bytes at the same path must change the generated source")
    assertEquals(changed, ClassicRuntimeFixtureSource.render(fixtures))
  }

  val windowsPath = "C:\\fixtures\\morphir-ir.json"
  val escaped      = ClassicRuntimeFixtureSource.escapeScalaString(windowsPath)
  assertEquals(escaped, "C:\\\\fixtures\\\\morphir-ir.json")
}
