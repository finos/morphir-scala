//| moduleDeps: ["//mill-build/src/org/finos/millmorphir/elm/MorphirElmProjectSandbox.scala", "//mill-build/src/org/finos/millmorphir/api/MorphirProjectConfig.scala"]
//| mvnDeps: ["com.lihaoyi::mill-libs:$MILL_VERSION"]

import java.nio.file.Files

import mill.PathRef
import org.finos.millmorphir.api.MorphirProjectConfig
import org.finos.millmorphir.elm.*
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
}
