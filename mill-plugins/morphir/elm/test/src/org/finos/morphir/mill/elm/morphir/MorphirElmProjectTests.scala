package org.finos.morphir.mill.elm.morphir

import java.nio.file.Files

import mill.*
import mill.api.ExecResult
import mill.testkit.{TestRootModule, UnitTester}
import org.finos.morphir.mill.*
import org.finos.morphir.mill.elm.ElmInputLimits
import scala.compiletime.testing.typeCheckErrors
import upickle.default.*
import utest.*

object MorphirElmProjectTests extends TestSuite {
  private final class TrackingBuild(workspace: os.Path)
      extends TestRootModule(workspace)
      with MorphirElmProjectInputsModule {
    lazy val millDiscover = mill.api.Discover[this.type]

    def observedSourceFingerprint: T[Vector[Int]] = Task {
      trackedMorphirProjectInputs().source.fingerprint
    }
  }

  private def success[A](result: Either[ExecResult.Failing[A], UnitTester.Result[A]]): UnitTester.Result[A] =
    result.fold(failure => throw new java.lang.AssertionError(failure.toString), identity)

  private def withTempDir[A](f: os.Path => A): A = {
    val directory = os.Path(Files.createTempDirectory("morphir-elm-project-test"))
    try f(directory)
    finally os.remove.all(directory)
  }

  private def writeProject(project: os.Path, sourceDirectory: String = "src"): MorphirProjectConfig = {
    val sourcePath = project / os.RelPath(sourceDirectory)
    val config     = MorphirProjectConfig(
      name = "Example.Project",
      sourceDirectory = sourceDirectory,
      exposedModules = List("Example"),
      dependencies = List("elm/core"),
      localDependencies = List("old/source-tree/path.json")
    )
    os.makeDir.all(sourcePath)
    os.write(sourcePath / "Example.elm", "module Example exposing (..)")
    os.write(project / "morphir.json", write(config, indent = 2))
    os.write(project / "elm.json", """{"type":"application","source-directories":["src"]}""")
    config
  }

  private def dependency(module: ModuleId, path: os.Path): MorphirDependencyArtifact =
    MorphirDependencyArtifact.fromArtifact(MorphirIrArtifact.fromFile(module, PathRef(path)))

  val tests = Tests {
    test("legacy metabuild module names retain the Elm adapter surface") {
      val errors = scala.compiletime.testing.typeCheckErrors(
        """
          def publishedAdapter(
              legacy: org.finos.millmorphir.MorphirModule
          ): org.finos.morphir.mill.elm.morphir.MorphirElmModule = legacy
          def makeSurface(legacy: org.finos.millmorphir.MorphirModule) = legacy.make
          def morphirIrSurface(legacy: org.finos.millmorphir.MorphirModule) = legacy.morphirIR
        """
      )
      assert(errors.isEmpty)
    }

    test("typed dependency IDs produce bounded sandbox paths") {
      val id = moduleId"morphir-elm.sdks.morphir-unit-test"
      assert(
        MorphirElmProjectSandbox.dependencyRelativePath(id) ==
          os.rel / ".morphir-deps" / "morphir-elm.sdks.morphir-unit-test" / "morphir-ir.json"
      )
      val errors = typeCheckErrors(
        """
          import org.finos.morphir.mill.elm.morphir.MorphirElmProjectSandbox
          MorphirElmProjectSandbox.dependencyRelativePath("../escape")
        """
      )
      assert(errors.nonEmpty)
    }

    test("configuration rewriting uses typed dependency artifacts") {
      withTempDir { root =>
        val first  = root / "first.json"
        val second = root / "second.json"
        os.write(first, "first")
        os.write(second, "second")
        val dependencies = Seq(
          dependency(moduleId"dep.one", first),
          dependency(moduleId"dep-two", second)
        )
        val config    = MorphirProjectConfig("Example.Project", "src", List("Example"))
        val rewritten = MorphirElmProjectSandbox
          .rewrittenConfig(config, dependencies)
          .fold(message => throw new IllegalStateException(message), identity)
        assert(
          rewritten.localDependencies == List(
            ".morphir-deps/dep.one/morphir-ir.json",
            ".morphir-deps/dep-two/morphir-ir.json"
          )
        )
        assert(MorphirElmProjectSandbox.rewrittenConfig(config, dependencies :+ dependencies.head).isLeft)
      }
    }

    test("project staging copies only validated inputs and typed dependencies") {
      Seq("src", "src/elm").foreach { sourceDirectory =>
        withTempDir { root =>
          val project = root / "project"
          writeProject(project, sourceDirectory)
          val ir = root / "dependency-ir.json"
          os.write(ir, "dependency")
          val sandbox = root / "task" / "project"
          val staged  = MorphirElmProjectSandbox
            .stage(
              sandbox,
              project / "morphir.json",
              Some(project / "elm.json"),
              project / "src",
              Seq(dependency(moduleId"dep.module", ir))
            )
            .fold(message => throw new IllegalStateException(message), identity)

          assert(staged.projectDir.path == sandbox)
          assert(staged.output == sandbox / "morphir-ir.json")
          assert(os.read(sandbox / os.RelPath(sourceDirectory) / "Example.elm") == "module Example exposing (..)")
          assert(os.read(sandbox / ".morphir-deps" / "dep.module" / "morphir-ir.json") == "dependency")
          val config = read[MorphirProjectConfig](os.read(sandbox / "morphir.json"))
          assert(config.sourceDirectory == sourceDirectory)
          assert(config.localDependencies == List(".morphir-deps/dep.module/morphir-ir.json"))
        }
      }
    }

    test("project staging preserves a configured non-src source root") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project, "elm-src")
        os.write.over(
          project / "elm.json",
          ujson.Obj("type" -> "application", "source-directories" -> ujson.Arr("elm-src")).render()
        )
        val sandbox = root / "task" / "project"
        val staged  = MorphirElmProjectSandbox
          .stage(
            sandbox,
            project / "morphir.json",
            Some(project / "elm.json"),
            project / "elm-src",
            Seq.empty
          )
          .fold(message => throw new IllegalStateException(message), identity)

        assert(staged.projectDir.path == sandbox)
        assert(os.read(sandbox / "elm-src" / "Example.elm") == "module Example exposing (..)")
        assert(!os.exists(sandbox / "src"))
        assert(read[MorphirProjectConfig](os.read(sandbox / "morphir.json")).sourceDirectory == "elm-src")
      }
    }

    test("dependency content identity is revalidated immediately before staging") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project)
        val ir = root / "dependency-ir.json"
        os.write(ir, "first")
        val tracked = dependency(moduleId"dep.module", ir)
        os.write.over(ir, "changed at the same path")

        val sandbox = root / "task" / "project"
        val result  = MorphirElmProjectSandbox.stage(
          sandbox,
          project / "morphir.json",
          Some(project / "elm.json"),
          project / "src",
          Seq(tracked)
        )
        assert(result.isLeft)
        assert(result.swap.toOption.get.contains("content identity"))
        assert(!os.exists(sandbox))
      }
    }

    test("project staging rejects symbolic links before creating the sandbox") {
      if (!scala.util.Properties.isWin) withTempDir { root =>
        val project = root / "project"
        writeProject(project)
        val sentinel = root / "sentinel.txt"
        os.write(sentinel, "outside")
        Files.createSymbolicLink((project / "src" / "external.elm").toNIO, sentinel.toNIO)
        val sandbox = root / "task" / "project"

        val result = MorphirElmProjectSandbox.stage(
          sandbox,
          project / "morphir.json",
          Some(project / "elm.json"),
          project / "src",
          Seq.empty
        )
        assert(result.isLeft)
        assert(!os.exists(sandbox))
        assert(os.read(sentinel) == "outside")
      }
    }

    test("project staging rejects Elm sources outside the Morphir source tree") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project)
        os.write.over(
          project / "elm.json",
          ujson.Obj("type" -> "application", "source-directories" -> ujson.Arr("../outside")).render()
        )
        val sandbox = root / "task" / "project"
        val result  = MorphirElmProjectSandbox.stage(
          sandbox,
          project / "morphir.json",
          Some(project / "elm.json"),
          project / "src",
          Seq.empty
        )
        assert(result.isLeft)
        assert(!os.exists(sandbox))
      }
    }

    test("project staging binds the supplied source to the snapshotted configuration") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project)
        val wrongSource = project / "other"
        os.write(wrongSource / "Other.elm", "module Other exposing (..)", createFolders = true)
        val sandbox = root / "task" / "project"
        val result  = MorphirElmProjectSandbox.stage(
          sandbox,
          project / "morphir.json",
          Some(project / "elm.json"),
          wrongSource,
          Seq.empty
        )
        assert(result.isLeft)
        assert(result.swap.toOption.get.contains("configured source root"))
        assert(!os.exists(sandbox))
      }
    }

    test("project staging rejects a configured source outside the tracked source root") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project)
        val trackedSource = project / "elm-src"
        os.write(trackedSource / "Example.elm", "module Example exposing (..)", createFolders = true)
        val sandbox = root / "task" / "project"
        val result  = MorphirElmProjectSandbox.stage(
          sandbox,
          project / "morphir.json",
          Some(project / "elm.json"),
          trackedSource,
          Seq.empty
        )

        assert(result.isLeft)
        assert(result.swap.toOption.get.contains("outside the tracked source root"))
        assert(!os.exists(sandbox))
      }
    }

    test("project staging rejects a tracked source root outside the configured project") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project, "elm-src")
        val outside = root / "elm-src"
        os.write(outside / "Example.elm", "module Example exposing (..)", createFolders = true)
        val sandbox = root / "task" / "project"
        val result  = MorphirElmProjectSandbox.stage(
          sandbox,
          project / "morphir.json",
          Some(project / "elm.json"),
          outside,
          Seq.empty
        )

        assert(result.isLeft)
        assert(result.swap.toOption.get.contains("outside the Morphir project root"))
        assert(!os.exists(sandbox))
      }
    }

    test("project input tracking rejects a source root outside the configured project") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project, "elm-src")
        val outside = root / "elm-src"
        os.write(outside / "Example.elm", "module Example exposing (..)", createFolders = true)

        val result = scala.util.Try {
          MorphirElmProjectInputs.capture(
            project / "morphir.json",
            Some(project / "elm.json"),
            outside,
            ElmInputLimits()
          )
        }
        assert(result.isFailure)
        val error = result.failed.get
        assert(error.isInstanceOf[IllegalArgumentException])
        assert(error.getMessage.contains("outside the Morphir project root"))
      }
    }

    test("project staging keeps custom source roots out of reserved sandbox paths") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project, ".morphir-deps")
        val sandbox = root / "task" / "project"
        val result  = MorphirElmProjectSandbox.stage(
          sandbox,
          project / "morphir.json",
          Some(project / "elm.json"),
          project / ".morphir-deps",
          Seq.empty
        )

        assert(result.isLeft)
        assert(result.swap.toOption.get.contains("reserved sandbox path"))
        assert(!os.exists(sandbox))
      }
    }

    test("project staging rejects a symlink in the tracked source root path") {
      if (!scala.util.Properties.isWin) withTempDir { root =>
        val project = root / "project"
        val outside = root / "outside"
        os.write(outside / "src" / "Example.elm", "module Example exposing (..)", createFolders = true)
        os.makeDir.all(project)
        Files.createSymbolicLink((project / "linked").toNIO, outside.toNIO)
        val config = MorphirProjectConfig(
          name = "Example.Project",
          sourceDirectory = "linked/src",
          exposedModules = List("Example")
        )
        os.write(project / "morphir.json", write(config, indent = 2))
        val sandbox = root / "task" / "project"
        val result  = MorphirElmProjectSandbox.stage(
          sandbox,
          project / "morphir.json",
          None,
          project / "linked" / "src",
          Seq.empty
        )

        assert(result.isLeft)
        assert(result.swap.toOption.get.contains("symbolic link"))
        assert(!os.exists(sandbox))
      }
    }

    test("project staging enforces bounded source inputs") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project)
        val sandbox = root / "task" / "project"
        val result  = MorphirElmProjectSandbox.stage(
          sandbox,
          project / "morphir.json",
          Some(project / "elm.json"),
          project / "src",
          Seq.empty,
          limits = ElmInputLimits(maxEntries = 1)
        )
        assert(result.isLeft)
        assert(result.swap.toOption.get.contains("entry count limit"))
        assert(!os.exists(sandbox))
      }
    }

    test("output filename remains a bounded sandbox leaf") {
      withTempDir { root =>
        val staged = StagedMorphirProject(PathRef(root), root / "morphir-ir.json")
        assert(
          MorphirElmProjectSandbox.withOutputFilename(staged, "custom-ir.json") ==
            Right(staged.copy(output = root / "custom-ir.json"))
        )
        Seq("", "../escape.json", "nested/output.json", "/absolute.json", "C:\\escape.json", "con.json")
          .foreach(filename => assert(MorphirElmProjectSandbox.withOutputFilename(staged, filename).isLeft))
      }
    }

    test("output filename cannot collide with a custom source root") {
      withTempDir { root =>
        val project = root / "project"
        writeProject(project, "custom-ir.json")
        val staged = MorphirElmProjectSandbox
          .stage(
            root / "task" / "project",
            project / "morphir.json",
            None,
            project / "custom-ir.json",
            Seq.empty
          )
          .fold(message => throw new IllegalStateException(message), identity)

        val result = MorphirElmProjectSandbox.withOutputFilename(staged, "custom-ir.json")
        assert(result.isLeft)
        assert(result.swap.toOption.get.contains("collides with a staged project input"))
      }
    }

    test("morphirIR is primary and make is a typed compatibility alias") {
      val errors = typeCheckErrors(
        """
          import mill.T
          import org.finos.morphir.mill.MorphirIrArtifact
          import org.finos.morphir.mill.elm.morphir.MorphirElmModule
          def primary(module: MorphirElmModule): T[MorphirIrArtifact] = module.morphirIR
          def compatibility(module: MorphirElmModule): T[MorphirIrArtifact] = module.make
        """
      )
      assert(errors.isEmpty)
    }

    test("Mill invalidates project inputs despite a legacy PathRef collision") {
      withTempDir { root =>
        val first    = "collision-00156726"
        val second   = "collision-00163008"
        val sources  = root / "sources"
        val config   = sources / "morphir.json"
        val changing = sources / "src" / "Collision.txt"
        os.write(config, """{"name":"Collision","sourceDirectory":"src"}""", createFolders = true)
        os.write(sources / "src" / "Main.elm", "module Main exposing (main)", createFolders = true)
        os.write(changing, first)
        val module = new TrackingBuild(root / "workspace")
        UnitTester(module, sources).scoped { evaluator =>
          val initial = success(evaluator(module.observedSourceFingerprint))
          assert(initial.evalCount > 0)
          assert(success(evaluator(module.observedSourceFingerprint)).evalCount == 0)

          val liveSource           = module.moduleDir / "src"
          val liveChanging         = liveSource / "Collision.txt"
          val firstLegacySignature = PathRef(liveSource).sig
          os.write.over(liveChanging, second)
          assert(firstLegacySignature == PathRef(liveSource).sig)
          val mutation = success(evaluator(module.observedSourceFingerprint))
          assert(mutation.evalCount > 0)
          assert(mutation.value != initial.value)
        }
      }
    }
  }
}
