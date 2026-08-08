package org.finos.morphir.mill.elm

import java.nio.file.Files

import mill.*
import mill.api.ExecResult
import mill.testkit.{TestRootModule, UnitTester}
import org.finos.morphir.mill.javascript.*
import org.finos.morphir.mill.javascript.node.NodeRuntimeModule
import org.finos.morphir.mill.javascript.npm.NpmPackageManagerModule
import org.finos.morphir.mill.toolchain.storageSize
import org.finos.morphir.mill.toolchain.StorageSize
import scala.compiletime.testing.typeCheckErrors
import utest.*

object ElmToolingTests extends TestSuite {
  private def withTempDir[A](body: os.Path => A): A = {
    val root = os.Path(Files.createTempDirectory("elm-tooling-test"))
    try body(root)
    finally os.remove.all(root)
  }

  private def successResult[A](result: Either[ExecResult.Failing[A], UnitTester.Result[A]]): UnitTester.Result[A] =
    result.fold(failure => throw new java.lang.AssertionError(failure.toString), identity)

  private def success[A](result: Either[ExecResult.Failing[A], UnitTester.Result[A]]): A = successResult(result).value

  private final class CommandBuild(workspace: os.Path) extends TestRootModule(workspace) { outer =>
    lazy val millDiscover = mill.api.Discover[this.type]

    object packages extends JavaScriptPackageManagerModule {
      def runtime: JavaScriptRuntimeModule = throw new UnsupportedOperationException
      def projectFiles: T[Seq[PathRef]]    = Task(Seq.empty)
      def lockFiles: T[Seq[PathRef]]       = Task(Seq.empty)
      def install: T[JavaScriptInstall]    = Task(throw new UnsupportedOperationException)
      def packageManagerCommand(arguments: Seq[String]): Task[JavaScriptCommand] =
        Task.Anon(throw new UnsupportedOperationException)
      def packageBinaryCommand(binary: PackageBinary, arguments: Seq[String]): Task[JavaScriptCommand] =
        Task.Anon(JavaScriptCommand(PathRef(outer.moduleDir / "provisioned" / binary.value), arguments))
    }

    object elm extends ElmToolModule {
      def packageManager = outer.packages
    }
  }

  private final class RealElmBuild(workspace: os.Path) extends TestRootModule(workspace) { outer =>
    lazy val millDiscover = mill.api.Discover[this.type]

    object runtime extends NodeRuntimeModule

    object packages extends NpmPackageManagerModule {
      def runtime                  = outer.runtime
      override def npmProjectPaths = Seq(outer.moduleDir / "tools" / "package.json")
      override def npmLockPaths    = Seq(outer.moduleDir / "tools" / "package-lock.json")
    }

    object tools extends ElmToolModule {
      def packageManager = outer.packages
    }

    object app extends ElmModule {
      def elmTool = outer.tools
    }
  }

  private final class TrackingBuild(workspace: os.Path) extends TestRootModule(workspace) { outer =>
    lazy val millDiscover = mill.api.Discover[this.type]

    object runtime extends JavaScriptRuntimeModule {
      def runtimeVersion: T[String]     = Task("test")
      def runtimeHome: T[PathRef]       = Task(PathRef(os.Path(System.getProperty("java.home"), os.pwd)))
      def runtimeExecutable: T[PathRef] = Task {
        PathRef(os.Path(System.getProperty("java.home"), os.pwd) / "bin" / "java")
      }
      def runtimeCommand(arguments: Seq[String]): Task[JavaScriptCommand] = Task.Anon {
        JavaScriptCommand(runtimeExecutable(), arguments)
      }
    }

    object packages extends JavaScriptPackageManagerModule {
      def runtime: JavaScriptRuntimeModule = outer.runtime
      def projectFiles: T[Seq[PathRef]]    = Task(Seq.empty)
      def lockFiles: T[Seq[PathRef]]       = Task(Seq.empty)
      def install: T[JavaScriptInstall]    = Task(throw new UnsupportedOperationException)
      def packageManagerCommand(arguments: Seq[String]): Task[JavaScriptCommand] =
        Task.Anon(throw new UnsupportedOperationException)
      def packageBinaryCommand(binary: PackageBinary, arguments: Seq[String]): Task[JavaScriptCommand] = Task.Anon {
        val executable = os.Path(System.getProperty("java.home"), os.pwd) / "bin" / "java"
        JavaScriptCommand(
          PathRef(executable),
          (outer.moduleDir / "FakeElm.java").toString +: arguments
        )
      }
    }

    object tools extends ElmToolModule {
      def packageManager = outer.packages
    }

    object app extends ElmModule {
      def elmTool = outer.tools
    }
  }

  val tests = Tests {
    test("Elm tooling exposes neutral package-manager APIs and an authoritative compile task") {
      val errors = typeCheckErrors(
        """
          import mill.*
          import org.finos.morphir.mill.elm.*
          import org.finos.morphir.mill.javascript.*
          trait Consumer extends ElmModule {
            def elmTool: ElmToolModule
            override def elmJsonPath: os.Path = moduleDir / "custom-elm.json"
            override def elmSourcePaths: Seq[os.Path] = Seq(moduleDir / "custom-src")
            val publicElmJson: T[PathRef] = elmJson
            val publicElmSources: T[Seq[PathRef]] = elmSources
            override def compile: T[PathRef] = super.compile
          }
          trait Tool extends ElmToolModule {
            def packageManager: JavaScriptPackageManagerModule
          }
        """
      )
      assert(errors.isEmpty)
      val maxFileBytes: StorageSize = ElmInputLimits().maxFileBytes
      assert(maxFileBytes == storageSize"64 MiB")
    }

    test("Elm command uses the typed package binary without runtime assumptions") {
      withTempDir { root =>
        val module  = new CommandBuild(root / "workspace")
        val sources = root / "sources"
        os.makeDir.all(sources)
        UnitTester(module, sources).scoped { evaluator =>
          val command = success(evaluator(module.elm.elmCommand(Seq("make", "src/Main.elm"))))
          assert(command.executable.path.last == "elm")
          assert(command.arguments == Seq("make", "src/Main.elm"))
          assert(!command.arguments.exists(Set("node", "npm", "npx", "bun", "mise")))
        }
      }
    }

    test("Elm process environment hides ambient executables and state") {
      withTempDir { root =>
        val poisoned    = root / "ambient"
        val environment = ElmProcessEnvironment.create(
          root / "task",
          Map(
            "PATH"             -> "/ambient/bin",
            "HOME"             -> poisoned.toString,
            "ELM_HOME"         -> (poisoned / "elm").toString,
            "HTTPS_PROXY"      -> "https://proxy.example.test",
            "UNRELATED_SECRET" -> "hidden"
          )
        )
        assert(!environment.contains("PATH"))
        assert(!environment.contains("UNRELATED_SECRET"))
        assert(environment("HOME").startsWith((root / "task").toString))
        assert(environment("ELM_HOME").startsWith((root / "task").toString))
        assert(environment("HTTPS_PROXY") == "https://proxy.example.test")
        assert(!environment.values.exists(_.contains(poisoned.toString)))
      }
    }

    test("Elm project snapshots reject symlinks duplicate destinations and escaping entrypoints") {
      withTempDir { root =>
        val elmJson = root / "inputs" / "elm.json"
        val source  = root / "inputs" / "src"
        os.write(elmJson, "{}", createFolders = true)
        os.write(source / "Main.elm", "module Main exposing (main)", createFolders = true)

        def rejected(
            name: String,
            json: os.Path = elmJson,
            sources: Seq[os.Path] = Seq(source),
            entrypoint: os.RelPath = os.rel / "src" / "Main.elm"
        )(expected: String): Unit = {
          val error = scala.util.Try {
            ElmProjectSnapshot.stage(
              root / "task" / name,
              PathRef(json),
              sources.map(PathRef(_)),
              entrypoint,
              ElmInputLimits(maxEntries = 10, maxFileBytes = storageSize"1 KiB", maxTotalBytes = storageSize"4 KiB")
            )
          }.failed.get
          assert(error.getMessage.contains(expected))
          assert(!os.exists(root / "task" / name / "project"))
        }

        val outside  = root / "outside"
        val rootLink = root / "source-link"
        os.makeDir.all(outside)
        os.write(outside / "Secret.elm", "secret")
        Files.createSymbolicLink(rootLink.toNIO, outside.toNIO)
        rejected("root-symlink", sources = Seq(rootLink))("symbolic link")

        val nested = source / "nested-link"
        Files.createSymbolicLink(nested.toNIO, outside.toNIO)
        rejected("nested-symlink")("symbolic link")
        Files.delete(nested.toNIO)

        val otherSource = root / "other" / "src"
        os.write(otherSource / "Other.elm", "module Other exposing (..)", createFolders = true)
        rejected("duplicate", sources = Seq(source, otherSource))("duplicate staged destination")
        rejected("upward", entrypoint = os.rel / os.up / "Outside.elm")("entrypoint")
        rejected("outside-source", entrypoint = os.rel / "other" / "Main.elm")("entrypoint")
      }
    }

    test("Elm project snapshots enforce limits and detect same-path input races") {
      withTempDir { root =>
        val elmJson = root / "inputs" / "elm.json"
        val source  = root / "inputs" / "src"
        val main    = source / "Main.elm"
        os.write(elmJson, "{}", createFolders = true)
        os.write(main, "module Main exposing (main)", createFolders = true)

        def failure(limits: ElmInputLimits, expected: String): Unit = {
          val error = scala.util.Try {
            ElmProjectSnapshot.stage(
              root / s"task-${expected.hashCode}",
              PathRef(elmJson),
              Seq(PathRef(source)),
              os.rel / "src" / "Main.elm",
              limits
            )
          }.failed.get
          assert(error.getMessage.contains(expected))
        }

        failure(
          ElmInputLimits(maxEntries = 1, maxFileBytes = storageSize"1 KiB", maxTotalBytes = storageSize"4 KiB"),
          "entry count"
        )
        failure(
          ElmInputLimits(maxEntries = 10, maxFileBytes = storageSize"4 B", maxTotalBytes = storageSize"4 KiB"),
          "file bytes"
        )
        failure(
          ElmInputLimits(maxEntries = 10, maxFileBytes = storageSize"1 KiB", maxTotalBytes = storageSize"4 B"),
          "total bytes"
        )

        val race = scala.util.Try {
          ElmProjectSnapshot.stage(
            root / "race-task",
            PathRef(elmJson),
            Seq(PathRef(source)),
            os.rel / "src" / "Main.elm",
            ElmInputLimits(),
            beforeRevalidate = () => os.write.over(main, "changed after snapshot")
          )
        }.failed.get
        assert(race.getMessage.contains("changed after snapshot"))
        assert(!os.exists(root / "race-task" / "project"))
      }
    }

    test("Elm compile invalidates on a full fingerprint despite a legacy PathRef collision") {
      withTempDir { root =>
        val first    = "collision-00156726"
        val second   = "collision-00163008"
        val sources  = root / "sources"
        val app      = sources / "app"
        val changing = app / "src" / "Collision.txt"
        os.write(
          sources / "FakeElm.java",
          """import java.nio.file.*;
            |final class FakeElm {
            |  public static void main(String[] arguments) throws Exception {
            |    int outputIndex = java.util.Arrays.asList(arguments).indexOf("--output");
            |    Path workingDirectory = Paths.get(System.getProperty("user.dir"));
            |    String content = Files.readString(workingDirectory.resolve("src").resolve("Collision.txt"));
            |    Files.writeString(workingDirectory.resolve(arguments[outputIndex + 1]), content);
            |  }
            |}
            |""".stripMargin,
          createFolders = true
        )
        os.write(app / "elm.json", "{}", createFolders = true)
        os.write(app / "src" / "Main.elm", "module Main exposing (main)", createFolders = true)
        os.write(changing, first)

        val legacyFirst  = PathRef(app / "src").sig
        val firstTracked = ElmProjectSnapshot
          .trackInputs(app / "elm.json", Seq(app / "src"), ElmInputLimits())
          .find(_.role == ElmProjectSnapshot.InputRole.Source)
          .get
        os.write.over(changing, second)
        val legacySecond  = PathRef(app / "src").sig
        val secondTracked = ElmProjectSnapshot
          .trackInputs(app / "elm.json", Seq(app / "src"), ElmInputLimits())
          .find(_.role == ElmProjectSnapshot.InputRole.Source)
          .get
        assert(legacyFirst == legacySecond)
        assert(firstTracked.fingerprint != secondTracked.fingerprint)
        os.write.over(changing, first)

        val module = new TrackingBuild(root / "workspace")
        UnitTester(module, sources).scoped { evaluator =>
          val initial = success(evaluator(module.app.compile))
          assert(os.read(initial.path) == first)
          assert(successResult(evaluator(module.app.compile)).evalCount == 0)

          os.write.over(module.moduleDir / "app" / "src" / "Collision.txt", second)
          val mutation = evaluator(module.app.compile).fold(
            failure => throw new java.lang.AssertionError(failure.toString),
            identity
          )
          assert(mutation.evalCount > 0)
          assert(os.read(mutation.value.path) == second)
        }
      }
    }

    test("provisioned Elm compiles a real minimal application without ambient tools") {
      withTempDir { root =>
        val sources = root / "sources"
        os.write(
          sources / "tools" / "package.json",
          """{"private":true,"dependencies":{"elm":"0.19.1-6"}}""",
          createFolders = true
        )
        os.write(
          sources / "app" / "elm.json",
          """{"type":"application","source-directories":["src"],"elm-version":"0.19.1","dependencies":{"direct":{"elm/core":"1.0.5","elm/json":"1.1.3"},"indirect":{}},"test-dependencies":{"direct":{},"indirect":{}}}""",
          createFolders = true
        )
        os.write(
          sources / "app" / "src" / "Main.elm",
          """module Main exposing (main)

import Platform
import Platform.Cmd as Cmd
import Platform.Sub as Sub

main : Program () () Never
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \message model -> never message
        , subscriptions = \_ -> Sub.none
        }
""",
          createFolders = true
        )

        val module = new RealElmBuild(root / "workspace")
        UnitTester(module, sources).scoped { evaluator =>
          val lockCommand = success(
            evaluator(
              module.packages.packageManagerCommand(
                Seq("install", "--package-lock-only", "--ignore-scripts", "--no-audit", "--no-fund")
              )
            )
          )
          val environment = ElmProcessEnvironment.create(
            root / "lock-state",
            Map("PATH" -> "/ambient-tools-must-not-be-used", "HOME" -> "/ambient-home")
          )
          ElmProcessEnvironment.initialize(environment)
          val _ = os.proc(lockCommand.executable.path.toString +: lockCommand.arguments)
            .call(cwd = module.moduleDir / "tools", env = environment, propagateEnv = false)
          val output = success(evaluator(module.app.compile))
          assert(os.isFile(output.path))
          assert(os.read(output.path).contains("Elm"))
        }
      }
    }
  }
}
