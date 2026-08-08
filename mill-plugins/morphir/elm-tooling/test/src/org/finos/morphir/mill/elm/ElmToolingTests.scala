package org.finos.morphir.mill.elm

import java.nio.file.Files

import mill.*
import mill.api.ExecResult
import mill.testkit.{TestRootModule, UnitTester}
import org.finos.morphir.mill.javascript.*
import org.finos.morphir.mill.javascript.node.NodeRuntimeModule
import org.finos.morphir.mill.javascript.npm.NpmPackageManagerModule
import scala.compiletime.testing.typeCheckErrors
import utest.*

object ElmToolingTests extends TestSuite {
  private def withTempDir[A](body: os.Path => A): A = {
    val root = os.Path(Files.createTempDirectory("elm-tooling-test"))
    try body(root)
    finally os.remove.all(root)
  }

  private def success[A](result: Either[ExecResult.Failing[A], UnitTester.Result[A]]): A =
    result.fold(failure => throw new java.lang.AssertionError(failure.toString), _.value)

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

  val tests = Tests {
    test("Elm tooling exposes neutral package-manager APIs and an authoritative compile task") {
      val errors = typeCheckErrors(
        """
          import mill.*
          import org.finos.morphir.mill.elm.*
          import org.finos.morphir.mill.javascript.*
          trait Consumer extends ElmModule {
            def elmTool: ElmToolModule
            override def elmJson: T[PathRef] = super.elmJson
            override def elmSources: T[Seq[PathRef]] = super.elmSources
            override def compile: T[PathRef] = super.compile
          }
          trait Tool extends ElmToolModule {
            def packageManager: JavaScriptPackageManagerModule
          }
        """
      )
      assert(errors.isEmpty)
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
