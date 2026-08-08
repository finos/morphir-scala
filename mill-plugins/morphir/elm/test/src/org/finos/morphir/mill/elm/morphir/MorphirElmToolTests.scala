package org.finos.morphir.mill.elm.morphir

import java.nio.file.Files

import mill.*
import mill.api.ExecResult
import mill.testkit.{TestRootModule, UnitTester}
import org.finos.morphir.mill.elm.ElmProcessEnvironment
import org.finos.morphir.mill.javascript.node.NodeRuntimeModule
import org.finos.morphir.mill.javascript.npm.NpmPackageManagerModule
import scala.compiletime.testing.typeCheckErrors
import utest.*

object MorphirElmToolTests extends TestSuite {
  private def withTempDir[A](body: os.Path => A): A = {
    val root = os.Path(Files.createTempDirectory("morphir-elm-tool-test"))
    try body(root)
    finally os.remove.all(root)
  }

  private def success[A](result: Either[ExecResult.Failing[A], UnitTester.Result[A]]): A =
    result.fold(failure => throw new java.lang.AssertionError(failure.toString), _.value)

  private final class RealMorphirBuild(workspace: os.Path) extends TestRootModule(workspace) { outer =>
    lazy val millDiscover = mill.api.Discover[this.type]

    object runtime extends NodeRuntimeModule

    object packages extends NpmPackageManagerModule {
      def runtime                  = outer.runtime
      override def npmProjectPaths = Seq(outer.moduleDir / "tool" / "package.json")
      override def npmLockPaths    = Seq(outer.moduleDir / "tool" / "package-lock.json")
    }

    object tool extends MorphirElmToolModule {
      def packageManager = outer.packages
    }
  }

  val tests = Tests {
    test("Morphir Elm tool uses neutral typed package-manager APIs") {
      val errors = typeCheckErrors(
        """
          import org.finos.morphir.mill.elm.morphir.*
          import org.finos.morphir.mill.javascript.*
          trait Consumer extends MorphirElmToolModule {
            def packageManager: JavaScriptPackageManagerModule
          }
        """
      )
      assert(errors.isEmpty)
      assert(MorphirElmTool.Version == "2.89.0")
    }

    test("pinned production tool lock is strict before any process") {
      val resource = Option(getClass.getClassLoader.getResource("morphir-elm/package-lock.json")).get
      val lock     = os.Path(java.nio.file.Paths.get(resource.toURI))
      MorphirElmLock.validate(lock)

      val original         = ujson.read(os.read(lock))
      val packages         = original("packages").obj
      val registry         = packages.collectFirst { case (path, value) if path.nonEmpty => path -> value }.get
      val (path, metadata) = registry

      def rejected(mutator: ujson.Obj => Unit, expected: String): Unit = {
        val copy = ujson.read(original.render()).obj
        mutator(copy)
        val temp = os.temp(prefix = "invalid-morphir-lock", suffix = ".json", deleteOnExit = true)
        os.write.over(temp, copy.render())
        val error = scala.util.Try(MorphirElmLock.validate(temp)).failed.get
        assert(error.getMessage.contains(expected))
      }

      rejected(lock => lock("packages").obj(path).obj.remove("resolved"), "resolved")
      rejected(lock => lock("packages").obj(path).obj.remove("integrity"), "integrity")
      rejected(
        lock => lock("packages").obj(path).obj("resolved") = ujson.Str("git+https://example.test/tool.git"),
        "registry"
      )
      Seq("git+https://example.test/tool.git", "file:../tool", "link:../tool").foreach { dependency =>
        rejected(
          lock => lock("packages").obj("").obj("dependencies").obj("morphir-elm") = ujson.Str(dependency),
          "non-registry"
        )
      }
      rejected(lock => lock("packages").obj(path).obj("hasInstallScript") = ujson.Bool(true), "install scripts")
      assert(metadata.obj.contains("resolved"))
    }

    test("provisioned npm installs and invokes the pinned Morphir Elm tool without ambient tools") {
      withTempDir { root =>
        val resource = Option(getClass.getClassLoader.getResource("morphir-elm/package.json")).get
        val fixture  = os.Path(java.nio.file.Paths.get(resource.toURI)) / os.up
        val sources  = root / "sources"
        os.copy.over(fixture, sources / "tool", createFolders = true)

        val module = new RealMorphirBuild(root / "workspace")
        UnitTester(module, sources).scoped { evaluator =>
          val command     = success(evaluator(module.tool.morphirElmCommand(Seq("--version"))))
          val environment = ElmProcessEnvironment.create(
            root / "tool-state",
            Map("PATH" -> "/ambient-tools-must-not-be-used", "HOME" -> "/ambient-home")
          )
          ElmProcessEnvironment.initialize(environment)
          val result = os.proc(command.executable.path.toString +: command.arguments)
            .call(cwd = module.moduleDir, env = environment, propagateEnv = false)
          assert(result.out.text().contains(MorphirElmTool.Version))
          assert(!command.arguments.exists(Set("npm", "npx", "bun", "mise")))
        }
      }
    }
  }
}
