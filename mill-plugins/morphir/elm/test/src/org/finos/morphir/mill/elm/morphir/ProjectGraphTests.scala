package org.finos.morphir.mill.elm.morphir

import java.nio.file.{Files, Paths}

import mill.*
import mill.api.ExecResult
import mill.testkit.{TestRootModule, UnitTester}
import org.finos.morphir.mill.*
import org.finos.morphir.mill.javascript.node.NodeRuntimeModule
import org.finos.morphir.mill.javascript.npm.NpmPackageManagerModule
import utest.*

object ProjectGraphTests extends TestSuite {
  private final class SourceDependencyBuild(workspace: os.Path) extends TestRootModule(workspace) { outer =>
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

    object unpublishedDependency extends MorphirElmModule {
      def morphirElmTool    = outer.tool
      override def moduleId = Task(moduleId"test.unpublished-source-dependency")

      override def morphirProjectConfigPath = outer.moduleDir / "unpublished-dependency" / "morphir.json"
      override def elmProjectConfigPaths    = Seq(outer.moduleDir / "unpublished-dependency" / "elm.json")
      override def morphirProjectSourcePath = outer.moduleDir / "unpublished-dependency" / "src"
    }

    object consumer extends MorphirElmModule {
      def morphirElmTool = outer.tool

      override def morphirModuleDeps = Seq(outer.unpublishedDependency)
    }
  }

  private def success[A](result: Either[ExecResult.Failing[A], UnitTester.Result[A]]): A =
    result.fold(failure => throw new java.lang.AssertionError(failure.toString), _.value)

  private def resource(name: String): os.Path = {
    val url = Option(getClass.getClassLoader.getResource(name))
      .getOrElse(throw new IllegalStateException(s"Missing test resource: $name"))
    os.Path(Paths.get(url.toURI))
  }

  private def withTempDir[A](body: os.Path => A): A = {
    val root = os.Path(Files.createTempDirectory("morphir-elm-project-graph-test"))
    try body(root)
    finally os.remove.all(root)
  }

  private def isName(value: ujson.Value, expected: Seq[String]): Boolean =
    value match {
      case array: ujson.Arr => array.value.toSeq == expected.map(ujson.Str(_))
      case _                => false
    }

  private def containsName(value: ujson.Value, expected: Seq[String]): Boolean =
    isName(value, expected) ||
      (value match {
        case array: ujson.Arr => array.value.exists(containsName(_, expected))
        case obj: ujson.Obj   => obj.value.values.exists(containsName(_, expected))
        case _                => false
      })

  private def containsReferenceTo(value: ujson.Value, expectedName: Seq[String]): Boolean =
    value match {
      case array: ujson.Arr =>
        val isReference = array.value.headOption.contains(ujson.Str("Reference"))
        (isReference && containsName(array, expectedName)) ||
        array.value.exists(containsReferenceTo(_, expectedName))
      case obj: ujson.Obj => obj.value.values.exists(containsReferenceTo(_, expectedName))
      case _              => false
    }

  val tests = Tests {
    test("generated IR contains a used symbol from an unpublished Elm source dependency") {
      withTempDir { root =>
        val sources = root / "sources"
        os.copy.over(resource("unpublished-source-dependency"), sources, createFolders = true)
        os.copy.over(resource("morphir-elm"), sources / "tool", createFolders = true)
        val consumerElmDependencies = ujson
          .read(os.read(sources / "consumer" / "elm.json"))("dependencies")
          .obj
          .keySet
        assert(!consumerElmDependencies.contains("finos/unpublished-source-dependency"))
        assert(!os.exists(sources / "unpublished-dependency" / "morphir-ir.json"))

        val module = new SourceDependencyBuild(root / "workspace")
        UnitTester(module, sources).scoped { evaluator =>
          val artifact = success(evaluator(module.consumer.morphirIR))
          val ir       = ujson.read(os.read(artifact.path.path))
          assert(containsReferenceTo(ir, Seq("unpublished", "source", "value")))
        }
      }
    }
  }
}
