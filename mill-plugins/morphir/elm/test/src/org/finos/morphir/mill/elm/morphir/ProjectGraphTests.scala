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
  private final case class ReferenceTarget(
      packageName: Seq[Seq[String]],
      moduleName: Seq[Seq[String]],
      localName: Seq[String]
  )

  private val unpublishedSourceValue = ReferenceTarget(
    packageName = Seq(Seq("unpublished"), Seq("source")),
    moduleName = Seq(Seq("dependency")),
    localName = Seq("unpublished", "source", "value")
  )

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

    object customSource extends MorphirElmModule {
      def morphirElmTool    = outer.tool
      override def moduleId = Task(moduleId"test.custom-source")

      override def morphirProjectConfigPath = outer.moduleDir / "custom-source" / "morphir.json"
      override def elmProjectConfigPaths    = Seq(outer.moduleDir / "custom-source" / "elm.json")
      override def morphirProjectSourcePath = outer.moduleDir / "custom-source" / "elm-src"
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

  private def name(value: ujson.Value): Option[Seq[String]] =
    value match {
      case array: ujson.Arr if array.value.forall(_.isInstanceOf[ujson.Str]) =>
        Some(array.value.toSeq.map(_.str))
      case _ => None
    }

  private def namePath(value: ujson.Value): Option[Seq[Seq[String]]] =
    value match {
      case array: ujson.Arr =>
        val names = array.value.toSeq.map(name)
        Option.when(names.forall(_.isDefined))(names.flatten)
      case _ => None
    }

  private def referenceTarget(value: ujson.Value): Option[ReferenceTarget] =
    value match {
      case reference: ujson.Arr
          if reference.value.length == 3 && reference.value.headOption.contains(ujson.Str("Reference")) =>
        reference.value(2) match {
          case fqName: ujson.Arr if fqName.value.length == 3 =>
            for {
              packageName <- namePath(fqName.value(0))
              moduleName  <- namePath(fqName.value(1))
              localName   <- name(fqName.value(2))
            } yield ReferenceTarget(packageName, moduleName, localName)
          case _ => None
        }
      case _ => None
    }

  private def containsReferenceTo(value: ujson.Value, expected: ReferenceTarget): Boolean =
    referenceTarget(value).contains(expected) ||
      (value match {
        case array: ujson.Arr => array.value.exists(containsReferenceTo(_, expected))
        case obj: ujson.Obj   => obj.value.values.exists(containsReferenceTo(_, expected))
        case _                => false
      })

  val tests = Tests {
    test("reference matching rejects the right local name under the wrong package and module") {
      val wrongTarget = ujson.Arr(
        "Reference",
        ujson.Obj(),
        ujson.Arr(
          ujson.Arr(ujson.Arr("wrong"), ujson.Arr("package")),
          ujson.Arr(ujson.Arr("wrong", "module")),
          ujson.Arr("unpublished", "source", "value")
        )
      )
      assert(!containsReferenceTo(wrongTarget, unpublishedSourceValue))
    }

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
          assert(containsReferenceTo(ir, unpublishedSourceValue))
        }
      }
    }

    test("generated IR supports a configured non-src source root") {
      withTempDir { root =>
        val sources = root / "sources"
        os.copy.over(
          resource("unpublished-source-dependency/unpublished-dependency"),
          sources / "custom-source",
          createFolders = true
        )
        os.move(sources / "custom-source" / "src", sources / "custom-source" / "elm-src")
        val morphirJson = ujson.read(os.read(sources / "custom-source" / "morphir.json"))
        morphirJson("sourceDirectory") = "elm-src"
        os.write.over(sources / "custom-source" / "morphir.json", morphirJson.render(indent = 2))
        os.copy.over(resource("morphir-elm"), sources / "tool", createFolders = true)

        val module = new SourceDependencyBuild(root / "workspace")
        UnitTester(module, sources).scoped { evaluator =>
          val artifact = success(evaluator(module.customSource.morphirIR))
          val ir       = ujson.read(os.read(artifact.path.path))
          assert(ir("formatVersion").num == 3)
          assert(ir("distribution").arr.head.str == "Library")
        }
      }
    }
  }
}
