package org.finos.morphir.mill

import java.nio.file.Files

import mill.*
import mill.api.ExecResult
import mill.testkit.{TestRootModule, UnitTester}
import utest.*

object MorphirScalaModuleTests extends TestSuite {
  private final class GeneratedScalaBuild(workspace: os.Path) extends TestRootModule(workspace)
      with MorphirScalaModule {
    lazy val millDiscover = mill.api.Discover[this.type]

    def scalaVersion = scala.util.Properties.versionNumberString

    private def irRoot: T[PathRef] = Task.Source(moduleDir / "ir")

    private def irSha256: T[String] = Task.Input {
      MorphirArtifactIdentity.sha256(moduleDir / "ir" / "Collision.txt")
    }

    override def generatedMorphirSources: T[GeneratedMorphirSources] = Task {
      val input       = irRoot().path / "Collision.txt"
      val inputDigest = irSha256()
      val sourceRoot  = Task.dest / "src"
      val value       = os.read(input).trim
      os.write(
        sourceRoot / "example" / "GeneratedModel.scala",
        s"""package example

object GeneratedModel {
  val value: String = "$value"
}
""",
        createFolders = true
      )
      GeneratedMorphirSources(PathRef(sourceRoot), inputDigest)
    }
  }

  private def withTempDir[A](f: os.Path => A): A = {
    val directory = os.Path(Files.createTempDirectory("morphir-scala-module-test"))
    try f(directory)
    finally os.remove.all(directory)
  }

  private def success[A](result: Either[ExecResult.Failing[A], UnitTester.Result[A]]): UnitTester.Result[A] =
    result.fold(failure => throw new java.lang.AssertionError(failure.toString), identity)

  private def fixture: os.Path = {
    val resource = Option(getClass.getClassLoader.getResource("generated-scala-project/ir/Collision.txt"))
      .getOrElse(throw new java.lang.AssertionError("generated Scala fixture is not on the test classpath"))
    os.Path(java.nio.file.Paths.get(resource.toURI)) / os.up / os.up
  }

  val tests = Tests {
    test("normal Scala compile consumes generated Morphir sources and invalidates for same-path IR changes") {
      withTempDir { root =>
        val sources = root / "sources"
        os.copy(fixture, sources, createFolders = true)
        val module = new GeneratedScalaBuild(root / "workspace")

        UnitTester(module, sources).scoped { evaluator =>
          val liveIr = module.moduleDir / "ir" / "Collision.txt"
          os.write.over(liveIr, "collision-00156726")
          os.write.over(module.moduleDir / "ir" / "Main.elm", "module Main exposing (main)")
          val firstCompile = success(evaluator(module.compile))
          assert(firstCompile.evalCount > 0)
          assert(success(evaluator(module.compile)).evalCount == 0)

          val generatedBefore = success(evaluator(module.generatedMorphirSources))
          assert(os.read(generatedBefore.value.sourceRoot.path / "example" / "GeneratedModel.scala").contains(
            "collision-00156726"
          ))

          val firstLegacySignature = PathRef(liveIr / os.up).sig
          os.write.over(liveIr, "collision-00163008")
          assert(PathRef(liveIr / os.up).sig == firstLegacySignature)

          val generationMutation = success(evaluator(module.generatedMorphirSources))
          assert(generationMutation.evalCount > 0)
          val generatedAfter = generationMutation.value
          assert(generatedAfter.inputSha256 != generatedBefore.value.inputSha256)
          assert(os.read(generatedAfter.sourceRoot.path / "example" / "GeneratedModel.scala").contains(
            "collision-00163008"
          ))
          assert(success(evaluator(module.generatedMorphirSources)).evalCount == 0)

          val secondCompile = success(evaluator(module.compile))
          assert(secondCompile.evalCount > 0)
          assert(success(evaluator(module.compile)).evalCount == 0)
        }
      }
    }
  }
}
