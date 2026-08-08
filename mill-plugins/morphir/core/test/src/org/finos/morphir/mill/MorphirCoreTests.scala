package org.finos.morphir.mill

import java.nio.file.Files

import mill.PathRef
import scala.compiletime.testing.typeCheckErrors
import upickle.default.*
import utest.*

object MorphirCoreTests extends TestSuite {
  private def parsed(value: String)(using SourceLocation): ModuleId =
    ModuleId.parse(value).fold(throw _, identity)

  private def withTempDir[A](f: os.Path => A): A = {
    val directory = os.Path(Files.createTempDirectory("morphir-core-test"))
    try f(directory)
    finally os.remove.all(directory)
  }

  val tests = Tests {
    test("runtime parsing accepts portable lower-case module IDs") {
      val accepted = Seq(
        "examples.defaults-tests",
        "morphir-elm.sdks.morphir-unit-test",
        "example-project",
        "a_b.c-1",
        "a",
        "0"
      )
      accepted.foreach { value =>
        val result = ModuleId.parse(value)
        assert(result.map(_.value) == Right(value))
      }
      assert(ModuleId.parse("examples.defaults-tests") == Right(moduleId"examples.defaults-tests"))
      assert(moduleId"examples.defaults-tests".segments.toSeq == Seq("examples", "defaults-tests"))
    }

    test("runtime parsing rejects non-portable and reserved module IDs") {
      val rejected = Seq(
        "",
        ".",
        "..",
        "examples..defaults-tests",
        "Examples.defaults-tests",
        "examples/con",
        "examples\\con",
        "examples.con",
        "con.examples",
        "examples.nul.txt",
        "examples.com1",
        "examples.lpt9",
        "-examples",
        "examples-",
        "_examples",
        "examples_"
      )
      rejected.foreach { value =>
        assert(ModuleId.parse(value).isLeft)
      }
    }

    test("runtime errors are typed exceptions carrying the caller location") {
      val Left(error) = ModuleId.parse("Examples.Bad"): @unchecked
      assert(error.isInstanceOf[IllegalArgumentException])
      assert(error.input == "Examples.Bad")
      assert(error.location.file.endsWith("MorphirCoreTests.scala"))
      assert(error.location.line > 0)
      assert(error.getMessage.contains(error.location.render))
    }

    test("literal interpolation shares validation and rejects dynamic values") {
      val valid = moduleId"examples.defaults-tests"
      assert(valid.value == "examples.defaults-tests")

      val invalid = typeCheckErrors(
        """import org.finos.morphir.mill.*; moduleId"Examples.Bad""""
      )
      val reserved = typeCheckErrors(
        """import org.finos.morphir.mill.*; moduleId"examples.con""""
      )
      val interpolated = typeCheckErrors(
        """import org.finos.morphir.mill.*; val dynamic = "example"; moduleId"$dynamic""""
      )
      assert(invalid.nonEmpty)
      assert(invalid.head.message.toLowerCase.contains("module id"))
      assert(invalid.head.column > 0)
      assert(reserved.nonEmpty)
      assert(interpolated.nonEmpty)
      assert(interpolated.head.message.contains("does not accept interpolation"))
    }

    test("uPickle round trips only validated module IDs") {
      val id   = moduleId"examples.defaults-tests"
      val json = write(id)
      assert(read[ModuleId](json) == id)

      val failure = scala.util.Try(read[ModuleId](write("Examples.Bad"))).failed.get
      val causes  = Iterator
        .iterate(Option(failure))(_.flatMap(error => Option(error.getCause)))
        .takeWhile(_.nonEmpty)
        .flatten
        .toSeq
      assert(causes.exists(_.isInstanceOf[ModuleId.Error]))
      assert(causes.collectFirst { case error: ModuleId.Error => error }.exists(_.location.render.nonEmpty))
    }

    test("raw strings cannot inhabit ModuleId") {
      val errors = typeCheckErrors(
        """
          import org.finos.morphir.mill.ModuleId
          val raw: String = "examples.defaults-tests"
          val id: ModuleId = raw
        """
      )
      assert(errors.nonEmpty)
    }

    test("IR artifacts carry typed identity and full content identity") {
      withTempDir { root =>
        val ir = root / "morphir-ir.json"
        os.write(ir, "first")
        val first = MorphirIrArtifact.fromFile(moduleId"examples.defaults-tests", PathRef(ir))
        assert(first.moduleId == moduleId"examples.defaults-tests")
        assert(first.path.path == ir)
        assert(first.irFilePath == first.path)
        assert(first.sha256.length == 64)
        assert(first.sha256.forall(character => character.isDigit || character >= 'a' && character <= 'f'))

        os.write.over(ir, "second")
        val second = MorphirIrArtifact.fromFile(moduleId"examples.defaults-tests", PathRef(ir))
        assert(second.path.path == first.path.path)
        assert(second.sha256 != first.sha256)

        val dependency = MorphirDependencyArtifact.fromArtifact(second)
        assert(dependency.moduleId == second.moduleId)
        assert(dependency.irFilePath == second.path)
        assert(dependency.sha256 == second.sha256)

        val roundTrip = read[MorphirIrArtifact](write(second))
        assert(roundTrip == second)
      }
    }

    test("artifact hashing rejects a symlink swap between validation and open") {
      if (!scala.util.Properties.isWin) withTempDir { root =>
        val artifact = root / "morphir-ir.json"
        val outside  = root / "outside.json"
        os.write(artifact, "expected")
        os.write(outside, "outside")

        val failure = scala.util.Try {
          MorphirArtifactIdentity.sha256(
            artifact,
            beforeOpen = () => {
              os.remove(artifact)
              Files.createSymbolicLink(artifact.toNIO, outside.toNIO)
              ()
            }
          )
        }.failed.get
        assert(failure.isInstanceOf[IllegalArgumentException])
        assert(failure.getMessage.contains("changed") || failure.getMessage.contains("non-symbolic-link"))
      }
    }

    test("frontend-neutral project configuration rewrites typed local IR dependencies") {
      val config = MorphirProjectConfig(
        name = "Example.Project",
        sourceDirectory = "src",
        exposedModules = List("Example"),
        dependencies = List("elm/core")
      )
      assert(config.localDependencies.isEmpty)
      assert(
        config.withLocalDependencies(List(".morphir-deps/one/morphir-ir.json")).localDependencies ==
          List(".morphir-deps/one/morphir-ir.json")
      )
      assert(read[MorphirProjectConfig](write(config)) == config)
    }

    test("runtime fixture source carries every IR artifact content identity") {
      withTempDir { root =>
        def artifact(name: String): MorphirIrArtifact = {
          val path = root / s"$name.json"
          os.write(path, name)
          MorphirIrArtifact.fromFile(moduleId"fixtures.runtime", PathRef(path))
        }
        val fixtures = ClassicRuntimeFixtureSet(
          evaluator = artifact("evaluator"),
          defaults = artifact("defaults"),
          unitTestFramework = artifact("unit-test-framework"),
          unitTestExample = artifact("unit-test-example"),
          unitTestFailing = artifact("unit-test-failing"),
          unitTestPassing = artifact("unit-test-passing"),
          unitTestIncomplete = artifact("unit-test-incomplete")
        )
        val rendered = ClassicRuntimeFixtureSource.render(fixtures)
        assert(rendered.contains("object GeneratedRuntimeFixtures"))
        Seq(
          fixtures.evaluator,
          fixtures.defaults,
          fixtures.unitTestFramework,
          fixtures.unitTestExample,
          fixtures.unitTestFailing,
          fixtures.unitTestPassing,
          fixtures.unitTestIncomplete
        ).foreach(artifact => assert(rendered.contains(artifact.sha256)))
        assert(
          ClassicRuntimeFixtureSource.escapeScalaString("C:\\fixtures\\morphir-ir.json") ==
            "C:\\\\fixtures\\\\morphir-ir.json"
        )
      }
    }
  }
}
