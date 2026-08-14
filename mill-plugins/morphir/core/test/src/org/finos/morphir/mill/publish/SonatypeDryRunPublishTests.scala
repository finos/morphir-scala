package org.finos.morphir.mill.publish

import java.nio.file.Files

import mill.*
import mill.api.ExecResult
import mill.javalib.JavaModule
import mill.javalib.SonatypeCentralPublishModule
import mill.javalib.publish.{License, PomSettings, VersionControl}
import mill.testkit.{TestRootModule, UnitTester}
import utest.*

/**
 * Isolated mill-testkit proof that Morphir CI `GPG_*` names, converted through [[MillSonatypeEnv]], reach Mill's
 * `publishSonatypeCentral` signing path without contacting Central (`MILL_TESTS_PUBLISH_DRY_RUN=1`).
 */
object SonatypeDryRunPublishTests extends TestSuite {

  private final class PublishBuild(workspace: os.Path) extends TestRootModule(workspace) {
    lazy val millDiscover = mill.api.Discover[this.type]

    object foo extends JavaModule with SonatypeCentralPublishModule {
      def publishVersion = "0.0.0-test"
      def pomSettings    = PomSettings(
        description = "morphir mill publish dry-run fixture",
        organization = "org.finos.morphir.test",
        url = "https://github.com/finos/morphir-scala",
        licenses = Seq(License.MIT),
        versionControl = VersionControl.github("finos", "morphir-scala"),
        developers = Seq.empty
      )
    }
  }

  private def withTempDir[A](f: os.Path => A): A = {
    val directory = os.Path(Files.createTempDirectory("morphir-sonatype-dry-run"))
    try f(directory)
    finally os.remove.all(directory)
  }

  private def success[A](result: Either[ExecResult.Failing[A], UnitTester.Result[A]]): UnitTester.Result[A] =
    result.fold(failure => throw new java.lang.AssertionError(failure.toString), identity)

  val tests = Tests {
    test("GPG_* conversion reaches publishSonatypeCentral signing in mill dry-run") {
      if !EphemeralPgp.gpgAvailable then {
        println("skipping sonatype dry-run publish: gpg not on PATH")
      } else {
        withTempDir { root =>
          val sources = root / "sources"
          os.write(
            sources / "foo" / "src" / "foo" / "Foo.java",
            """package foo;
              |public class Foo {}
              |""".stripMargin,
            createFolders = true
          )
          val passphrase = "morphir-ci-test-pass"
          val millEnv    = MillSonatypeEnv.fromEnvOrThrow(
            Map(
              "GPG_PRIVATE_KEY"   -> EphemeralPgp.generateArmoredSecret(passphrase),
              "GPG_PASSPHRASE"    -> passphrase,
              "SONATYPE_USERNAME" -> "dry-user",
              "SONATYPE_PASSWORD" -> "dry-pass"
            )
          )
          val module = new PublishBuild(root / "workspace")
          UnitTester(
            module,
            sources,
            env = sys.env ++ millEnv.toProcessEnv ++ Map("MILL_TESTS_PUBLISH_DRY_RUN" -> "1"),
            resetSourcePath = true
          ).scoped { evaluator =>
            success(evaluator(module.foo.publishSonatypeCentral(sources = false, docs = false)))
            val out  = module.moduleDir / "out"
            val ascs = os.walk(out).filter(_.last.endsWith(".asc"))
            assert(os.walk(out).exists(_.last == "repository"))
            assert(ascs.nonEmpty)
          }
        }
      }
    }
  }
}
