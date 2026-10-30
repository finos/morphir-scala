package millbuild

import millbuild.PomCheck.Platform
import utest.*

object PomCheckTests extends TestSuite {

  private def pom(dependencies: String, managed: String = ""): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<project>
       |  <dependencies>$dependencies</dependencies>
       |  <dependencyManagement><dependencies>$managed</dependencies></dependencyManagement>
       |</project>""".stripMargin

  private def dep(artifact: String, version: Option[String] = None): String =
    s"<dependency><groupId>io.getkyo</groupId><artifactId>$artifact</artifactId>" +
      version.fold("")(v => s"<version>$v</version>") + "</dependency>"

  val tests = Tests {
    test("platform follows the Mill module path") {
      assert(Platform.of("morphir.connector.github.js") == Platform.Js)
      assert(Platform.of("morphir.connector.github.native") == Platform.Native)
      assert(Platform.of("morphir.connector.github.jvm") == Platform.Jvm)
      assert(Platform.of("morphir.js") == Platform.Js)
    }

    test("a versioned dependency, or a versionless one with a pin, is publishable") {
      assert(PomCheck.problems(pom(dep("kyo-core_3", Some("1.0.0-RC7"))), Platform.Jvm).isEmpty)
      assert(PomCheck.problems(pom(dep("kyo-config_3"), dep("kyo-config_3", Some("1.0.0-RC7"))), Platform.Jvm).isEmpty)
    }

    test("regression: the 0.5.0-M05 to M10 Scala.js POM") {
      // The JVM artifact, no version, and the pin keyed by the platformed name.
      val problems =
        PomCheck.problems(pom(dep("kyo-config_3"), dep("kyo-config_sjs1_3", Some("1.0.0-RC7"))), Platform.Js)
      assert(
        problems == Seq(
          "io.getkyo:kyo-config_3 has no version and no dependencyManagement pin",
          "io.getkyo:kyo-config_3 is a JVM artifact; expected the sjs1_3 variant"
        )
      )
    }

    test("the fixed Scala.js and Scala Native POMs pass") {
      assert(
        PomCheck.problems(pom(dep("kyo-config_sjs1_3"), dep("kyo-config_sjs1_3", Some("1.0.0-RC7"))), Platform.Js)
          .isEmpty
      )
      assert(PomCheck.problems(pom(dep("kyo-config_native0.5_3", Some("1.0.0-RC7"))), Platform.Native).isEmpty)
    }

    test("a Scala.js POM may name the Scala.js library, which publishes under a 2.13 suffix") {
      assert(PomCheck.problems(pom(dep("scalajs-library_2.13", Some("1.22.0"))), Platform.Js).isEmpty)
    }

    test("a dependencyManagement entry without a version is not a pin") {
      assert(
        PomCheck.problems(pom(dep("kyo-http_3"), dep("kyo-http_3")), Platform.Jvm) ==
          Seq("io.getkyo:kyo-http_3 has no version and no dependencyManagement pin")
      )
    }
  }
}
