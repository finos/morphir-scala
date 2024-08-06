#!/usr/bin/env -S scala-cli shebang -S 3
//> using toolkit latest
//> using dep com.lihaoyi::pprint::0.8.1
//> using dep com.lihaoyi::mainargs:0.7.1
//> using options -deprecation -unchecked

import mainargs.{main, arg, ParserForMethods, Flag}

object Main {
  @main
  def run(
      @arg(name = "scala-version", doc = "Scala version", short = 's')
      scalaVersion: Option[String],
      @arg(name = "cross-scala-versions", doc = "Cross Scala versions", short = 'x')
      crossScalaVersions: List[String],
      @arg(
        doc = "Enable JVM projects",
        short = 'j'
      )
      jvm: Flag,
      @arg(
        doc = "Enable Scala.js projects"
      )
      js: Flag,
      @arg(
        doc = "Enable native projects",
        short = 'n'
      )
      native: Flag
  ) = {
    var env = Map(
      "MORPHIR_BUILD_JVM_ENABLE"    -> "false",
      "MORPHIR_BUILD_JS_ENABLE"     -> "false",
      "MORPHIR_BUILD_NATIVE_ENABLE" -> "false"
    )
    if (jvm.value) env += "MORPHIR_BUILD_JVM_ENABLE"       -> "true"
    if (js.value) env += "MORPHIR_BUILD_JS_ENABLE"         -> "true"
    if (native.value) env += "MORPHIR_BUILD_NATIVE_ENABLE" -> "true"

    // If all flags are false, we enable all platforms
    if (env.values.forall(_ == "false")) {
      env = Map(
        "MORPHIR_BUILD_JVM_ENABLE"    -> "true",
        "MORPHIR_BUILD_JS_ENABLE"     -> "true",
        "MORPHIR_BUILD_NATIVE_ENABLE" -> "true"
      )
    }

    if (crossScalaVersions.nonEmpty) {
      env += "MORPHIR_BUILD_SCALA_CROSSSCALAVERSIONS" -> crossScalaVersions.mkString(",")
    }

    if (scalaVersion.nonEmpty) {
      env += "MORPHIR_BUILD_SCALA_DEFAULTVERSION" -> scalaVersion.get
    }

    // os.proc("./mill", "-i", "showBuildSettings").call(env = env, stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
    os.proc("./mill", "-i", "mill.idea.GenIdea/idea").call(
      env = env,
      stdin = os.Inherit,
      stdout = os.Inherit,
      stderr = os.Inherit
    )

  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args.toIndexedSeq)
}
