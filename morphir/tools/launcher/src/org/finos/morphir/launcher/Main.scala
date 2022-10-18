package org.finos.morphir.launcher

import coursier.{Dependency, Module, ModuleName, Organization}
import mainargs.{ParserForMethods, main}

/**
 * Fetch the necessary JARs for a Morphir development environment using Coursier.
 */
// Avoid using, for instance, ZIO, for dependency injection to keep dependencies minimal.
final case class Main(morphirVersion: MorphirVersion, coursier: Coursier) {
  private val morphirCliDep =
    Dependency(
      Module(Organization("org.finos.morphir"), ModuleName("morphir-tools-cli_3")),
      morphirVersion.version
    )
  def run(): Unit =
    coursier.fetch(
      morphirCliDep
    )
}

object Main {
  @main
  def run(): Unit = Main(MorphirVersionLive, CoursierLive).run()

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args.toIndexedSeq)
}
