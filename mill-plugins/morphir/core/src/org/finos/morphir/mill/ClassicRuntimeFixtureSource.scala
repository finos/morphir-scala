package org.finos.morphir.mill

final case class ClassicRuntimeFixtureSet(
    evaluator: MorphirIrArtifact,
    defaults: MorphirIrArtifact,
    unitTestFramework: MorphirIrArtifact,
    unitTestExample: MorphirIrArtifact,
    unitTestFailing: MorphirIrArtifact,
    unitTestPassing: MorphirIrArtifact,
    unitTestIncomplete: MorphirIrArtifact
)

object ClassicRuntimeFixtureSource {
  def escapeScalaString(value: String): String =
    value.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c    => c.toString
    }

  def render(fixtures: ClassicRuntimeFixtureSet): String = {
    def field(name: String, artifact: MorphirIrArtifact): String =
      s"  // $name SHA-256: ${artifact.sha256}\n" +
        s"  val $name: java.nio.file.Path = java.nio.file.Paths.get(\"${escapeScalaString(artifact.path.path.toString)}\")"

    Seq(
      "package org.finos.morphir.runtime.fixtures",
      "",
      "object GeneratedRuntimeFixtures {",
      field("evaluator", fixtures.evaluator),
      field("defaults", fixtures.defaults),
      field("unitTestFramework", fixtures.unitTestFramework),
      field("unitTestExample", fixtures.unitTestExample),
      field("unitTestFailing", fixtures.unitTestFailing),
      field("unitTestPassing", fixtures.unitTestPassing),
      field("unitTestIncomplete", fixtures.unitTestIncomplete),
      "}",
      ""
    ).mkString("\n")
  }
}
