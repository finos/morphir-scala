package org.finos.millmorphir.api

import mill.api.JsonFormatters.*
import millbuild.util.Collections.*
import upickle.default.*

final case class MakeArgs(
    projectDir: os.Path,
    output: os.Path,
    indentJson: Boolean,
    typesOnly: Boolean,
    fallbackCli: Option[Boolean]
) derives ReadWriter { self =>
  def useFallbackCli: Boolean = self.fallbackCli.getOrElse(false)

  def toCommandArgs: Seq[String] =
    Seq("make")
      .appendWhen(Option(projectDir).nonEmpty)("--project-dir", projectDir.toString())
      .appendWhen(Option(output).nonEmpty)("--output", output.toString())
      .appendIf(indentJson)("--indent-json")
      .appendIf(typesOnly)("--types-only")
      .appendIf(useFallbackCli)("--fallback-cli")

  def toCommandArgs(cli: String): Seq[String] = Seq(cli) ++ toCommandArgs
}
