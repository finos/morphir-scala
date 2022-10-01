package org.finos
package morphir

import java.nio.file.Paths
import morphir.cli.{CliSetup, CliCommand, MorphirCliBuildInfo}
import zio.Console.printLine
import zio.cli.HelpDoc.Span.text
import zio.cli._
import zio.Scope
import zio.ZIOAppArgs
import org.finos.morphir.cli.CliCommandRouter

object Main extends ZIOCliDefault:
  override def cliApp = CliApp.make(
    name = MorphirCliBuildInfo.product,
    version = MorphirCliBuildInfo.version,
    summary = text(MorphirCliBuildInfo.description),
    command = CliSetup.morphir
  )(CliCommandRouter.handleCliCommand(_))
