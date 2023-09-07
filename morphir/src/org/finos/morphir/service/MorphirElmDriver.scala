package org.finos.morphir.service
import org.finos.morphir.util.vfile._
import zio._

/**
 * A service that provides a driver for invoking the `morphir-elm` CLI as well as performing Elm specific tasks which
 * may not be directily backed by the `morphir-elm` CLI.
 */
trait MorphirElmDriver {
  /// Execute the `morphir-elm develop` command
  def develop(port: Int, host: String, projectDir: VFilePath, openInBrowser: Boolean = false): Task[Unit]

  /// Initialize the current directory/workspace for use with Morphir's Elm tooling.
  def init(morphirHomeDir: VFilePath, projectDir: VFilePath): Task[Unit]

  /// Compule Elm sources into Morphir IR.
  def make(
      projectDir: VFilePath,
      output: VFilePath,
      typesOnly: Boolean = false,
      fallbackCli: Boolean = false,
      indentJson: Boolean = false
  ): Task[Seq[VFile]]

  /// Restore the Elm dependencies for the current project/workspace.
  def restore(elmHome: VFilePath, projectDir: VFilePath): Task[Unit]
}

/// Provides constructors and accessors for a MorphirElmDriver.
object MorphirElmDriver extends MorphirElmDriverPlatformSpecific {

  /// Execute the `morphir-elm develop` command
  def develop(
      port: Int,
      host: String,
      projectDir: VFilePath,
      openInBrowser: Boolean = false
  ): ZIO[MorphirElmDriver, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirElmDriver](_.develop(port, host, projectDir, openInBrowser))

  /// Initialize the current directory/workspace for use with Morphir's Elm tooling.
  def init(morphirHomeDir: VFilePath, projectDir: VFilePath): ZIO[MorphirElmDriver, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirElmDriver](_.init(morphirHomeDir, projectDir))

  /// Compule Elm sources into Morphir IR.
  def make(
      projectDir: VFilePath,
      output: VFilePath,
      typesOnly: Boolean = false,
      fallbackCli: Boolean = false,
      indentJson: Boolean = false
  ): ZIO[MorphirElmDriver, Throwable, Seq[VFile]] =
    ZIO.serviceWithZIO[MorphirElmDriver](_.make(
      projectDir = projectDir,
      output = output,
      typesOnly = typesOnly,
      fallbackCli = fallbackCli,
      indentJson = indentJson
    ))

  /// Restore the Elm dependencies for the current project/workspace.
  def restore(elmHome: VFilePath, projectDir: VFilePath): ZIO[MorphirElmDriver, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirElmDriver](_.restore(elmHome, projectDir))
}
