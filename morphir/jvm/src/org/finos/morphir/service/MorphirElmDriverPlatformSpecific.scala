package org.finos.morphir.service

import org.finos.morphir.util.vfile._
import zio._

trait MorphirElmDriverPlatformSpecific {
  // val live: ZLayer[ProcessIO, Nothing, MorphirElmDriver] = ZLayer {
  //   for {
  //     processIO <- ZIO.service[ProcessIO]
  //   } yield MorphirElmDriverLive(processIO)
  // }

  // HACK: Scala 3 blows up when I try and use the above code. I'm not sure why.
  val live: ZLayer[Any, Nothing, MorphirElmDriver] = ProcessIO.live >>> ZLayer {
    for {
      processIO <- ZIO.service[ProcessIO]
    } yield MorphirElmDriverLive(processIO)
  }

  sealed case class MorphirElmDriverLive(processIO: ProcessIO) extends MorphirElmDriver {
    def develop(port: Int, host: String, projectDir: VFilePath): Task[Unit] = for {
      _ <- Console.printLine("Elm develop command executing")
      _ <- ZIO.logDebug(s"\tport: $port")
      _ <- ZIO.logDebug(s"\thost: $host")
      _ <- ZIO.logDebug(s"\tprojectDir: $projectDir")
      exitCode <- processIO.exec(
        "morphir-elm",
        "develop",
        "--port",
        port.toString,
        "--host",
        host,
        "--project-dir",
        projectDir.toString
      )
      _ <- Console.printLine(s"\texitCode: $exitCode")
      _ <- Console.printLine("Elm develop command executed")
    } yield ()

    def init(morphirHomeDir: VFilePath, projectDir: VFilePath): Task[Unit] = for {
      _ <- Console.printLine("Elm init command executing")
      _ <- Console.printLine(s"\tmorphirHomeDir: $morphirHomeDir")
      _ <- Console.printLine(s"\tprojectDir: $projectDir")
      _ <- Console.printLine("Elm init command executed")
    } yield ()

    def make(
        projectDir: VFilePath,
        output: VFilePath,
        typesOnly: Boolean = false,
        fallbackCli: Boolean = false,
        indentJson: Boolean = false
    ): Task[Seq[VFile]] = for {
      _ <- ZIO.logDebug(s"\toutput: $output")
      _ <- ZIO.logDebug(s"\ttypesOnly: $typesOnly")
      _ <- ZIO.logDebug(s"\tfallbackCli: $fallbackCli")
      _ <- ZIO.logDebug(s"\tindentJson: $indentJson")
      exitCode <- processIO.exec(
        "morphir-elm",
        "make",
        "--project-dir",
        projectDir.toString,
        "--output",
        output.toString,
        "--types-only",
        typesOnly.toString,
        "--fallback-cli",
        fallbackCli.toString,
        "--indent-json",
        indentJson.toString
      )
      _ <- Console.printLine(s"\texitCode: $exitCode")
    } yield Seq.empty

    def restore(elmHome: VFilePath, projectDir: VFilePath): Task[Unit] =
      for {
        _ <- Console.printLine("Elm restore command executed")
        _ <- Console.printLine(s"\telmHome: $elmHome")
        _ <- Console.printLine(s"\tprojectDir: $projectDir")
        _ <- Console.printLine("Elm restore command executed")
      } yield ()
  }
}
