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

  sealed case class MorphirElmDriverLive(processIO: ProcessIO) extends MorphirElmDriver { self =>
    import zio.process._
    def develop(port: Int, host: String, projectDir: VPath, openInBrowser: Boolean = false): Task[Unit] = {
      val shutdownPromptJob = for {
        _ <- Console.printLine(
          s"Press enter to stop the Elm development server"
        )
        _ <- Console.readLine
      } yield ()

      for {
        _ <- ZIO.logDebug(s"\tport: $port")
        _ <- ZIO.logDebug(s"\thost: $host")
        _ <- ZIO.logDebug(s"\tprojectDir: $projectDir")
        _ <- ZIO.logDebug(s"\topenInBrowser: $openInBrowser")
        serverFiber <- processIO.exec(
          "morphir-elm",
          "develop",
          "--port",
          port.toString,
          "--host",
          host,
          "--project-dir",
          projectDir.toString
        ).fork
        _ <- if (openInBrowser) launchInBrowser(s"http://$host:$port") else ZIO.unit
        // Now let's wait for the server to shutdown or for the user to press enter
        _ <- serverFiber.join race shutdownPromptJob
      } yield ()
    }

    def init(morphirHomeDir: VPath, projectDir: VPath): Task[Unit] = for {
      _ <- Console.printLine("Elm init command executing")
      _ <- Console.printLine(s"\tmorphirHomeDir: $morphirHomeDir")
      _ <- Console.printLine(s"\tprojectDir: $projectDir")
      _ <- Console.printLine("Elm init command executed")
    } yield ()

    def make(
        projectDir: VPath,
        output: VPath,
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

    def restore(elmHome: VPath, projectDir: VPath): Task[Unit] =
      for {
        _ <- Console.printLine("Elm restore command executed")
        _ <- Console.printLine(s"\telmHome: $elmHome")
        _ <- Console.printLine(s"\tprojectDir: $projectDir")
        _ <- Console.printLine("Elm restore command executed")
      } yield ()

    def test(projectDir: VPath): Task[Unit] =
      for {
        _ <- ZIO.logDebug(s"Executing tests...")
        _ <- ZIO.logDebug(s"\tprojectDir: $projectDir")
        _ <- processIO.exec(
          "morphir-elm",
          "test",
          "--project-dir",
          projectDir.toString
        )
      } yield ()

    private def launchInBrowser(url: String): ZIO[Any, Throwable, Unit] = {
      val notify = Console.printLine(s"Attempting to open $url in browser")
      for {
        os <- System.property("os.name")
        _ <- os match {
          case Some("Linux") => notify *> processIO.exec("xdg-open", url)
          case Some("Mac OS X") =>
            notify *> processIO.exec("open", url)
          case _ => ZIO.unit
        }
      } yield ()
    }
  }
}
