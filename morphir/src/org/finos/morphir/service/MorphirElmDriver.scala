package org.finos.morphir.service
import org.finos.morphir.util.vfile._
import zio._

trait MorphirElmDriver {
  def develop(port: Int, host: String, projectDir: VFilePath): Task[Unit]
  def init(morphirHomeDir: VFilePath, projectDir: VFilePath): Task[Unit]
  def make(projectDir: VFilePath, output: VFilePath, fallbackCli: Boolean = false): Task[Seq[VFile]]
  def restore(elmHome: VFilePath, projectDir: VFilePath): Task[Unit]
}

object MorphirElmDriver extends MorphirElmDriverPlatformSpecific {

  def develop(port: Int, host: String, projectDir: VFilePath): ZIO[MorphirElmDriver, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirElmDriver](_.develop(port, host, projectDir))

  def init(morphirHomeDir: VFilePath, projectDir: VFilePath): ZIO[MorphirElmDriver, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirElmDriver](_.init(morphirHomeDir, projectDir))

  def make(
      projectDir: VFilePath,
      output: VFilePath,
      fallbackCli: Boolean = false
  ): ZIO[MorphirElmDriver, Throwable, Seq[VFile]] =
    ZIO.serviceWithZIO[MorphirElmDriver](_.make(projectDir, output, fallbackCli))

  def restore(elmHome: VFilePath, projectDir: VFilePath): ZIO[MorphirElmDriver, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirElmDriver](_.restore(elmHome, projectDir))
}
