package org.finos.morphir.service
import zio._
import java.io.IOException
import org.finos.morphir.util.vfile.VPath

trait FileIO {
  def readFileText(path: VPath): IO[IOException, String]
  def readLines(path: VPath): IO[IOException, List[String]]
}

object FileIO extends FileIOPlatformSpecific {
  def readFileText(path: VPath): ZIO[FileIO, IOException, String] =
    ZIO.serviceWithZIO[FileIO](_.readFileText(path))

  def readLines(path: VPath): ZIO[FileIO, IOException, List[String]] =
    ZIO.serviceWithZIO[FileIO](_.readLines(path))
}
