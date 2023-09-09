package org.finos.morphir.service
import zio._
import java.io.IOException
import org.finos.morphir.util.vfile.VFilePath

trait FileIO {
  def readFileText(path: VFilePath): IO[IOException, String]
  def readLines(path: VFilePath): IO[IOException, List[String]]
}

object FileIO extends FileIOPlatformSpecific {
  def readFileText(path: VFilePath): ZIO[FileIO, IOException, String] =
    ZIO.serviceWithZIO[FileIO](_.readFileText(path))

  def readLines(path: VFilePath): ZIO[FileIO, IOException, List[String]] =
    ZIO.serviceWithZIO[FileIO](_.readLines(path))
}
