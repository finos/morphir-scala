package org.finos.morphir.service

import java.io.IOException
import org.finos.morphir.util.vfile._
import zio._

trait FileIOPlatformSpecific {
  val live: ULayer[FileIO] = ZLayer.succeed(FileIOLive)

  object FileIOLive extends FileIO {

    def readFileText(path: VPath): IO[IOException, String] =
      ZIO.fail(new IOException(s"FileIOLive::readFileText Not implemented"))

    def readLines(path: VPath): IO[IOException, List[String]] =
      ZIO.fail(new IOException(s"FileIOLive::readFileText Not implemented"))
  }
}
