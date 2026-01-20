package org.finos.morphir.ir.v4

import zio._
import zio.json._
import java.nio.file.{Files, Paths, Path}
import org.finos.morphir.ir.json.MorphirJsonDecodingSupportV4._

object DistributionLoader {
  val live: ZLayer[VfsLoader, Nothing, DistributionLoader] = ZLayer.fromFunction(DistributionLoaderLive(_))

  def load(path: String): ZIO[DistributionLoader, Throwable, Distribution] =
    ZIO.serviceWithZIO[DistributionLoader](_.load(path))
}

trait DistributionLoader {
  def load(path: String): Task[Distribution]
}

final case class DistributionLoaderLive(vfsLoader: VfsLoader) extends DistributionLoader {

  /**
   * Loads a Distribution from the given path. Auto-detects whether the path is a VFS directory or a Classic single-file
   * JSON.
   */
  override def load(path: String): Task[Distribution] =
    for {
      p <- ZIO.attempt(Paths.get(path))
      _ <- ZIO.fail(new java.io.FileNotFoundException(s"Path not found: $path")).unless(Files.exists(p))

      dist <- if (Files.isDirectory(p)) {
        // VFS Mode: Delegate to VfsLoader
        vfsLoader.loadRequest(path)
      } else {
        // Classic Mode or explicitly provided JSON file
        loadClassicDistribution(p)
      }
    } yield dist

  private def loadClassicDistribution(path: Path): Task[Distribution] =
    for {
      json <- ZIO.attempt(Files.readString(path))
      dist <- ZIO.fromEither(json.fromJson[Distribution]).mapError(e =>
        new RuntimeException(s"Failed to decode distribution from $path: $e")
      )
    } yield dist
}
