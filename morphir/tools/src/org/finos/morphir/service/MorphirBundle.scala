package org.finos.morphir.service

import java.nio.file.Path
import org.finos.morphir.util.vfile.*
import zio.*

trait MorphirBundle {
  def bundle(outputPath: VPath, irFiles: List[VPath]): Task[Unit]
  def bundle(outputPath: Path, irFiles: List[Path]): Task[Unit] =
    bundle(VPath(outputPath), irFiles.map(VPath(_)))

  def library(outputDir: VPath, irFiles: List[VPath]): Task[Unit]
  def library(outputDir: Path, irFiles: List[Path]): Task[Unit] =
    library(VPath(outputDir), irFiles.map(VPath(_)))

}

object MorphirBundle extends MorphirBundlePlatformSpecific {
  def bundle(outputPath: VPath, irFiles: List[VPath]): ZIO[MorphirBundle, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirBundle](_.bundle(outputPath, irFiles))

  def bundle(outputPath: Path, irFiles: List[Path]): ZIO[MorphirBundle, Throwable, Unit] =
    bundle(VPath(outputPath), irFiles.map(VPath(_)))

  def library(outputDir: VPath, irFiles: List[VPath]): ZIO[MorphirBundle, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirBundle](_.library(outputDir, irFiles))

  def library(outputDir: Path, irFiles: List[Path]): ZIO[MorphirBundle, Throwable, Unit] =
    library(VPath(outputDir), irFiles.map(VPath(_)))
}
