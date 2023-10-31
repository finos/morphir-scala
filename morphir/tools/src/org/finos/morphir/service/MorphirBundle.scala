package org.finos.morphir.service

import org.finos.morphir.util.vfile.*
import zio.*

trait MorphirBundle {
  def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): Task[Unit]
}

object MorphirBundle extends MorphirBundlePlatformSpecific {
  def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): ZIO[MorphirBundle, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirBundle](_.bundle(outputBundleIRFilePath, irFiles))
}
