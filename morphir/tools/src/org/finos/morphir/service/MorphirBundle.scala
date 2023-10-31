package org.finos.morphir.service

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.MorphirIRVersion
import org.finos.morphir.util.vfile.*
import scala.io.Source
import zio.*
import zio.json.*

trait MorphirBundle {
  def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): Task[Unit]
}

object MorphirBundle {


  def bundle(outputBundleIRFilePath: VPath, irFiles: List[VPath]): ZIO[MorphirBundle, Throwable, Unit] =
    ZIO.serviceWithZIO[MorphirBundle](_.bundle(outputBundleIRFilePath, irFiles))
}
