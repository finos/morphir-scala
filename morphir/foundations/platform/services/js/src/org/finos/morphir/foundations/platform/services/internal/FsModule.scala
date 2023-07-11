package org.finos.morphir.foundations.platform.services.internal

import fs.Fs
import path.Path
import process.Process

import scala.scalajs.js
import js.JSConverters._
object FsModule extends FsApi { self =>
  final type Path = String

  def root: Path = {
    val rootOpt: Option[String] = Path.parse(Process.cwd()).root.toOption
    rootOpt.getOrElse("") // TODO: This is a hack. We need to figure out how to handle this properly
  }

  def exists(path: Path): Boolean = Fs.existsSync(path)
}
