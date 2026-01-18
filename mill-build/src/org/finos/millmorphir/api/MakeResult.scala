package org.finos.millmorphir.api

import mill.PathRef
import mill.api.JsonFormatters.*
import upickle.default.*

case class MakeResult(
    makeArgs: MakeArgs,
    irFilePath: PathRef,
    commandArgs: Seq[String],
    workingDir: os.Path,
    morphirHashesPath: Option[PathRef] = None
) derives ReadWriter
