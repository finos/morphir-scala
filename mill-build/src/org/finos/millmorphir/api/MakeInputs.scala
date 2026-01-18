package org.finos.millmorphir.api

import mill.api.*
import mill.api.JsonFormatters.*
import upickle.default.*

final case class MakeInputs(sourceFiles: Seq[PathRef], incrementalBuildFiles: Seq[PathRef]) derives ReadWriter
