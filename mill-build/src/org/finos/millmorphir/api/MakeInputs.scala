package org.finos.millmorphir.api
import mill.api._
import mill.api.JsonFormatters._

final case class MakeInputs(sourceFiles: Seq[PathRef], incrementalBuildFiles: Seq[PathRef])
object MakeInputs {
  implicit val jsonFormatter: upickle.default.ReadWriter[MakeInputs] = upickle.default.macroRW
}
