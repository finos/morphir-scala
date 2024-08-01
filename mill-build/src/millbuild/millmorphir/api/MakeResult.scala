package millbuild.millmorphir.api 

import mill.PathRef
import mill.api.JsonFormatters._

case class MakeResult(
    makeArgs:    MakeArgs,
    irFilePath:  PathRef,
    commandArgs: Seq[String],
    workingDir:  os.Path,
  )
object MakeResult {
    implicit val jsonFormatter: upickle.default.ReadWriter[MakeResult] = upickle.default.macroRW
}