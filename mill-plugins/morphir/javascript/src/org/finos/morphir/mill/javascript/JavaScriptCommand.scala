package org.finos.morphir.mill.javascript

import mill.PathRef
import upickle.default.{ReadWriter, macroRW}

final case class JavaScriptCommand(executable: PathRef, arguments: Seq[String])

object JavaScriptCommand {
  given ReadWriter[JavaScriptCommand] = macroRW
}
