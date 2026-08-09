package org.finos.morphir.mill.javascript

import mill.PathRef
import upickle.default.{ReadWriter, macroRW}

final case class JavaScriptInstall(root: PathRef, projectFiles: Seq[PathRef], lockFiles: Seq[PathRef])

object JavaScriptInstall {
  given ReadWriter[JavaScriptInstall] = macroRW
}
