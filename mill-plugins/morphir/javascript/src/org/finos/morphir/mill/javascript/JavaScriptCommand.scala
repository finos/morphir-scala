package org.finos.morphir.mill.javascript

import mill.PathRef

final case class JavaScriptCommand(executable: PathRef, arguments: Seq[String])
