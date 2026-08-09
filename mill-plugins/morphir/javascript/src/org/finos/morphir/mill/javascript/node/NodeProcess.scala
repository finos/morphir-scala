package org.finos.morphir.mill.javascript.node

import mill.PathRef
import org.finos.morphir.mill.javascript.JavaScriptCommand

object NodeProcess {
  def runtime(nodeExecutable: PathRef, arguments: Seq[String]): JavaScriptCommand =
    JavaScriptCommand(nodeExecutable, arguments)

  def npm(nodeExecutable: PathRef, npmCli: PathRef, arguments: Seq[String]): JavaScriptCommand =
    JavaScriptCommand(nodeExecutable, npmCli.path.toString +: arguments)
}
