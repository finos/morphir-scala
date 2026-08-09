package org.finos.morphir.mill.elm

import mill.*
import org.finos.morphir.mill.javascript.*

trait ElmToolModule extends Module {
  def packageManager: JavaScriptPackageManagerModule

  def elmCommand(arguments: Seq[String]): Task[JavaScriptCommand] =
    packageManager.packageBinaryCommand(packageBinary"elm", arguments)
}
