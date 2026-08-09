package org.finos.morphir.mill.elm.morphir

import mill.*
import org.finos.morphir.mill.elm.ElmToolModule
import org.finos.morphir.mill.javascript.*

trait MorphirElmToolModule extends ElmToolModule {
  def morphirElmInstall: T[JavaScriptInstall] = Task {
    packageManager.lockFiles().foreach(lock => MorphirElmLock.validate(lock.path))
    packageManager.install()
  }

  def morphirElmCommand(arguments: Seq[String]): Task[JavaScriptCommand] = Task.Anon {
    val _ = morphirElmInstall()
    MorphirElmCommand(packageManager, arguments)()
  }

  def morphirElmExecutable: Task[JavaScriptCommand] = Task.Anon {
    morphirElmCommand(Seq.empty)()
  }
}
