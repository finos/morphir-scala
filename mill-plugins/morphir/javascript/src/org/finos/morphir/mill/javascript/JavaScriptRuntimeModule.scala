package org.finos.morphir.mill.javascript

import mill.*

trait JavaScriptRuntimeModule extends Module {
  def runtimeVersion: T[String]
  def runtimeHome: T[PathRef]
  def runtimeExecutable: T[PathRef]
  def runtimeCommand(arguments: Seq[String]): Task[JavaScriptCommand]
}
