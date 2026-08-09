package org.finos.morphir.mill.javascript

import mill.*

trait JavaScriptPackageManagerModule extends Module {
  def runtime: JavaScriptRuntimeModule
  def projectFiles: T[Seq[PathRef]]
  def lockFiles: T[Seq[PathRef]]
  def install: T[JavaScriptInstall]
  def packageManagerCommand(arguments: Seq[String]): Task[JavaScriptCommand]
  def packageBinaryCommand(binary: PackageBinary, arguments: Seq[String]): Task[JavaScriptCommand]
}
