package org.finos.millmorphir.elm

import mill.*
import org.finos.morphir.mill.elm.ElmToolModule
import org.finos.morphir.mill.javascript.*
import org.finos.morphir.mill.javascript.npm.NpmPackageManagerModule
import org.finos.millmorphir.toolchain.NodeToolchainModule

/**
 * Repository elm-format toolchain: npm + elm-tooling pin of elm-format 0.8.7.
 *
 * Mill's npm install uses `--ignore-scripts`, so this module runs `elm-tooling install` after `npm ci` and resolves the
 * native binary under `node_modules/.bin/elm-format`.
 */
trait ElmFormatToolModule extends ElmToolModule {
  def nodeToolchain: NodeToolchainModule

  def toolManifest: T[PathRef] = Task.Source(
    mill.api.BuildCtx.workspaceRoot / "toolchains" / "elm-format" / "package.json"
  )

  def toolLock: T[PathRef] = Task.Source(
    mill.api.BuildCtx.workspaceRoot / "toolchains" / "elm-format" / "package-lock.json"
  )

  def elmToolingManifest: T[PathRef] = Task.Source(
    mill.api.BuildCtx.workspaceRoot / "toolchains" / "elm-format" / "elm-tooling.json"
  )

  object packages extends NpmPackageManagerModule {
    def runtime = nodeToolchain

    override def npmProjectPaths: Seq[os.Path] = Seq(
      mill.api.BuildCtx.workspaceRoot / "toolchains" / "elm-format" / "package.json",
      mill.api.BuildCtx.workspaceRoot / "toolchains" / "elm-format" / "elm-tooling.json"
    )
    override def npmLockPaths: Seq[os.Path] = Seq(
      mill.api.BuildCtx.workspaceRoot / "toolchains" / "elm-format" / "package-lock.json"
    )
  }

  def packageManager = packages

  def elmFormatInstall: T[JavaScriptInstall] = Task {
    val _         = toolManifest()
    val _         = toolLock()
    val _         = elmToolingManifest()
    val installed = packageManager.install()
    ElmFormatToolModule.linkElmFormat(nodeToolchain.runtimeExecutable(), installed)
    installed
  }

  /**
   * elm-tooling places a native binary (or Windows shim) under `.bin`, not an npm package with a `bin` field, so
   * packageBinary discovery cannot find it.
   */
  override def elmFormatCommand(arguments: Seq[String]): Task[JavaScriptCommand] = Task.Anon {
    val installed = elmFormatInstall()
    val binary    = ElmFormatToolModule.elmFormatBinary(installed)
    JavaScriptCommand(PathRef(binary), arguments)
  }

  /**
   * Cached resolved elm-format command (empty args). Named task so `./mill show` works.
   *
   * Prefer `Task.Worker` when the value is AutoCloseable; `JavaScriptCommand` is not, so this is a disk-cached `Task`
   * that still depends on install before resolving the binary.
   */
  def elmFormatExecutable: T[JavaScriptCommand] = Task {
    elmFormatCommand(Seq.empty)()
  }
}

object ElmFormatToolModule {
  private def elmFormatBinary(installed: JavaScriptInstall): os.Path = {
    val binary = installed.root.path / "node_modules" / ".bin" / "elm-format"
    if (!os.exists(binary))
      throw new IllegalStateException(
        s"elm-format binary was not linked at $binary after elm-tooling install"
      )
    binary
  }

  private def linkElmFormat(node: PathRef, installed: JavaScriptInstall): Unit = {
    val elmTooling = installed.root.path / "node_modules" / "elm-tooling" / "index.js"
    if (!os.exists(elmTooling))
      throw new IllegalStateException(
        s"elm-tooling package is missing under ${installed.root.path}"
      )
    val result = os.proc(node.path.toString, elmTooling.toString, "install")
      .call(cwd = installed.root.path, stdout = os.Pipe, stderr = os.Pipe, check = false)
    if (result.exitCode != 0)
      throw new IllegalStateException(
        s"elm-tooling install failed (exit ${result.exitCode}): ${result.err.text()}"
      )
    val _ = elmFormatBinary(installed)
  }
}
