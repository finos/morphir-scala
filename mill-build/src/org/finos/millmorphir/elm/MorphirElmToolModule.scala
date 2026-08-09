package org.finos.millmorphir.elm

import mill.*
import org.finos.morphir.mill.javascript.npm.NpmPackageManagerModule
import org.finos.millmorphir.toolchain.NodeToolchainModule

/** Temporary old-package composition bridge; implementation lives in mill-morphir-elm. */
trait MorphirElmToolModule extends org.finos.morphir.mill.elm.morphir.MorphirElmToolModule {
  def nodeToolchain: NodeToolchainModule

  def toolManifest: T[PathRef] = Task.Source(
    mill.api.BuildCtx.workspaceRoot / "mill-plugins" / "morphir" / "elm" / "test-tools" / "morphir-elm" / "package.json"
  )

  def toolLock: T[PathRef] = Task.Source(
    mill.api.BuildCtx.workspaceRoot / "mill-plugins" / "morphir" / "elm" / "test-tools" / "morphir-elm" / "package-lock.json"
  )

  object packages extends NpmPackageManagerModule {
    def runtime = nodeToolchain

    override def npmProjectPaths: Seq[os.Path] = Seq(
      mill.api.BuildCtx.workspaceRoot / "mill-plugins" / "morphir" / "elm" / "test-tools" / "morphir-elm" /
        "package.json"
    )
    override def npmLockPaths: Seq[os.Path] = Seq(
      mill.api.BuildCtx.workspaceRoot / "mill-plugins" / "morphir" / "elm" / "test-tools" / "morphir-elm" /
        "package-lock.json"
    )
  }

  def packageManager = packages
}
