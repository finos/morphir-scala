package org.finos.morphir.foundations.platform.services.internal

import org.finos.morphir.foundations.platform.services.internal.FsApi

object FsModule extends FsApi {
  type Path = String

  def root: Path = java.nio.file.Paths.get("user.dir").getRoot().toString()

  def exists(path: String): Boolean =
    java.nio.file.Files.exists(java.nio.file.Paths.get(path))
}
