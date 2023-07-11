package org.finos.morphir.foundations.platform.services.internal

import org.finos.morphir.foundations.platform.services.internal.FsApi

object FsModule extends FsApi {         
  def exists(path: String): Boolean = {
    java.nio.file.Files.exists(java.nio.file.Paths.get(path))
  }
}
