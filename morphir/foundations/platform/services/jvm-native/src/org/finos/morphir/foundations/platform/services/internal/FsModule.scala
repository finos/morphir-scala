package org.finos.morphir.foundations.platform.services.internal

import org.finos.morphir.foundations.platform.services.internal.FsApi

object FsModule extends FsApi {
  def exists(path: String): Boolean = {
    val file = new java.io.File(path)
    file.exists()
  }
}
