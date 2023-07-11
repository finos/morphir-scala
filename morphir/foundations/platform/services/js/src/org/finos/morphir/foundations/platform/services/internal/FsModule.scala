package org.finos.morphir.foundations.platform.services.internal

import fs.Fs

object FsModule extends FsApi {
  def exists(path: String): Boolean = {
    Fs.existsSync(path)
  }  
}
