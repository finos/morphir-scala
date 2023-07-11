package org.finos.morphir.foundations.platform.services.internal

trait FsApi {
  def exists(path: String): Boolean  
}
