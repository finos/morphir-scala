package org.finos.morphir.foundations.platform.services.internal

trait PathPlatformSpecific extends PathApi {
  def sep: String = Path.sep
}


