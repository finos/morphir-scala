package org.finos.morphir.foundations.platform.services.internal

trait PathPlatformSpecific extends PathApi {
  def delimiter:String = Path.delimiter
  def sep: String = Path.sep
}


