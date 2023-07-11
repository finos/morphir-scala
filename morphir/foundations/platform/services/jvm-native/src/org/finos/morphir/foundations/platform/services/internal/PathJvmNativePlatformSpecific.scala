package org.finos.morphir.foundations.platform.services.internal

import java.io.File

private[services] trait PathJvmNativePlatformSpecific extends PathApi {
  def delimiter: String = File.pathSeparator
  def sep: String = File.separator
}
