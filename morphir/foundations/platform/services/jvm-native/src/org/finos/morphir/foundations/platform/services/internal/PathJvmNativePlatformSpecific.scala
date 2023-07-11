package org.finos.morphir.foundations.platform.services.internal

import java.io.File

trait PathJvmNativePlatformSpecific extends PathApi {
  def sep: String = File.separator
}
