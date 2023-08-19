package org.finos.morphir.util.vfile
import zio.test._
trait VFilePathSpecPlatformSpecific { self: VFilePathSpec.type =>
  def platformSpecificSuite = suite("PlatformSpecific")()
}


