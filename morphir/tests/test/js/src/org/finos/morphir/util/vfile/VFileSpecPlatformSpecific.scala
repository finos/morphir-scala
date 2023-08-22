package org.finos.morphir.util.vfile

import zio.test._

trait VFileSpecPlatformSpecific { self: VFileSpec.type =>
  def platformSpecificSuite = suite("PlatformSpecific")()
}
