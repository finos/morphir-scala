package org.finos.morphir.util.vfile
import zio.test._
trait VPathSpecPlatformSpecific { self: VPathSpec.type =>
  def platformSpecificSuite = suite("PlatformSpecific")()
}
