package org.finos.morphir.util.vfile

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object VFileSpec extends MorphirBaseSpec with VFileSpecPlatformSpecific {
  def spec = suite("VFileSpec")(
    platformSpecificSuite
  )
}
