package org.finos.morphir.extensibility

import org.finos.morphir.MorphirTag
import org.finos.morphir.naming._
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

import SdkModuleDescriptors.*
object SdkModuleDescriptorsSpec extends MorphirBaseSpec {
  def spec = suite("SdkModuleDescriptorsSpec")(
  )

  def morphirTagSuite = suite("MorphirTag")(
    test("MorphirTag should be available for the Basics module via direct summoning")(
      assertTrue(MorphirTag[Morphir.SDK.Basics.type] == Morphir.SDK.Basics.morphirTag)
    )
  )

}
