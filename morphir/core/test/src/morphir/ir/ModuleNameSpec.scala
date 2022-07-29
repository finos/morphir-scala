package morphir.mir

import morphir.testing.MorphirBaseSpec
import zio.test.*

object ModuleNameSpec extends MorphirBaseSpec {
  import morphir.mir.Module.ModuleName
  def spec = suite("ModuleName Spec")(
    test("fromString") {
      assertTrue(ModuleName.fromString("Basics") == ModuleName.unsafeMake()("basics"))
    }
  )
}
