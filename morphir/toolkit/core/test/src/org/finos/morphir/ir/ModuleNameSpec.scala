package org.finos.morphir.ir

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*

object ModuleNameSpec extends MorphirBaseSpec {
  import org.finos.morphir.ir.Module.QualifiedModuleName
  def spec = suite("ModuleName Spec")(
    test("fromString") {
      assertTrue(QualifiedModuleName.fromString("Basics") == QualifiedModuleName.unsafeMake()("basics"))
    }
  )
}
