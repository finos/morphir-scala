package org.finos.morphir.mir.sdk

import zio.Scope
import org.finos.morphir.mir.Type.Type
import org.finos.morphir.mir.{Gens, Path}
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object CommonSpec extends MorphirBaseSpec {
  def spec = suite("Common Spec")(
    suite("packageName")(
      test("should return the expected value") {
        assertTrue(Common.packageName.toPath == Path.fromString("Morphir.SDK"))
      }
    ),
    suite("tVar")(
      test("should work as expected") {
        check(Gens.words) { s =>
          val actual = Common.tVar(s)
          assertTrue(
            actual == Type.variable(s)
          )
        }
      }
    )
  )
}
