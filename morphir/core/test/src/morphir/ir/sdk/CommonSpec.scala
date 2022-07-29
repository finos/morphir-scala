package morphir.mir.sdk

import zio.Scope
import morphir.mir.Type.Type
import morphir.mir.{Gens, Path}
import morphir.testing.MorphirBaseSpec
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
