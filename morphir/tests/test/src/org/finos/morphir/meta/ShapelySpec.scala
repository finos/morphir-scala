package org.finos.morphir.meta

import org.finos.morphir.testing.MorphirBaseSpec
import zio.{test as _, *}
import zio.test.*
import Shapely._
object ShapelySpec extends MorphirBaseSpec {
  import ShapelyTestExamples._
  def spec = suite("ShapelySpec")(
    suite("CaseClass")(
      testRoundtrip(Foo("hello")),
      testRoundtrip(Bar("goodbye", -1)),
      testRoundtrip(lower("hello"))
    ),
    suite("CaseClass0")(
      testRoundtrip(Baz()),
      testRoundtrip(Car)
    ),
//    suite("SealedTrait")(
//      testRoundtrip[Gaz, SealedTrait4[Gaz, Foo, Bar, Baz, Car.type]](Foo("hello"): Gaz)
//    ),
    suite("SealedTrait")(
//      testRoundtrip[Gaz, SealedTrait4[Gaz, Foo, Bar, Baz, Car.type]](Foo("hello"): Gaz)
      test("Roundtrip testing for Gaz") {
        val original = Foo("hello"): Gaz
        val S        = Shapely[Gaz, SealedTrait4[Gaz, Foo, Bar, Baz, Car.type]]
        val encoded  = S.to(original)
        val decoded  = S.from(encoded)
        assertTrue(decoded == original)
      }
    ),
    suite("PolyCaseClass")(
      testRoundtrip(PolyFoo("hello", 1))
    ),
    suite("PolySealedTrait")(
      // testRoundtrip(PolyFoo("hello", 1): Poly[Any]),
//      test("Roundtrip testing of Poly[Any]") {
//        val original = PolyFoo("hello", 1): Poly[Any]
//        val S        = Shapely[Poly[Any], SealedTrait2[Poly[Any], PolyFoo, PolyBar]]
//        val encoded  = S.to(original)
//        val decoded  = S.from(encoded)
//        assertTrue(decoded == original)
//      }
    ),
    suite("Recursive")(
      // testRoundtrip(Branch(List(Leaf("hello"), Leaf("world"))): ATree)
    )
  )

  def testRoundtrip[A, B](a: A)(implicit S: Shapely[A, B]) =
    test(s"Roundtrip testing for: $a")(
      assertTrue(S.from(S.to(a)) == a)
    )
}
