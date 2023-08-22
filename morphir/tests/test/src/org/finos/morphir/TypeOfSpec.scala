package org.finos.morphir

import org.finos.morphir.mir._
import org.finos.morphir.naming._
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object TypeOfSpec extends MorphirBaseSpec {

  def collectionsSuite = suite("Collections")()

  def primitivesSuite = suite("Primitives")(
    test("It should be possible to get the proper type info for a string") {
      val actual = TypeOf[String]
      assertTrue(
        actual.fqName == Some(fqn"Morphir.SDK:String:String"),
        actual.getType.isReference,
        actual.typeInfo.isOpaque
      )
    },
    test("It should be possible to get the proper type infor for an int ") {
      val actual = TypeOf[Int]
      assertTrue(
        actual.fqName == Some(fqn"Morphir.SDK:Basics:Int"),
        actual.getType.isReference,
        actual.typeInfo.isOpaque
      )
    }
  )

  def spec = suite("TypeOfSpec")(
    collectionsSuite,
    primitivesSuite
  )
}
