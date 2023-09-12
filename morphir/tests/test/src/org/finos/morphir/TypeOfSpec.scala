package org.finos.morphir

import org.finos.morphir.mir.*
import org.finos.morphir.naming.*
import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.testing.model._
import zio.test.*

object TypeOfSpec extends MorphirBaseSpec {

  def collectionsSuite = suite("Collections")(
    test("It should be possible to get the proper type info for a list of strings") {
      val actual = TypeOf[List[String]]
      assertTrue(
        actual.fqName == Some(fqn"Morphir.SDK:List:List"),
        actual.getType.isReference,
        actual.typeInfo.isOpaque
      )
    }
  )

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

  def productsSuite = suite("Products")(
    suite("Case Classes/Records")(
      test("It should be possible to get type info on a case class annotated with @qualifiedModuleName") {
        val actual = TypeOf.gen[Person]
        assertTrue(
          actual.fqName == Some(fqn"Morphir.SDK.Test:Model:Person"),
          !actual.getType.isReference,
          !actual.typeInfo.isOpaque
        )
      },
      test("It should be possible to get type info on a case class annotated with @fullyQualifiedName") {
        val actual = TypeOf.gen[Employee]
        assertTrue(
          actual.fqName == Some(fqn"Morphir.SDK.Testing:Test.Models:Employee"),
          !actual.getType.isReference,
          !actual.typeInfo.isOpaque
        )
      }
    )
  )

  def sumTypesSuite = suite("Sum Types")()

  def spec = suite("TypeOfSpec")(
    collectionsSuite,
    primitivesSuite,
    productsSuite,
    sumTypesSuite
  )
}
