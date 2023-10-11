package org.finos.morphir
package ir

import org.finos.morphir.ir.StringOps
import org.finos.morphir.ir.Type.Field
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object FieldSpec extends MorphirBaseSpec {
  def spec = suite("Field Spec")(
    suite("Field Construction")(
      test("A Field can be created from a string and TypeExpr using <:>") {
        val actual = "name" <:> Type.variable("String")
        assertTrue(
          actual == Field("name", Type.variable("String"))
        )
      },
      test("A Field can be created from a string and a Type using as") {
        val actual = "name" <:> Type.variable("String")
        assertTrue(
          actual == Field("name", Type.variable("String"))
        )
      }
    )
  )
}
