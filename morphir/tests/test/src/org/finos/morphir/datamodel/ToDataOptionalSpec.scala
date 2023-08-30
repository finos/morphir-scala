package org.finos.morphir.datamodel

import org.finos.morphir.testing.MorphirBaseSpec
import zio.{test => _, _}
import zio.test._

object ToDataOptionalSpec extends MorphirBaseSpec {
  def spec = suite("ToDataOptionalSpec")(
    test("Option") {
      assertTrue(
        Deriver.toData(Option(123)) == Data.Optional.Some(Data.Int(123))
      )
    },
    test("Option Some") {
      assertTrue(
        Deriver.toData(Some(123)) ==
          Data.Optional.Some(Data.Int(123))
      )
    },
    test("Option None") {
      assertTrue(
        Deriver.toData(None) ==
          Data.Optional.None(Concept.Nothing())
      )
    }
  )

}
