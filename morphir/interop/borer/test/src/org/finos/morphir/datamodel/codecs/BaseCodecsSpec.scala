package org.finos.morphir.datamodel

import io.bullet.borer._
import org.finos.morphir.datamodel.codecs.BaseCodecs._
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
import java.time._

object BaseCodecsSpec extends MorphirBaseSpec {
  def spec = suite("BaseCodecsSpec")(
    test("Month") {
      val data  = Month.APRIL
      val bytes = Cbor.encode(data).toByteArray
      assertTrue(Cbor.decode(bytes).to[Month].value == data)
    },
    test("DayOfWeek") {
      val data  = DayOfWeek.MONDAY
      val bytes = Cbor.encode(data).toByteArray
      assertTrue(Cbor.decode(bytes).to[DayOfWeek].value == data)
    },
    test("LocalTime") {
      val data  = LocalTime.now()
      val bytes = Cbor.encode(data).toByteArray
      assertTrue(Cbor.decode(bytes).to[LocalTime].value == data)
    },
    test("LocalDate") {
      val data  = LocalDate.now()
      val bytes = Cbor.encode(data).toByteArray
      assertTrue(Cbor.decode(bytes).to[LocalDate].value == data)
    }
  )
}
