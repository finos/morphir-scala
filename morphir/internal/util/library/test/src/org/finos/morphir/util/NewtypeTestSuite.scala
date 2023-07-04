package org.finos.morphir.util
import NewtypeTestTypes._
class NewtypeTestSuite extends munit.FunSuite {
  test("When using assetions allows valid values at compile-time"){
    assertEquals(Natural(0), Natural.unsafeWrap(0))
  }
  test("Meter example test"){
    import NewtypeTestSuite.Meter
    val x = Meter(3.4)
    val y = Meter(4.3)
    val z = x add y
    assertEquals(Meter.unwrap(z), (3.4 + 4.3))
  }
}

object NewtypeTestSuite {
  type Meter = Meter.Type
  object Meter extends Newtype[Double] {
    implicit final class MeterOps(private val self: Meter) extends AnyVal {
      def add(that: Meter): Meter =
        Meter.wrap(Meter.unwrap(self) + Meter.unwrap(that))
    }
  }
}


