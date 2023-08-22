package org.finos.morphir.util

import Printer.given
import zio.test._
import org.finos.morphir.testing.MorphirBaseSpec

object PrinterSpec extends MorphirBaseSpec {
  def spec = suite("PrinterSpec")(
    test("The out the box Orinter with a Simple Renderer should properly print Booleans") {
      check(Gen.boolean) { (v: Boolean) =>
        assertTrue(v.text == Printer.Text.Run(v.toString), v.tprint == v.toString)
      }
    },
    test("The out the box Printer with a Simple Renderer should properly print a sting") {
      check(Gen.alphaNumericString) { (v: String) =>
        assertTrue(v.text == Printer.Text.Run(v), v.tprint == v)
      }
    }
  )
}
