package org.finos
package morphir
package util

import morphir.util.Printer.given
import org.scalacheck.Prop._

class PrinterSuite extends munit.ScalaCheckSuite {

  property("The out the box Orinter with a Simple Renderer should properly print Booleans") {
    forAll { (v: Boolean) =>
      assertEquals(v.text, Printer.Text.Run(v.toString), v.tprint == v.toString)
    }
  }
  property("The out the box Printer with a Simple Renderer should properly print a sting") {
    forAll { (v: String) =>
      assertEquals(v.text == Printer.Text.Run(v), v.tprint == v)
    }
  }
}
