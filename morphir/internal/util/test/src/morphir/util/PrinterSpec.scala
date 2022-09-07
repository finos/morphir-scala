package morphir.util

import zio.Console
import zio.test.*
import morphir.util.Printer.given

object PrinterSpec extends ZIOSpecDefault:
  def spec = suite("PrinterSpec")(
    suite("Out the box Printers")(
      suite("With Simple Renderer")(
        test("Printing Booleans") {
          check(Gen.boolean) { v =>
            assertTrue(v.text == Printer.Text.Run(v.toString), v.tprint == v.toString)
          }
        },
        test("Printing strings") {
          check(Gen.alphaNumericString) { v =>
            assertTrue(v.text == Printer.Text.Run(v), v.tprint == v)
          }
        }
      )
    )
  )
end PrinterSpec
