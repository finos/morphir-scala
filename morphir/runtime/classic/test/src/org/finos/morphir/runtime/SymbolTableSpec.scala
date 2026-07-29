package org.finos.morphir.runtime

import org.finos.morphir
import org.finos.morphir.runtime.{SymbolTable, SymbolValue}
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*

object SymbolTableSpec extends MorphirBaseSpec {
  def spec = suite("SymbolTableSpec")(
    test("Should support binding a value") {
      val sut    = SymbolTable.empty
      val n      = morphir.runtime.Symbol.variable("n")
      val actual = sut.bind(n, 100)
      assertTrue(actual(n) == SymbolValue.typed(100))
    },
    test("Should support appending multiple bindings with '++=' operator") {
      val sut     = SymbolTable.empty
      val nVar    = morphir.runtime.Symbol.variable("n")
      val flagVar = morphir.runtime.Symbol.variable("flag")
      val strVar  = morphir.runtime.Symbol.variable("str")
      val actual  = sut ++= Seq(
        nVar    := 42,
        flagVar := false,
        strVar  := "Hello"
      )

      assertTrue(
        actual(nVar) == SymbolValue.typed(42),
        actual(flagVar) == SymbolValue.typed(false),
        actual(strVar) == SymbolValue.typed("Hello"),
        actual.size == 3
      )
    },
    test("Should support appending a single binding with the '+' operator") {
      val sut     = SymbolTable.empty
      val testVar = morphir.runtime.Symbol.variable("test")
      val actual  = sut + (testVar := 3.14)
      assertTrue(actual(testVar) == SymbolValue.typed(3.14))
    },
    test("Should support 'update' and '++=' as equivalent operations") {
      val sut     = SymbolTable.empty
      val nVar    = morphir.runtime.Symbol.variable("n")
      val flagVar = morphir.runtime.Symbol.variable("flag")
      val strVar  = morphir.runtime.Symbol.variable("str")
      val actual1 = sut ++= Seq(
        nVar    := 42,
        flagVar := false,
        strVar  := "Hello"
      )

      val actual2 = sut.update(
        nVar    := 42,
        flagVar := false,
        strVar  := "Hello"
      )

      assertTrue(actual1 == actual2, actual1.size == 3)
    },
    test("Should support getting a symbol in a safe manner") {
      val boundVar   = morphir.runtime.Symbol.variable("bound")
      val unboundVar = morphir.runtime.Symbol.variable("unbound")
      val sut        = SymbolTable.create(
        boundVar := "This is bound!"
      )
      assertTrue(
        sut.get(boundVar) == Some(SymbolValue.typed("This is bound!")),
        sut.get(unboundVar) == None
      )
    }
  )
}
