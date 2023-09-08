module Morphir.Examples.App.DecimalTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Decimal exposing (Decimal, fromFloat, toString)
import Morphir.SDK.Decimal as Decimal

--Test: Decimal/fromFloat
decimalFromFloatTest : TestContext ->Decimal
decimalFromFloatTest ctx = test ctx
    (fromFloat 1.2)
--expected = 1.2

--Test: Decimal/toFloat
decimalToFloatTest : TestContext ->Float
decimalToFloatTest ctx = test ctx
    (Decimal.toFloat (fromFloat 1.5))
--expected = 1.5

--Test: Decimal/toString
decimalToStringTest : TestContext ->String
decimalToStringTest ctx = test ctx
    (toString (fromFloat 1.2))
--expected = "1.2"