module Morphir.Examples.App.DecimalTests exposing (..)
import Morphir.SDK.Decimal exposing (Decimal, fromFloat, toString)
import Morphir.SDK.Decimal as Decimal

--Test: Decimal/fromFloat
decimalFromFloatTest : () -> Decimal
decimalFromFloatTest _ =
    fromFloat 1.2
--expected = 1.2

--Test: Decimal/toFloat
decimalToFloatTest : () -> Float
decimalToFloatTest _ =
    Decimal.toFloat (fromFloat 1.5)
--expected = 1.5

--Test: Decimal/toString
decimalToStringTest : () -> String
decimalToStringTest _ =
    toString (fromFloat 1.2)
--expected = "1.2"