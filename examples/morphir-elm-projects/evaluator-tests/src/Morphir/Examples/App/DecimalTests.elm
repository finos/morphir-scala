module Morphir.Examples.App.DecimalTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Decimal as Decimal exposing (Decimal, fromFloat, toString)


{-| Test: Decimal/fromFloat
expected = 1.2
-}
decimalFromFloatTest : TestContext -> Decimal
decimalFromFloatTest ctx =
    test ctx
        (fromFloat 1.2)


{-| Test: Decimal/toFloat
expected = 1.5
-}
decimalToFloatTest : TestContext -> Float
decimalToFloatTest ctx =
    test ctx
        (Decimal.toFloat (fromFloat 1.5))


{-| Test: Decimal/toString
expected = "1.2"
-}
decimalToStringTest : TestContext -> String
decimalToStringTest ctx =
    test ctx
        (toString (fromFloat 1.2))
