module Morphir.Examples.App.DecimalTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Decimal as Decimal exposing (..)


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


{-| Test: Decimal/abs
-}
decimalAbs : Decimal -> Decimal
decimalAbs dec =
    abs dec


{-| Test: Decimal/add
-}
decimalAdd : Decimal -> Decimal -> Decimal
decimalAdd dec1 dec2 =
    add dec1 dec2


{-| Test: Decimal/bps
-}
decimalBps : Int -> Decimal
decimalBps int =
    bps int


{-| Test: Decimal/compare
-}
decimalCompare : Decimal -> Decimal -> Decimal
decimalCompare a b =
    compare a b


{-| Test: Decimal/div
-}
decimalDiv : Decimal -> Decimal -> Maybe Decimal
decimalDiv a b =
    div a b
