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
    Decimal.abs dec


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


--{-| Test: Decimal/compare
---}
--decimalCompare : Decimal -> Decimal -> Decimal
--decimalCompare a b =
--    Decimal.compare a b


{-| Test: Decimal/div
-}
decimalDiv : Decimal -> Decimal -> Maybe Decimal
decimalDiv a b =
    div a b


{-|
Test: Decimal/divWithDefault
-}
decimalDivWithDefault: Decimal -> Decimal -> Decimal -> Decimal
decimalDivWithDefault default a b = Decimal.divWithDefault default a b


{-|
Test: Decimal/eq
-}
decimalEq: Decimal -> Decimal -> Bool
decimalEq a b = eq a b


{-|
Test: Decimal/fromInt
-}
decimalFromInt: Int -> Decimal
decimalFromInt int = fromInt int


{-|
Test: Decimal/fromString
-}
decimalFromString: String -> Maybe Decimal
decimalFromString str = fromString str


{-|
Test: Decimal/gt
-}
decimalGt: Decimal -> Decimal -> Bool
decimalGt a b = gt a b

{-|
Test: Decimal/gte
-}
decimalGte: Decimal -> Decimal -> Bool
decimalGte a b = gte a b


{-|
Test: Decimal/lt
-}
decimalLt: Decimal -> Decimal -> Bool
decimalLt a b = lt a b


{-|
Test: Decimal/lte
-}
decimalLte: Decimal -> Decimal -> Bool
decimalLte a b = lte a b


{-|
Test: Decimal/minusOne
-}
decimalMinusOne: Decimal
decimalMinusOne = fromInt -1


{-|
Test: Decimal/mul
-}
decimalMul: Decimal -> Decimal -> Decimal
decimalMul a b = mul a b


{-|
Test: Decimal/negate
-}
decimalNegate: Decimal -> Decimal
decimalNegate dec = Decimal.negate dec


{-|
Test: Decimal/neq
-}
decimalNeq: Decimal -> Decimal -> Bool
decimalNeq a b = neq a b


{-|
Test: Decimal/one
-}
decimalOne: Decimal
decimalOne = fromInt 1


{-|
Test: Decimal/round
-}
decimalRound: Decimal -> Decimal
decimalRound dec = Decimal.round dec


{-|
Test: Decimal/sub
-}
decimalSub: Decimal -> Decimal -> Decimal
decimalSub a b = sub a b


{-|
Test: Decimal/truncate
-}
decimalTruncate: Decimal -> Decimal
decimalTruncate dec = Decimal.truncate dec


{-|
Test: Decimal/zero
-}
decimalZero: Decimal
decimalZero = fromInt 0
