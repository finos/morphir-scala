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



-- {-| Test: Decimal/toFloat
-- expected = 1.5
-- -}
-- decimalToFloatTest : TestContext -> Float
-- decimalToFloatTest ctx =
--     test ctx
--         (Decimal.toFloat (fromFloat 1.5))


{-| Test: Decimal/toString
expected = "-1.0"
-}
decimalToStringTest : TestContext -> String
decimalToStringTest ctx =
    test ctx
        (toString decimalMinusOne)


{-| Test: Decimal/abs
expected = 1.0
-}
decimalPositiveAbs : TestContext -> Decimal
decimalPositiveAbs ctx =
    test ctx
        Decimal.abs
        decimalOne


{-| Test: Decimal/abs
expected = 100.243
-}
decimalNegativeAbs : TestContext -> Decimal
decimalNegativeAbs ctx =
    test ctx
        Decimal.abs
        (fromFloat -100.243)


{-| Test: Decimal/add
expected = 673.45
-}
decimalAdd : TestContext -> Decimal
decimalAdd ctx =
    test ctx
        add
        (fromFloat -100)
        (fromFloat 773.45)


{-| Test: Decimal/bps
expected = 0.0463
-}
decimalBps : TestContext -> Decimal
decimalBps ctx =
    test ctx
        bps
        463



--{-| Test: Decimal/compare
---}
--decimalCompare : Decimal -> Decimal -> Decimal
--decimalCompare a b =
--    Decimal.compare a b


{-| Test: Decimal/div
expected Just 1.8
-}
decimalGoodDiv : TestContext -> Maybe Decimal
decimalGoodDiv ctx =
    test ctx
        div
        (fromFloat 4.5)
        (fromFloat 2.5)


{-| Test: Decimal/div
expected None
-}
decimalBadDiv : TestContext -> Maybe Decimal
decimalBadDiv ctx =
    test ctx
        div
        (fromFloat 4.5)
        decimalZero


{-| Test: Decimal/divWithDefault
expected = -7
-}
decimalDivWithDefault : TestContext -> Decimal
decimalDivWithDefault ctx =
    test ctx
        Decimal.divWithDefault
        (fromFloat 0)
        (fromFloat 777.7)
        (fromFloat -111.1)


{-| Test: Decimal/divWithDefault
expected = 0
-}
decimalZeroDivWithDefault : TestContext -> Decimal
decimalZeroDivWithDefault ctx =
    test ctx
        Decimal.divWithDefault
        (fromFloat 0)
        (fromFloat 777.7)
        (fromFloat 0)


{-| Test: Decimal/eq
expected True
-}
decimalTrueEq : TestContext -> Bool
decimalTrueEq ctx =
    test ctx
        eq
        (fromFloat 3.463)
        (fromFloat 3.463)


{-| Test: Decimal/eq
expected False
-}
decimalFalseEq : TestContext -> Bool
decimalFalseEq ctx =
    test ctx
        eq
        (fromFloat 634.3)
        (fromFloat -634.3)


{-| Test: Decimal/fromInt
-}
decimalFromInt : Int -> Decimal
decimalFromInt int =
    fromInt int


{-| Test: Decimal/fromString
-}
decimalFromString : String -> Maybe Decimal
decimalFromString str =
    fromString str


{-| Test: Decimal/gt
expected = true
-}
decimalTrueGt : TestContext -> Bool
decimalTrueGt ctx =
    test ctx
        gt
        (fromFloat 14.6)
        (fromFloat 7.23)


{-| Test: Decimal/gt
expected = false
-}
decimalFalseGt : TestContext -> Bool
decimalFalseGt ctx =
    test ctx
        gt
        (fromFloat 7.23)
        (fromFloat 14.6)


{-| Test: Decimal/gte
expected = true
-}
decimalTrueGte : TestContext -> Bool
decimalTrueGte ctx =
    test ctx
        gte
        (fromFloat 84.5)
        (fromFloat 14.6)


{-| Test: Decimal/gte
expected = true
-}
decimalTrueEqualGte : TestContext -> Bool
decimalTrueEqualGte ctx =
    test ctx
        gte
        (fromFloat 14.6)
        (fromFloat 14.6)


{-| Test: Decimal/gte
expected = false
-}
decimalFalseGte : TestContext -> Bool
decimalFalseGte ctx =
    test ctx
        gte
        (fromFloat 14.6)
        (fromFloat 100.3)


{-| Test: Decimal/lt
expected = true
-}
decimalTrueLt : TestContext -> Bool
decimalTrueLt ctx =
    test ctx
        lt
        (fromFloat -2.6)
        (fromFloat 7.23)


{-| Test: Decimal/lt
expected = false
-}
decimalFalseLt : TestContext -> Bool
decimalFalseLt ctx =
    test ctx
        lt
        (fromFloat 7.23)
        (fromFloat -14.6)


{-| Test: Decimal/lte
expected = true
-}
decimalTrueLte : TestContext -> Bool
decimalTrueLte ctx =
    test ctx
        lte
        (fromFloat -11184.5)
        (fromFloat 14.6)


{-| Test: Decimal/lte
expected = true
-}
decimalTrueEqualLte : TestContext -> Bool
decimalTrueEqualLte ctx =
    test ctx
        lte
        (fromFloat -14.6)
        (fromFloat -14.6)


{-| Test: Decimal/lte
expected = false
-}
decimalFalseLte : TestContext -> Bool
decimalFalseLte ctx =
    test ctx
        lte
        (fromFloat 100.3)
        (fromFloat 14.6)


{-| Test: Decimal/minusOne
-}
decimalMinusOne : Decimal
decimalMinusOne =
    fromInt -1


{-| Test: Decimal/mul
expected = 0.06927
-}
decimalMul : TestContext -> Decimal
decimalMul ctx =
    test ctx
        Decimal.mul
        (fromFloat 23.09)
        (fromFloat 0.003)


{-| Test: Decimal/negate
expected = 34.222
-}
decimalNegate : TestContext -> Decimal
decimalNegate ctx =
    test ctx
        Decimal.negate
        (fromFloat -34.222)


{-| Test: Decimal/neq
expected = true
-}
decimalTrueNeq : TestContext -> Bool
decimalTrueNeq ctx =
    test ctx
        neq
        (fromFloat 3.33)
        (fromFloat -3.33)


{-| Test: Decimal/neq
expected = false
-}
decimalFalseNeq : TestContext -> Bool
decimalFalseNeq ctx =
    test ctx
        neq
        (fromFloat 3.33)
        (fromFloat 3.33)


{-| Test: Decimal/one
-}
decimalOne : Decimal
decimalOne =
    fromInt 1


{-| Test: Decimal/round
expected = 322.0
-}
decimalWholeRound : TestContext -> Decimal
decimalWholeRound ctx =
    test ctx
        Decimal.round
        (fromFloat 322.14)


{-| Test: Decimal/round
expected = -92.0
-}
decimalNegativeRound : TestContext -> Decimal
decimalNegativeRound ctx =
    test ctx
        Decimal.round
        (fromFloat -91.51)


{-| Test: Decimal/sub
expected = 1.1
-}
decimalSub : TestContext -> Decimal
decimalSub ctx =
    test ctx
        sub
        (fromFloat 4.2)
        (fromFloat 3.1)


{-| Test: Decimal/truncate
expected = 1.0
-}
decimalTruncate : TestContext -> Decimal
decimalTruncate ctx =
    test ctx
        Decimal.truncate
        (fromFloat 1.33333333333333)


{-| Test: Decimal/zero
-}
decimalZero : Decimal
decimalZero =
    fromInt 0


{-| Test: Decimal/shiftDecimalLeft
shift = 2
value = 123.45
expected = 1.2345
-}
decimalShiftLeft : Int -> Decimal -> Decimal
decimalShiftLeft shift dec =
    Decimal.shiftDecimalLeft
        shift
        dec


{-| Test: Decimal/shiftDecimalRight
shift = 2
value = 1.2345
expected = 123.45
-}
decimalShiftRight : Int -> Decimal -> Decimal
decimalShiftRight shift dec =
    Decimal.shiftDecimalRight
        shift
        dec


{-| Test: Decimal/hundred
value = 123
expected = 12300
-}
decimalHundred : Int -> Decimal
decimalHundred value =
    Decimal.hundred
        value


{-| Test: Decimal/hundredth
value = 123
expected = 1.23
-}
decimalHundredth : Int -> Decimal
decimalHundredth value =
    Decimal.hundredth
        value


{-| Test: Decimal/million
value = 123
expected = 123000000
-}
decimalMillion : Int -> Decimal
decimalMillion value =
    Decimal.million
        value


{-| Test: Decimal/millionth
value = 123
expected = 0.000123
-}
decimalMillionth : Int -> Decimal
decimalMillionth value =
    Decimal.millionth
        value


{-| Test: Decimal/tenth
value = 123
expected = 12.3
-}
decimalTenth : Int -> Decimal
decimalTenth value =
    Decimal.tenth
        value


{-| Test: Decimal/thousand
value = 123
expected = 123000
-}
decimalThousand : Int -> Decimal
decimalThousand value =
    Decimal.thousand
        value


{-| Test: Decimal/thousandth
value = 123
expected = 0.123
-}
decimalThousandth : Int -> Decimal
decimalThousandth value =
    Decimal.thousandth
        value
