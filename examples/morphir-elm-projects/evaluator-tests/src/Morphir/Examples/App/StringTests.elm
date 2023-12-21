module Morphir.Examples.App.StringTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.String as String exposing (..)


{-|

    Test: String/append

-}
stringAppend : String -> String -> String
stringAppend l r =
    append l r


{-|

    Test: String/left
    expected = "Mu"

-}
stringLeftTest : TestContext -> String
stringLeftTest ctx =
    test ctx
        (left 2 "Mulder")


{-|

    Test: String/right
    expected = "ly"

-}
stringRightTest : TestContext -> String
stringRightTest ctx =
    test ctx
        (right 2 "Scully")


{-|

    Test: String/slice

-}
stringSlice : Int -> Int -> String -> String
stringSlice start end str =
    slice start end str


{-|

    Test: String/split

-}
stringSplit : String -> String -> List String
stringSplit sep str =
    split sep str


{-|

    Test: String/startsWith

-}
stringStartsWith : String -> String -> Bool
stringStartsWith ref str =
    startsWith ref str


{-|

    Test: String/slice

-}
stringSlice : Int -> Int -> String -> String
stringSlice start end str =
    slice start end str


{-|

    Test: String/split

-}
stringSplit : String -> String -> List String
stringSplit sep str =
    split sep str


{-|

    Test: String/startsWith

-}
stringStartsWith : String -> String -> Bool
stringStartsWith ref str =
    startsWith ref str


{-|

    Test: String/fromInt
    expected = "25"

-}
stringFromIntTest : TestContext -> String
stringFromIntTest ctx =
    test ctx
        (fromInt 25)


{-|

    Test: String/fromFloat
    expected = "1.5"

-}
stringFromFloatTest : TestContext -> String
stringFromFloatTest ctx =
    test ctx
        (fromFloat 1.5)


{-|

    Test: String/toFloat
    expected = Just 1.5

-}
stringToFloatTest1 : TestContext -> Maybe Float
stringToFloatTest1 ctx =
    test ctx
        (String.toFloat "1.5")


{-|

    Test: String/toFloat - Invalid
    expected = Nothing

-}
stringToFloatTest2 : TestContext -> Maybe Float
stringToFloatTest2 ctx =
    test ctx
        (String.toFloat "not a float")


{-|

    Test: String/toInt
    expected = Just 25

-}
stringToIntTest1 : TestContext -> Maybe Int
stringToIntTest1 ctx =
    test ctx
        (toInt "25")


{-|

    Test: String/toInt - Invalid
    expected = Nothing

-}
stringToIntTest2 : TestContext -> Maybe Int
stringToIntTest2 ctx =
    test ctx
        (toInt "notAnInt")


{-|

    Test: String/isEmpty - True
    expected = True

-}
stringIsEmptyTest1 : TestContext -> Bool
stringIsEmptyTest1 ctx =
    test ctx
        (isEmpty "")


{-|

    Test: String/isEmpty - False
    expected = False

-}
stringIsEmptyTest2 : TestContext -> Bool
stringIsEmptyTest2 ctx =
    test ctx
        (isEmpty "content")
