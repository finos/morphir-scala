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

    Test: String/concat

-}
stringConcat : List String -> String
stringConcat l =
    concat l

{-|

    Test: String/repeat
-}
stringRepeat : Int ->  String -> String
stringRepeat i s =
    repeat i s

{-|

    Test: String/contains

-}
stringContains : String -> String -> Bool
stringContains substring str =
    contains substring str


{-|

    Test: String/dropLeft

-}
stringDropLeft : Int -> String -> String
stringDropLeft n str =
    dropLeft n str


{-|

    Test: String/dropRight

-}
stringDropRight : Int -> String -> String
stringDropRight n str =
    dropRight n str


{-|

    Test: String/endsWith

-}
stringEndsWith : String -> String -> Bool
stringEndsWith ref str =
    endsWith ref str


{-|

    Test: String/join

-}
stringJoin : String -> List String -> String
stringJoin sep list =
    join sep list


{-|

    Test: String/length

-}
stringLength : String -> Int
stringLength str =
    length str


{-|

    Test: String/padLeft

-}
stringPadLeft : Int -> Char -> String -> String
stringPadLeft n ch str =
    padLeft n ch str


{-|

    Test: String/padRight

-}
stringPadRight : Int -> Char -> String -> String
stringPadRight n ch str =
    padRight n ch str


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
stringGoodToFloatTest : TestContext -> Maybe Float
stringGoodToFloatTest ctx =
    test ctx
        (String.toFloat "1.5")


{-|

    Test: String/toFloat - Invalid
    expected = Nothing

-}
stringBadToFloatTest : TestContext -> Maybe Float
stringBadToFloatTest ctx =
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


{-|

    Test: String/toLower

-}
stringToLower : String -> String
stringToLower str =
    toLower str


{-|

    Test: String/toUpper

-}
stringToUpper : String -> String
stringToUpper str =
    toUpper str


{-|

    Test: String/trim

-}
stringTrim : String -> String
stringTrim str =
    trim str


{-|

    Test: String/trimLeft

-}
stringTrimLeft : String -> String
stringTrimLeft str =
    trimLeft str


{-|

    Test: String/trimRight

-}
stringTrimRight : String -> String
stringTrimRight str =
    trimRight str
