module Morphir.Examples.App.StringTests exposing (..)
import Morphir.SDK.String exposing (String, left, right, fromInt, toInt, fromFloat, isEmpty, append)
import Morphir.SDK.String as String

stringAppend : String -> String -> String
stringAppend l r = append l r

--Test: String/left
stringLeftTest : () -> String
stringLeftTest _ =
    left 2 "Mulder"
--expected = "Mu"

--Test: String/right
stringRightTest : () -> String
stringRightTest _ =
    right 2 "Scully"
--expected = "ly"

--Test: String/fromInt
stringFromIntTest : () -> String
stringFromIntTest _ =
    fromInt 25
--expected = "25"

--Test: String/fromFloat
stringFromFloatTest : () -> String
stringFromFloatTest _ =
    fromFloat 1.5
--expected = "1.5"

--Test: String/toInt
stringToIntTest1 : () -> Maybe Int
stringToIntTest1 _ =
    toInt "25"
--expected = Just 25

--Test: String/toInt
stringToIntTest2 : () -> Maybe Int
stringToIntTest2 _ =
    toInt "notAnInt"
--expected = Nothing

--Test: String/isEmpty
stringIsEmptyTest1 : () -> Bool
stringIsEmptyTest1 _ =
    isEmpty ""
--expected = True

--Test: String/isEmpty
stringIsEmptyTest2 : () -> Bool
stringIsEmptyTest2 _ =
    isEmpty "content"
--expected = False