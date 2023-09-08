module Morphir.Examples.App.StringTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.String exposing (String, left, right, fromInt, toInt, fromFloat, isEmpty, append)
import Morphir.SDK.String as String

stringAppend : String -> String -> String
stringAppend l r = append l r

--Test: String/left
stringLeftTest : TestContext ->String
stringLeftTest ctx = test ctx
    left 2 "Mulder"
--expected = "Mu"

--Test: String/right
stringRightTest : TestContext ->String
stringRightTest ctx = test ctx
    right 2 "Scully"
--expected = "ly"

--Test: String/fromInt
stringFromIntTest : TestContext ->String
stringFromIntTest ctx = test ctx
    fromInt 25
--expected = "25"

--Test: String/fromFloat
stringFromFloatTest : TestContext ->String
stringFromFloatTest ctx = test ctx
    fromFloat 1.5
--expected = "1.5"

--Test: String/toInt
stringToIntTest1 : TestContext ->Maybe Int
stringToIntTest1 ctx = test ctx
    toInt "25"
--expected = Just 25

--Test: String/toInt
stringToIntTest2 : TestContext ->Maybe Int
stringToIntTest2 ctx = test ctx
    toInt "notAnInt"
--expected = Nothing

--Test: String/isEmpty
stringIsEmptyTest1 : TestContext ->Bool
stringIsEmptyTest1 ctx = test ctx
    isEmpty ""
--expected = True

--Test: String/isEmpty
stringIsEmptyTest2 : TestContext ->Bool
stringIsEmptyTest2 ctx = test ctx
    isEmpty "content"
--expected = False