module Morphir.Examples.App.StringTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.String exposing (String, left, right, fromInt, toInt, fromFloat, isEmpty, append)
import Morphir.SDK.String as String

stringAppend : String -> String -> String
stringAppend l r = append l r

{-|
    Test: String/left
    expected = "Mu"
-}
stringLeftTest : TestContext ->String
stringLeftTest ctx = test ctx
    left 2 "Mulder"

{-|
    Test: String/right
    expected = "ly"
-}
stringRightTest : TestContext ->String
stringRightTest ctx = test ctx
    right 2 "Scully"

{-|
    Test: String/fromInt
    expected = "25"
-}
stringFromIntTest : TestContext ->String
stringFromIntTest ctx = test ctx
    fromInt 25

{-|
    Test: String/fromFloat
    expected = "1.5"
-}
stringFromFloatTest : TestContext ->String
stringFromFloatTest ctx = test ctx
    fromFloat 1.5

{-|
    Test: String/toInt
    expected = Just 25
-}
stringToIntTest1 : TestContext ->Maybe Int
stringToIntTest1 ctx = test ctx
    toInt "25"

{-|
    Test: String/toInt - Invalid
    expected = Nothing
-}
stringToIntTest2 : TestContext ->Maybe Int
stringToIntTest2 ctx = test ctx
    toInt "notAnInt"

{-|
    Test: String/isEmpty - True
    expected = True
-}
stringIsEmptyTest1 : TestContext ->Bool
stringIsEmptyTest1 ctx = test ctx
    isEmpty ""

{-|
    Test: String/isEmpty - False
    expected = False
-}
stringIsEmptyTest2 : TestContext ->Bool
stringIsEmptyTest2 ctx = test ctx
    isEmpty "content"