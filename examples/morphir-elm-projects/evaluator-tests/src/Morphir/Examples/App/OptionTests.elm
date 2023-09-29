module Morphir.Examples.App.OptionTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

{-|
Test: Option/JustInt
expected = Just 1
-}
returnJustIntTest : TestContext ->Maybe Int
returnJustIntTest ctx = test ctx 
    Just 1

{-|
Test: Option/JustString
expected = Just "Hello"
-}
returnJustStringTest : TestContext ->Maybe String
returnJustStringTest ctx = test ctx
    (Just "Hello")

{-|
Test: Option/NoneInt
expected = Nothing
-}
returnNoneIntTest : TestContext ->Maybe Int
returnNoneIntTest ctx = test ctx
    Nothing

{-|
Test: Result/Return
expected(0) = Ok 0
expected(1) = Err "Negative"
-}
returnResultType : Int -> Result String Int
returnResultType x = if x == 0 then Ok x else Err "Negative"

{-|
Test: Result/Resolve
expected(Ok 0) = 0
expected(Error False) = 0
expected(Error True) = 1
-}
resolveResultType : Result Bool Int -> Int
resolveResultType res = case res of
    Err False -> 0
    Err True -> 1
    Ok x -> x

matchInput : Maybe String -> String
matchInput input = case input of
    Just "Red" -> "Found Red"
    Just other -> other
    Nothing -> "Let's say green"
