module Morphir.Examples.App.OptionTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)


returnJustIntTest : TestContext ->Maybe Int
returnJustIntTest ctx = test ctx 
    Just 1

returnJustStringTest : TestContext ->Maybe String
returnJustStringTest ctx = test ctx
    Just "Hello"

returnNoneIntTest : TestContext ->Maybe Int
returnNoneIntTest ctx = test ctx
    Nothing

returnResultType : Int -> Result String Int
returnResultType x = if x == 0 then Ok x else Err "Negative"

resolveResultType : Result Bool Int -> Int
resolveResultType res = case res of
    Err False -> 0
    Err True -> 1
    Ok x -> x
