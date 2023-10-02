module Morphir.Examples.App.MaybeTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

{-|
Test: Maybe/JustInt
expected = Just 1
-}
returnJustIntTest : TestContext ->Maybe Int
returnJustIntTest ctx = test ctx 
    Just 1

{-|
Test: Maybe/JustString
expected = Just "Hello"
-}
returnJustStringTest : TestContext ->Maybe String
returnJustStringTest ctx = test ctx
    (Just "Hello")

{-|
Test: Maybe/NoneInt
expected = Nothing
-}
returnNoneIntTest : TestContext ->Maybe Int
returnNoneIntTest ctx = test ctx
    Nothing

{-|
Test: Maybe/Input
expected(Just "Red") = "Found Red"
expected(Just other) = other
expected(Nothing) = "Octarine"
-}
matchInput : Maybe String -> String
matchInput input = case input of
    Just "Red" -> "Found Red"
    Just other -> other
    Nothing -> "Octarine"

{-|
Test: Maybe/Map
Desc: Tests native function Maybe.map
expected(Just "Red") = "Bright Red"
expected(Nothing) = "Ultraviolet"
-}
maybeMap : Maybe String -> String
maybeMap input = 
    case (Maybe.map (\x -> "Bright " ++ x) input) of
        Just x -> x
        Nothing -> "Ultraviolet"

{-|
Test: Maybe/Map
Desc: Tests native function Maybe.map, with additional check for higher-order sensitivity
expected(Just "Red") = Just "Dark Red"
expected(Nothing) = "Nothing"
-}
maybeMapWithContext : Maybe String -> Maybe String
maybeMapWithContext input = 
    let 
        f color = 
            let 
                x = "Dark " 
            in
                x ++ color
    in
        let x = "Bright" in
            Maybe.map f input

{-|
Test: Maybe/WithDefault
Description: Tests native function Maybe.withDefault
expected(Just True) = True
expected(Nothing) = False
-}
maybeWithDefault : Maybe Bool -> Bool
maybeWithDefault input = 
    Maybe.withDefault False input

