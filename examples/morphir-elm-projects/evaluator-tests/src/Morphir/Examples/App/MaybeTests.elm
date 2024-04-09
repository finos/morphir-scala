module Morphir.Examples.App.MaybeTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)


{-| Test: Maybe/JustInt
expected = Just 1
-}
returnJustIntTest : TestContext -> Maybe Int
returnJustIntTest ctx =
    test ctx
        Just
        1


{-| Test: Maybe/JustString
expected = Just "Hello"
-}
returnJustStringTest : TestContext -> Maybe String
returnJustStringTest ctx =
    test ctx
        (Just "Hello")


{-| Test: Maybe/NoneInt
expected = Nothing
-}
returnNoneIntTest : TestContext -> Maybe Int
returnNoneIntTest ctx =
    test ctx
        Nothing


{-| Test: Maybe/Input
expected(Just "Red") = "Found Red"
expected(Just other) = other
expected(Nothing) = "Octarine"
-}
matchInput : Maybe String -> String
matchInput input =
    case input of
        Just "Red" ->
            "Found Red"

        Just other ->
            other

        Nothing ->
            "Octarine"


{-| Test: Maybe/Map
Desc: Tests native function Maybe.map
expected(Just "Red") = "Bright Red"
expected(Nothing) = "Ultraviolet"
-}
maybeMap : Maybe String -> String
maybeMap input =
    case Maybe.map (\x -> "Bright " ++ x) input of
        Just x ->
            x

        Nothing ->
            "Ultraviolet"


{-| Test: Maybe/Map
Desc: Tests native function Maybe.map, with additional check for higher-order sensitivity
expected(Just "Red") = Just "Dark Red"
expected(Nothing) = "Nothing"
-}
maybeMapWithContext : Maybe String -> Maybe String
maybeMapWithContext input =
    let
        f color =
            let
                x =
                    "Dark "
            in
            x ++ color
    in
    let
        _ =
            "Bright"
    in
    Maybe.map f input


{-| Test: Maybe/WithDefault
Description: Tests native function Maybe.withDefault
expected(Just True) = True
expected(Nothing) = False
-}
maybeWithDefault : Maybe Bool -> Bool
maybeWithDefault input =
    Maybe.withDefault False input


{-| Test: Maybe/andThen
Description: Tests native function Maybe.andThen
expected(1) = Just 1.0
expected(0) = Nothing
expected(N) = Nothing
-}
maybeAndThen : Int -> Maybe Float
maybeAndThen input =
    let
        validateIsZeroOrOne : Int -> Maybe Int
        validateIsZeroOrOne x =
            case x of
                0 ->
                    Just 0

                1 ->
                    Just 1

                _ ->
                    Nothing

        inverse : Int -> Maybe Float
        inverse x =
            if x == 0 then
                Nothing

            else
                Just (1 / toFloat x)
    in
    validateIsZeroOrOne input
        |> Maybe.andThen inverse


{-| Test: Maybe/Map2
Description: Tests native function Maybe.map2
expected(Just 1 Just 2) = Just 3
expected(Just 1 Nothing) = Nothing
expected(Nothing Nothing) = Nothing
-}
maybeMap2TestInt : Maybe Int -> Maybe Int -> Maybe Int
maybeMap2TestInt input1 input2 =
    Maybe.map2 (\x y -> x + y) input1 input2


{-| Test: Maybe/Map2
Description: Tests native function Maybe.map2
expected(Just "Hello" Just "World") = "Hello World"
expected(Just "Hello" Nothing) = "Error"
expected(Nothing Nothing) = "Error"
-}
maybeMap2TestString : Maybe String -> Maybe String -> String
maybeMap2TestString input1 input2 =
    case Maybe.map2 (\x y -> x ++ y) input1 input2 of
        Just x ->
            x

        Nothing ->
            "Error"


{-| Test: Maybe/Map3
Description: Tests native function Maybe.map3
expected(Just 1 Just 2 Just 3) = Just 6
expected(Just 1 Just 2 Nothing) = Nothing
expected(Just 1 Nothing Just 3) = Nothing
expected(Nothing Just 2 Just 3) = Nothing
expected(Nothing Nothing Nothing) = Nothing
-}
maybeMap3TestInt : Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
maybeMap3TestInt input1 input2 input3 =
    Maybe.map3 (\x y z -> x + y + z) input1 input2 input3


{-| Test: Maybe/Map3
Description: Tests native function Maybe.map3
expected(Just "2" Just "3" Just "4") = Just "234"
expected(Just "2" Nothing Just "4") = Just "24"
expected(Just "2" Just "3" Nothing) = Just "23"
expected(Nothing Just "3" Just "4") = Just "34"
expected(Nothing Nothing Nothing) = Just "1"
-}
maybeMap3TestString : Maybe String -> Maybe String -> Maybe String -> String
maybeMap3TestString input1 input2 input3 =
    case Maybe.map3 (\x y z -> x ++ y ++ z) input1 input2 input3 of
        Just x ->
            x

        Nothing ->
            "Error"


{-| Test: Maybe/Map4
Description: Tests native function Maybe.map4
expected(Just 1 Just 2 Just 3 Just 4) = Just 10
expected(Just 1 Just 2 Just 3 Nothing) = Nothing
expected(Just 1 Just 2 Nothing Just 4) = Nothing
expected(Just 1 Nothing Just 3 Just 4) = Nothing
expected(Nothing Just 2 Just 3 Just 4) = Nothing
expected(Nothing Nothing Nothing Nothing) = Nothing
-}
maybeMap4TestInt : Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
maybeMap4TestInt input1 input2 input3 input4 =
    Maybe.map4 (\x y z w -> x + y + z + w) input1 input2 input3 input4


{-| Test: Maybe/Map4
Description: Tests native function Maybe.map4
expected(Just "2" Just "3" Just "4" Just "5") = Just 2345
expected(Just "2" Just "3" Just "4" Nothing) = Just 234
expected(Just "2" Just "3" Nothing Just "5") = Just 23
expected(Just "2" Nothing Just "4" Just "5") = Just 2
expected(Nothing Just "3" Just "4" Just "5") = Just 345
expected(Nothing Nothing Nothing Nothing) = Just 1
-}
maybeMap4TestString : Maybe String -> Maybe String -> Maybe String -> Maybe String -> String
maybeMap4TestString input1 input2 input3 input4 =
    case Maybe.map4 (\x y z w -> x ++ y ++ z ++ w) input1 input2 input3 input4 of
        Just x ->
            x

        Nothing ->
            "Error"


{-| Test: Maybe/HasValue
Description: Tests native function Maybe.hasValue
expected(Just 1) = True
expected(Nothing) = False
-}
maybeHasValueTest : Maybe Int -> Bool
maybeHasValueTest input =
    Maybe.hasValue input
