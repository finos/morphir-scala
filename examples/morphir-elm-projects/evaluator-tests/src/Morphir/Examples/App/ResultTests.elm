module Morphir.Examples.App.ResultTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)


{-| Test: Result/Return
expected(0) = Ok 0
expected(1) = Err "Negative"
-}
returnResultType : Int -> Result String Int
returnResultType x =
    if x == 0 then
        Ok x

    else
        Err "Negative"


{-| Test: Result/Resolve
expected(Ok 0) = 0
expected(Error False) = 0
expected(Error True) = 1
-}
resolveResultType : Result Bool Int -> Int
resolveResultType res =
    case res of
        Err False ->
            0

        Err True ->
            1

        Ok x ->
            x


{-| Test: Result/Map
Desc: Tests native function Result.map
expected(Ok "Red") = "Bright Red"
expected(Err "Missing") = "Error: Missing"
-}
resultMap : Result String String -> String
resultMap input =
    case Result.map (\x -> "Bright " ++ x) input of
        Ok ok ->
            ok

        Err err ->
            "Error: " ++ err


{-| Test: Result/mapError
Desc: Tests native function Result.mapError
expected(Ok "Energy") = "Fine: Energy"
expected(Err "Matter") = "Anti-Matter"
-}
resultMapError : Result String String -> String
resultMapError input =
    case Result.mapError (\x -> "Anti-" ++ x) input of
        Ok ok ->
            "Fine: " ++ ok

        Err err ->
            err


{-| Test: Result/Map
Desc: Tests native function Result.map, with additional check for higher-order sensitivity
expected(Ok "Red") = Just "Dark Red"
expected(Err "Missing") = "Missing"
-}
resultMapWithContext : Result x String -> Result x String
resultMapWithContext input =
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
    Result.map f input


{-| Test: Result/WithDefault
Description: Tests native function Result.withDefault
expected(Ok True) = True
expected(Err True) = False
-}
resultWithDefault : Result x Bool -> Bool
resultWithDefault input =
    Result.withDefault False input


{-| Test: Result/ToMaybe
Description: Tests native function Result.toMaybe
expected(Ok Red) = Just Red
expected(Err X) = Nothing
-}
resultToMaybe : Result err ok -> Maybe ok
resultToMaybe input =
    Result.toMaybe input


{-| Test: Result/FromMaybe
Description: Tests native function Result.fromMaybe
expected(Just 3) = Ok 3
expected(Nothing) = Err Undefined
-}
resultFromMaybe : Maybe Int -> Result String Int
resultFromMaybe input =
    Result.fromMaybe "Undefined" input


{-| Test: Result/andThen
Description: Tests native function Result.andThen
expected(1) = Ok 1.0
expected(0) = Err "undefined"
expected(N) = Err "invalid"
-}
resultAndThen : Int -> Result String Float
resultAndThen input =
    let
        validateIsZeroOrOne : Int -> Result String Int
        validateIsZeroOrOne x =
            case x of
                0 ->
                    Ok 0

                1 ->
                    Ok 1

                _ ->
                    Err "invalid"

        inverse : Int -> Result String Float
        inverse x =
            if x == 0 then
                Err "undefined"

            else
                Ok (1 / toFloat x)
    in
    validateIsZeroOrOne input
        |> Result.andThen inverse
