module Conformance.Patterns exposing (..)

{-| Every pattern form the CST models.
-}


anything : Int -> Int
anything _ =
    0


literalPatterns : Int -> String
literalPatterns n =
    case n of
        0 ->
            "zero"

        1 ->
            "one"

        _ ->
            "many"


floatPattern : Float -> String
floatPattern f =
    case f of
        3.14 ->
            "pi"

        _ ->
            "other"


textPatterns : String -> String
textPatterns s =
    case s of
        "" ->
            "empty"

        _ ->
            s


charPatterns : Char -> Bool
charPatterns c =
    case c of
        'a' ->
            True

        _ ->
            False


unitPattern : () -> Int
unitPattern () =
    0


constructorPatterns : Maybe (Result String Int) -> Int
constructorPatterns value =
    case value of
        Just (Ok n) ->
            n

        Just (Err _) ->
            -1

        Nothing ->
            0


tuplePatterns : ( Int, Int ) -> Int
tuplePatterns ( x, y ) =
    x + y


listPatterns : List Int -> Int
listPatterns xs =
    case xs of
        [] ->
            0

        [ only ] ->
            only

        first :: rest ->
            first + listPatterns rest


aliasPatterns : List Int -> List Int
aliasPatterns xs =
    case xs of
        (first :: _) as whole ->
            first :: whole

        [] ->
            []


recordPatterns : { name : String, age : Int } -> String
recordPatterns { name } =
    name
