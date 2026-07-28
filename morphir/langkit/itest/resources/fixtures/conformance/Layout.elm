module Conformance.Layout exposing (..)

{-| Layout shapes that decide where a declaration or a block ends.
-}


continued : Int -> Int
continued n =
    n
        + 1
        + 2


applied : (Int -> Int) -> Int -> Int
applied f n =
    f
        n


nestedBlocks : Maybe (List Int) -> Int
nestedBlocks value =
    case value of
        Just xs ->
            let
                total =
                    List.sum xs

                count =
                    List.length xs
            in
            case count of
                0 ->
                    0

                _ ->
                    total // count

        Nothing ->
            0


afterTheBlocks : Int
afterTheBlocks =
    1
