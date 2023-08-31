module Morphir.Examples.App.DictionaryTests exposing (..)

import Dict exposing (Dict)

{-
-}

dictFilterTest: () -> Dict Int String
dictFilterTest _ =
    let
        dict = Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Blue"), (4, "Blue"), (5, "Green")]
    in
        Dict.filter (\k v -> k > 2 && v == "Blue") dict

--Test: Dict/fromList
dictFromListTest : () -> Dict Int String
dictFromListTest _ =
    Dict.fromList [(1, "Red"), (2, "Blue"), (3, "Orange"), (4, "White"), (5, "Green")]

--Test: Dict/Get
dictGetTest : () -> Maybe String
dictGetTest _ =
    let
        animals = Dict.fromList [ ("Tom", "Cat"), ("Jerry", "Mouse") ]
    in
        Dict.get "Tom" animals
--expected = Just "Cat"