module Morphir.Examples.App.DictionaryTests exposing (..)

import Dict exposing (Dict)

{-
-}

returnDictionaryTest : () -> Dict Int String
returnDictionaryTest _ = 
    Dict.fromList [(1, "Red"), (2, "Blue")]

--Test: Dict/Get
dictGetTest : () -> Maybe String
dictGetTest _ =
    let
        animals = Dict.fromList [ ("Tom", "Cat"), ("Jerry", "Mouse") ]
    in
        Dict.get "Tom" animals
--expected = Just "Cat"