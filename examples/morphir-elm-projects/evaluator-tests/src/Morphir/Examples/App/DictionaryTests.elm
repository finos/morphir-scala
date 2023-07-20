module Morphir.Examples.App.DictionaryTests exposing (..)

import Dict exposing (Dict)

{-
-}

returnDictionaryTest : () -> Dict Int String
returnDictionaryTest _ = 
    Dict.fromList [(1, "Red"), (2, "Blue")]