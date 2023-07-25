module Morphir.Examples.App.OptionTests exposing (..)


{-
-}

returnJustIntTest : () -> Maybe Int
returnJustIntTest _ = 
    Just 1

returnNoneIntTest : () -> Maybe Int
returnNoneIntTest _ =
    Nothing