module Morphir.Examples.App.StringTests exposing (..)
import String exposing (append)
import Morphir.SDK.String exposing (..)

stringAppend : String -> String -> String
stringAppend l r = append l r

--Test: String/left
stringLeftTest : () -> String
stringLeftTest _ =
    left 2 "Mulder"
--expected = "Mu"