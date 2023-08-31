module Morphir.Examples.App.StringTests exposing (..)
import Morphir.SDK.String as String

stringAppend : String -> String -> String
stringAppend l r = append l r

--Test: String/left
stringLeftTest : () -> String
stringLeftTest _ =
    left 2 "Mulder"
--expected = "Mu"

--Test: String/right
stringRightTest : () -> String
stringRightTest _ =
    right 2 "Scully"
--expected = "ly"