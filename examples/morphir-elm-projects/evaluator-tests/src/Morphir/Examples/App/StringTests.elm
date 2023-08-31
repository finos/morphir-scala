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

--Test: String/fromInt
stringFromIntTest : () -> String
stringFromIntTest _ =
    fromInt 25
--expected = "25"

--Test: String/fromFloat
stringFromFloatTest : () -> String
stringFromFloatTest _ =
    fromFloat 1.5
--expected = "1.5"