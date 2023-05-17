module Morphir.Examples.App.LiteralTests exposing (..)
{-
    Note that "DecimalLiteral" appears in the IR definition, but not these tests.
    I cannot find anything that compiles to such
    It seems to be partially supported at best, even in Elm
-}

--Test: Literal/String
litStringTest : () -> String
litStringTest _ =
    let
        value : String
        value = "Bloop"
    in
        value
--expected = "Bloop"

--Test: Literal/Float
litFloatTest : () -> Float
litFloatTest _ = 
    let
        value : Float
        value = 5.0
    in
        value
--expected = 5.0

--Test: Literal/Char
litCharTest : () -> Char
litCharTest _ = 
    let
        value : Char
        value = 'f'
    in
        value
--expected = 'f'

--Test: Literal/Bool
litBoolTest : () -> Bool
litBoolTest _ = 
    let
        value : Bool
        value = True
    in
        value
--expected = True

--Test: Literal/WholeNumber
litWholeNumberLiteralTest : () -> Int
litWholeNumberLiteralTest _ = 
    let
        value : Int
        value = 5
    in
        value
--expected = 5