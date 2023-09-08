module Morphir.Examples.App.LiteralTests exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)
{-
    Note that "DecimalLiteral" appears in the IR definition, but not these tests.
    I cannot find anything that compiles to such
    It seems to be partially supported at best, even in Elm
-}

--Test: Literal/String
litStringTest : TestContext ->String
litStringTest ctx = test ctx
    let
        value : String
        value = "Bloop"
    in
        value
--expected = "Bloop"

--Test: Literal/Float
litFloatTest : TestContext ->Float
litFloatTest ctx = test ctx 
    let
        value : Float
        value = 5.0
    in
        value
--expected = 5.0

--Test: Literal/Char
litCharTest : TestContext ->Char
litCharTest ctx = test ctx 
    let
        value : Char
        value = 'f'
    in
        value
--expected = 'f'

--Test: Literal/Bool
litBoolTest : TestContext ->Bool
litBoolTest ctx = test ctx 
    let
        value : Bool
        value = True
    in
        value
--expected = True

--Test: Literal/WholeNumber
litWholeNumberLiteralTest : TestContext ->Int
litWholeNumberLiteralTest ctx = test ctx 
    let
        value : Int
        value = 5
    in
        value
--expected = 5