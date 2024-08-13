module Morphir.Examples.App.LiteralTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)



{-
   Note that "DecimalLiteral" appears in the IR definition, but not these tests.
   I cannot find anything that compiles to such
   It seems to be partially supported at best, even in Elm
-}


{-| Test: Literal/String
expected = "Bloop"
-}
litStringTest : TestContext -> String
litStringTest ctx =
    test ctx <|
        let
            value : String
            value =
                "Bloop"
        in
        value


{-| Test: Literal/Float
expected = 5.0
-}
litFloatTest : TestContext -> Float
litFloatTest ctx =
    test ctx <|
        let
            value : Float
            value =
                5.0
        in
        value


{-| Test: Literal/Char
expected = 'f'
-}
litCharTest : TestContext -> Char
litCharTest ctx =
    test ctx <|
        let
            value : Char
            value =
                'f'
        in
        value


{-| Test: Literal/Bool
expected = True
-}
litBoolTest : TestContext -> Bool
litBoolTest ctx =
    test ctx <|
        let
            value : Bool
            value =
                True
        in
        value


{-| Test: Literal/WholeNumber
expected = 5
-}
litWholeNumberLiteralTest : TestContext -> Int
litWholeNumberLiteralTest ctx =
    test ctx <|
        let
            value : Int
            value =
                5
        in
        value
