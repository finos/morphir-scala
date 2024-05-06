module Morphir.Examples.App.FloatTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)

{-| Test: SdkFloat/fromInt
expected = 500.0
-}
floatFromInt : TestContext -> Float
floatFromInt ctx =
    test ctx <|
        let
            f x =
                toFloat x
        in
        f 500
