module Morphir.Examples.App.CharTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Char as Char exposing (..)


{-|
    Test: Char/isUpper
    expected:
-}
charIsUpper : Char -> Bool
charIsUpper ch =
    isUpper ch
