module Morphir.Examples.App.CharTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Char as Char exposing (..)


{-|
    Test: Char/isUpper
    expected('A') = True
    expected('w') = False
-}
charIsUpper : Char -> Bool
charIsUpper ch =
    isUpper ch


{-|
    Test: Char/isLower
    expected('A') = False
    expected('w') = True
-}
charIsLower : Char -> Bool
charIsLower ch =
    isLower ch
