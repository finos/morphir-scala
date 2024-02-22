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
charIsLower: Char -> Bool
charIsLower ch =
    isLower ch


{-|
    Test: Char/isAlpha
-}
isAlpha: Char -> Bool
isAlpha ch = isAlpha ch


{-|
    Test: Char/isAlphaNum
-}
isAlphaNum: Char -> Bool
isAlphaNum ch = isAlphaNum ch


{-|
    Test: Char/isDigit
-}
isDigit: Char -> Bool
isDigit ch = isDigit ch


{-|
    Test: Char/isOctDigit
-}
isOctDigit: Char -> Bool
isOctDigit ch = isOctDigit ch

{-|
    Test: Char/isHexDigit
-}
isHexDigit: Char -> Bool
isHexDigit ch = isHexDigit ch

{-|
    Test: Char/toUpper
-}
toUpper: Char -> Char
toUpper ch = toUpper ch


{-|
    Test: Char/toLower
-}
toLower: Char -> Char
toLower ch = toLower ch


{-|
    Test: Char/toLocaleUpper
-}
toLocaleUpper: Char -> Char
toLocaleUpper ch = toLocaleUpper ch


{-|
    Test: Char/toLocaleLower
-}
toLocaleLower: Char -> Char
toLocaleLower ch = toLocaleLower ch


{-|
    Test: Char/toCode
-}
toCode: Char -> Int
toCode ch = toCode ch


{-|
    Test: Char/fromCode
-}
fromCode: Int -> Char
fromCode int = fromCode int
