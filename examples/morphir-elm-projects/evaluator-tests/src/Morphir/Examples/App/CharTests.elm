module Morphir.Examples.App.CharTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Char as Char exposing (..)


{-|
    Test: Char/isUpper
    expected('A') = True
    expected('w') = False
-}
charIsUpperTest : Char -> Bool
charIsUpperTest ch =
    isUpper ch


{-|
    Test: Char/isLower
    expected('A') = False
    expected('w') = True
-}
charIsLowerTest: Char -> Bool
charIsLowerTest ch =
    isLower ch


{-|
    Test: Char/isAlpha
-}
charIsAlphaTest: Char -> Bool
charIsAlphaTest ch = isAlpha ch


{-|
    Test: Char/isAlphaNum
-}
charIsAlphaNumTest: Char -> Bool
charIsAlphaNumTest ch = isAlphaNum ch


{-|
    Test: Char/isDigit
-}
charIsDigitTest: Char -> Bool
charIsDigitTest ch = isDigit ch


{-|
    Test: Char/isOctDigit
-}
charIsOctDigitTest: Char -> Bool
charIsOctDigitTest ch = isOctDigit ch

{-|
    Test: Char/isHexDigit
-}
charIsHexDigitTest: Char -> Bool
charIsHexDigitTest ch = isHexDigit ch

{-|
    Test: Char/toUpper
-}
charToUpperTest: Char -> Char
charToUpperTest ch = toUpper ch


{-|
    Test: Char/toLower
-}
charToLowerTest: Char -> Char
charToLowerTest ch = toLower ch


{-|
    Test: Char/toLocaleUpper
-}
charToLocaleUpperTest: Char -> Char
charToLocaleUpperTest ch = toLocaleUpper ch


{-|
    Test: Char/toLocaleLower
-}
charToLocaleLowerTest: Char -> Char
charToLocaleLowerTest ch = toLocaleLower ch


{-|
    Test: Char/toCode
-}
charToCodeTest: Char -> Int
charToCodeTest ch = toCode ch


{-|
    Test: Char/fromCode
-}
charFromCodeTest: Int -> Char
charFromCodeTest int = fromCode int
