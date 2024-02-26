module Morphir.Examples.App.CharTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Char as Char exposing (..)


{-|
    Test: Char/isUpper
    expected('A') = True
    expected('w') = False
    expected('1') = False
    expected('Σ') = False
-}
charIsUpperTest : Char -> Bool
charIsUpperTest ch =
    isUpper ch


{-|
    Test: Char/isLower
    expected('w') = True
    expected('A') = False
    expected('0') = False
    expected('π') = False
-}
charIsLowerTest: Char -> Bool
charIsLowerTest ch =
    isLower ch


{-|
    Test: Char/isAlpha
    expected('z') = True
    expected('A') = True
    expected('1') = False
    expected('π') = False
-}
charIsAlphaTest: Char -> Bool
charIsAlphaTest ch = isAlpha ch


{-|
    Test: Char/isAlphaNum
    expected('z') = True
    expected('A') = False
    expected('1') = True
    expected('π') = False
-}
charIsAlphaNumTest: Char -> Bool
charIsAlphaNumTest ch = isAlphaNum ch


{-|
    Test: Char/isDigit
    expected('1') = True
    expected('A') = False
    expected('π') = False
-}
charIsDigitTest: Char -> Bool
charIsDigitTest ch = isDigit ch


{-|
    Test: Char/isOctDigit
    expected('1') = True
    expected('8') = False
    expected('A') = False
    expected('π') = False
-}
charIsOctDigitTest: Char -> Bool
charIsOctDigitTest ch = isOctDigit ch

{-|
    Test: Char/isHexDigit
    expected('1') = True
    expected('A') = True
    expected('f') = True
    expected('g') = False
    expected('π') = False
-}
charIsHexDigitTest: Char -> Bool
charIsHexDigitTest ch = isHexDigit ch

{-|
    Test: Char/toUpper
    expected('z') = 'Z'
-}
charToUpperTest: Char -> Char
charToUpperTest ch = toUpper ch


{-|
    Test: Char/toLower
    expected('Z') = 'z'
-}
charToLowerTest: Char -> Char
charToLowerTest ch = toLower ch


{-|
    Test: Char/toLocaleUpper
    expected('z') = 'Z'
-}
charToLocaleUpperTest: Char -> Char
charToLocaleUpperTest ch = toLocaleUpper ch


{-|
    Test: Char/toLocaleLower
    expected('Z') = 'z'
-}
charToLocaleLowerTest: Char -> Char
charToLocaleLowerTest ch = toLocaleLower ch


{-|
    Test: Char/toCode
    expected('A') = 65
    expected('B') = 66
    expected('木') = 0x6728

-}
charToCodeTest: Char -> Int
charToCodeTest ch = toCode ch


{-|
    Test: Char/fromCode
    expected(65) = 'A'
    expected(66) = 'B'
    expected(0x6728) = '木'
    expected(-1) = '�'
-}
charFromCodeTest: Int -> Char
charFromCodeTest int = fromCode int
