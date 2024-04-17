module Morphir.Examples.App.StringTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)


{-|

    Test: String/append

-}
stringAppend : String -> String -> String
stringAppend l r =
    String.append l r


{-|

    Test: String/concat

-}
stringConcat : List String -> String
stringConcat l =
    String.concat l


{-|

    Test: String/repeat

-}
stringRepeat : Int -> String -> String
stringRepeat i s =
    String.repeat i s


{-|

    Test: String/contains

-}
stringContains : String -> String -> Bool
stringContains substring str =
    String.contains substring str


{-|

    Test: String/dropLeft

-}
stringDropLeft : Int -> String -> String
stringDropLeft n str =
    String.dropLeft n str


{-|

    Test: String/dropRight

-}
stringDropRight : Int -> String -> String
stringDropRight n str =
    String.dropRight n str


{-|

    Test: String/endsWith

-}
stringEndsWith : String -> String -> Bool
stringEndsWith ref str =
    String.endsWith ref str


{-|

    Test: String/join

-}
stringJoin : String -> List String -> String
stringJoin sep list =
    String.join sep list


{-|

    Test: String/length

-}
stringLength : String -> Int
stringLength str =
    String.length str


{-|

    Test: String/padLeft

-}
stringPadLeft : Int -> Char -> String -> String
stringPadLeft n ch str =
    String.padLeft n ch str


{-|

    Test: String/padRight

-}
stringPadRight : Int -> Char -> String -> String
stringPadRight n ch str =
    String.padRight n ch str


{-|

    Test: String/left
    expected = "Mu"

-}
stringLeftTest : TestContext -> String
stringLeftTest ctx =
    test ctx
        (String.left 2 "Mulder")


{-|

    Test: String/right
    expected = "ly"

-}
stringRightTest : TestContext -> String
stringRightTest ctx =
    test ctx
        (String.right 2 "Scully")


{-|

    Test: String/slice

-}
stringSlice : Int -> Int -> String -> String
stringSlice start end str =
    String.slice start end str


{-|

    Test: String/split

-}
stringSplit : String -> String -> List String
stringSplit sep str =
    String.split sep str


{-|

    Test: String/startsWith

-}
stringStartsWith : String -> String -> Bool
stringStartsWith ref str =
    String.startsWith ref str


{-|

    Test: String/fromInt
    expected = "25"

-}
stringFromIntTest : TestContext -> String
stringFromIntTest ctx =
    test ctx
        (String.fromInt 25)


{-|

    Test: String/fromFloat
    expected = "1.5"

-}
stringFromFloatTest : TestContext -> String
stringFromFloatTest ctx =
    test ctx
        (String.fromFloat 1.5)


{-|

    Test: String/toFloat
    expected = Just 1.5

-}
stringGoodToFloatTest : TestContext -> Maybe Float
stringGoodToFloatTest ctx =
    test ctx
        (String.toFloat "1.5")


{-|

    Test: String/toFloat - Invalid
    expected = Nothing

-}
stringBadToFloatTest : TestContext -> Maybe Float
stringBadToFloatTest ctx =
    test ctx
        (String.toFloat "not a float")


{-|

    Test: String/toInt
    expected = Just 25

-}
stringToIntTest1 : TestContext -> Maybe Int
stringToIntTest1 ctx =
    test ctx
        (String.toInt "25")


{-|

    Test: String/toInt - Invalid
    expected = Nothing

-}
stringToIntTest2 : TestContext -> Maybe Int
stringToIntTest2 ctx =
    test ctx
        (String.toInt "notAnInt")


{-|

    Test: String/isEmpty - True
    expected = True

-}
stringIsEmptyTest1 : TestContext -> Bool
stringIsEmptyTest1 ctx =
    test ctx
        (String.isEmpty "")


{-|

    Test: String/isEmpty - False
    expected = False

-}
stringIsEmptyTest2 : TestContext -> Bool
stringIsEmptyTest2 ctx =
    test ctx
        (String.isEmpty "content")


{-|

    Test: String/toLower

-}
stringToLower : String -> String
stringToLower str =
    String.toLower str


{-|

    Test: String/toUpper

-}
stringToUpper : String -> String
stringToUpper str =
    String.toUpper str


{-|

    Test: String/trim

-}
stringTrim : String -> String
stringTrim str =
    String.trim str


{-|

    Test: String/trimLeft

-}
stringTrimLeft : String -> String
stringTrimLeft str =
    String.trimLeft str


{-|

    Test: String/trimRight

-}
stringTrimRight : String -> String
stringTrimRight str =
    String.trimRight str


{-|

    Test: String/fromChar
    expected = "a"

-}
stringFromCharTest : TestContext -> String
stringFromCharTest ctx =
    test ctx
        (String.fromChar 'a')


{-|

    Test: String/cons
    expected = "abc"

-}
stringConsTest : TestContext -> String
stringConsTest ctx =
    test ctx
        (String.cons 'a' "bc")


{-|

    Test: String/uncons
    expected = Just ('a', "bc")

-}
stringUnconsTest : TestContext -> Maybe ( Char, String )
stringUnconsTest ctx =
    test ctx
        (String.uncons "abc")


{-|

    Test: String/toList
    expected = ['a', 'b', 'c']

-}
stringToListTest : TestContext -> List Char
stringToListTest ctx =
    test ctx
        (String.toList "abc")


{-|

    Test: String/fromList
    expected = "abc"

-}
stringFromListTest : TestContext -> String
stringFromListTest ctx =
    test ctx
        (String.fromList [ 'a', 'b', 'c' ])


{-|

    Test: String/pad
    pad 5 ' ' "1" == " 1 "
    pad 5 ' ' "11" == " 11 "
    pad 5 ' ' "121" == " 121 "
    pad 5 ' ' "1234" == " 1234"
    pad 5 ' ' "12345" == "12345"
    pad 5 ' ' "123456" == "123456"
    pad 0 ' ' "123" == "123"
    pad -5 ' ' "123" == "123"
    pad 5 ' ' "" == "     "

-}
stringPadTest : Int -> String -> String
stringPadTest size input =
    String.pad size ' ' input


{-|

    Test: String/map
    expected = "a.b.c"

-}
stringMapTest : TestContext -> String
stringMapTest ctx =
    test ctx
        String.map
        (\c ->
            if c == '/' then
                '.'

            else
                c
        )
        "a/b/c"


{-|

    Test: String/filter
    expected = "bc"

-}
stringFilterTest : TestContext -> String
stringFilterTest ctx =
    test ctx
        (String.filter (\c -> c /= 'a') "abc")


{-|

    Test: String/foldl
    input = UPPERCASE, expected = True
    input = lowercase, expected = False
    input = camelCase, expected = False

-}
stringFoldlTest : String -> Bool
stringFoldlTest input =
    String.foldl (\char acc -> acc && (char >= 'A' && char <= 'Z')) True input


{-|

    Test: String/foldr
    input = "Hello, World", expected = 2
    input = "HELLO, WORLD", expected = 10

-}
stringFoldrTest : String -> Int
stringFoldrTest input =
    String.foldr
        (\char count ->
            if char >= 'A' && char <= 'Z' then
                count + 1

            else
                count
        )
        0
        input


{-|

    Test: String/any
    input = "scala", expected = True
    input = "elm", expected = False

-}
stringAnyTest : String -> Bool
stringAnyTest input =
    String.any (\c -> c == 'a') input


{-|

    Test: String/all
    input = "aaa", expected = True
    input = "abc", expected = False

-}
stringAllTest : String -> Bool
stringAllTest input =
    String.all (\c -> c == 'a') input
