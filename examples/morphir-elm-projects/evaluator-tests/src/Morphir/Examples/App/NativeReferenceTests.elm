module Morphir.Examples.App.NativeReferenceTests exposing (..)

{-
   TODO:
       Morphir compiler might be importing wrong - explicit imports from Basics make it break
   Unhappy:
       Missing reference (elm precludes)
-}

import List exposing (map)
import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.LocalDate exposing (LocalDate)
import Morphir.SDK.LocalTime exposing (LocalTime)


{-| Test: NativeReference/Map
expected = [(1,1),(2,2),(3,3)]
-}
nativeReferenceMapTest : TestContext -> List ( Int, Int )
nativeReferenceMapTest ctx =
    test ctx <|
        map (\x -> ( x, x )) [ 1, 2, 3 ]


{-|

    Test: NativeReference/Add
    expected = 3

-}
nativeReferenceAddTest : TestContext -> Int
nativeReferenceAddTest ctx =
    test ctx <|
        let
            f x y =
                x + y
        in
        f 1 2


{-|

    Test: NativeReference/CurriedLog
    expected = Infinity

-}
nativeReferenceCurriedLogTest : TestContext -> Float
nativeReferenceCurriedLogTest ctx =
    test ctx <|
        let
            curried =
                let
                    f =
                        logBase
                in
                f 1
        in
        curried 2


{-|

    Test: NativeReference/Pi
    expected = 3

-}
nativeReferencePiTest : TestContext -> Float
nativeReferencePiTest ctx =
    test ctx <|
        pi


{-|

    Test: NativeReference/ModBy
    expected = x % 3

-}
nativeReferenceModByTest : Int -> Int
nativeReferenceModByTest x =
    modBy 3 x


localDatePassthrough : LocalDate -> LocalDate
localDatePassthrough x =
    x


localTimePassthrough : LocalTime -> LocalTime
localTimePassthrough x =
    x


nativeReferenceCeilingTest : Float -> Int
nativeReferenceCeilingTest x =
    ceiling x


nativeReferenceAbsTest : Float -> Float
nativeReferenceAbsTest x =
    abs x


-- nativeReferenceAbsTest : Int -> Int
-- nativeReferenceAbsTest x =
--     abs x
