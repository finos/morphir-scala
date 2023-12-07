module Morphir.Examples.App.LocalTimeTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.LocalTime exposing (..)


{-|

    Test: LocalTime/fromMilliseconds
    expected = java.time.LocalTime(10, 43, 26)

-}
fromMillisecondsTest : TestContext -> LocalTime
fromMillisecondsTest ctx =
    test ctx
        (fromMilliseconds 38606000)


{-| Test: LocalTime addHours
expected = java.time.LocalTime(12, 43, 26)
-}
addHoursTest : Int -> LocalTime -> LocalTime
addHoursTest =
    addHours


{-| Test: LocalTime addMinutes
-}
addMinutesTest : Int -> LocalTime -> LocalTime
addMinutesTest =
    addMinutes


{-| Test: LocalTime addSeconds
-}
addSecondsTest : Int -> LocalTime -> LocalTime
addSecondsTest =
    addSeconds
