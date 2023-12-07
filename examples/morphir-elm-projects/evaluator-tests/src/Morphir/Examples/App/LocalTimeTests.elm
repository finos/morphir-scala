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
addHoursTest : TestContext -> LocalTime
addHoursTest ctx =
    test ctx <|
        addHours 2 (fromMilliseconds 38606000)


{-| Test: LocalTime addMinutes
expected = java.time.LocalTime(10, 45, 26)
-}
addMinutesTest : TestContext -> LocalTime
addMinutesTest ctx =
    test ctx <|
        addMinutes 2 (fromMilliseconds 38606000)


{-| Test: LocalTime addSeconds
expected = java.time.LocalTime(10, 43, 28)
-}
addSecondsTest : TestContext -> LocalTime
addSecondsTest ctx =
    test ctx <|
        addSeconds 2 (fromMilliseconds 38606000)
