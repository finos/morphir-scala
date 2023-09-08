module Morphir.Examples.App.LocalTimeTests exposing (..)
import Morphir.SDK.LocalTime exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

--Test: List/fromMilliseconds
fromMillisecondsTest : TestContext ->LocalTime
fromMillisecondsTest ctx = test ctx
    fromMilliseconds 38606000
--expected = java.time.LocalTime(10, 43, 26)