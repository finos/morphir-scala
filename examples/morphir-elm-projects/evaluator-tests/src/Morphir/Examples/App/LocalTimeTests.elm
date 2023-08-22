module Morphir.Examples.App.LocalTimeTests exposing (..)
import Morphir.SDK.LocalTime exposing (..)

--Test: List/fromMilliseconds
fromMillisecondsTest : () -> LocalTime
fromMillisecondsTest _ =
    fromMilliseconds 38606000
--expected = java.time.LocalTime(10, 43, 26)