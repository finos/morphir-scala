module Morphir.Examples.App.LocalTimeTests exposing (..)
import Morphir.SDK.LocalTime exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

{-|
    Test: LocalTime/fromMilliseconds
    expected = java.time.LocalTime(10, 43, 26)
-}
fromMillisecondsTest : TestContext ->LocalTime
fromMillisecondsTest ctx = test ctx
    (fromMilliseconds 38606000)