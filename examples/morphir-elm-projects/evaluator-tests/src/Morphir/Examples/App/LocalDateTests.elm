module Morphir.Examples.App.LocalDateTests exposing (..)
import Morphir.SDK.LocalDate exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

{-|
Test: LocalDate/FromParts
expected = Just (1900 Jan 20)
-}
fromPartsTest : TestContext ->Maybe LocalDate
fromPartsTest ctx = test ctx
    fromParts 1900 1 20