module Morphir.Examples.App.LocalDateTests exposing (..)
import Morphir.SDK.LocalDate exposing (..)
import Morphir.Examples.App.TestUtils exposing (..)

--Test: List/FromParts
fromPartsTest : TestContext ->Maybe LocalDate
fromPartsTest ctx = test ctx
    fromParts 1900 1 20
--expected = Just (1900 Jan 20)