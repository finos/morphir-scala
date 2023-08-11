module Morphir.Examples.App.LocalDateTests exposing (..)
import Morphir.SDK.LocalDate exposing (..)

--Test: List/FromParts
fromPartsTest : () -> Maybe LocalDate
fromPartsTest _ =
    fromParts 1900 1 20
--expected = Just (1900 Jan 20)