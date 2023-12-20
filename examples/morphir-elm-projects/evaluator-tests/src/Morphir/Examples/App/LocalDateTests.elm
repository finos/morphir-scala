module Morphir.Examples.App.LocalDateTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.LocalDate exposing (..)



{- TODO: current fromParts impl returns a NON Maybe type for entrypoint compat, but is typed Maybe in SDK.
   as a workaround, all enabled tests below take a LocalDate as input instead of constructing one
   in place using `fromParts`.
-}


{-| Test: LocalDate/FromParts
expected = Just (1900 Jan 20)
-}
fromPartsTest : TestContext -> Maybe LocalDate
fromPartsTest ctx =
    test ctx
        (fromParts 1900 1 20)


{-| Test: LocalDate addWeeks
input = 2, 1900-01-20
expected = 1900-02-03
-}
addWeeksTest : Int -> LocalDate -> LocalDate
addWeeksTest weeks date =
    addWeeks weeks date


{-| Test: LocalDate diffInDays
input = 1900-01-20, 1902-10-16
expected = -999
-}
diffInDaysTest : LocalDate -> LocalDate -> Int
diffInDaysTest localDate1 localDate2 =
    diffInDays localDate1 localDate2


{-| Test: LocalDate fromISO
-}
fromISOTest : String -> Maybe LocalDate
fromISOTest iso =
    fromISO iso
