module Morphir.Examples.App.LocalDateTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.LocalDate exposing (..)


{-| Test: LocalDate:fromParts
expected = Just (1900 Jan 20)
-}
fromPartsTest : TestContext -> Maybe LocalDate
fromPartsTest ctx =
    test ctx
        (fromParts 1900 1 20)


{-| Test: LocalDate:fromParts, invalid
expected = Nothing
-}
fromPartsInvalidTest : TestContext -> Maybe LocalDate
fromPartsInvalidTest ctx =
    test ctx
        (fromParts 1900 1 9999)


{-| Test: LocalDate:addWeeks
input = 2, 1900-01-20
expected = 1900-02-03
-}
addWeeksTest : Int -> LocalDate -> LocalDate
addWeeksTest weeks date =
    addWeeks weeks date


{-| Test: LocalDate:diffInDays
input = 1900-01-20, 1902-10-16
expected = -999
-}
diffInDaysTest : LocalDate -> LocalDate -> Int
diffInDaysTest localDate1 localDate2 =
    diffInDays localDate1 localDate2


{-| Test: LocalDate:fromISO
-}
fromISOTest : String -> Maybe LocalDate
fromISOTest iso =
    fromISO iso


{-| Test: LocalDate:fromOrdinalDate
-}
fromOrdinalDateTest : Int -> Int -> LocalDate
fromOrdinalDateTest year dayOfYear =
    fromOrdinalDate year dayOfYear


{-| Test: LocalDate:year
-}
yearTest : LocalDate -> Int
yearTest localDate =
    year localDate


{-| Test: LocalDate:month
-}
monthTest : LocalDate -> Month
monthTest localDate =
    month localDate


{-| Test: LocalDate:monthNumber
-}
monthNumberTest : LocalDate -> Int
monthNumberTest localDate =
    monthNumber localDate


{-| Test: LocalDate:day
-}
dayTest : LocalDate -> Int
dayTest localDate =
    day localDate


{-| Test: LocalDate:dayOfWeek
-}
dayOfWeekTest : LocalDate -> DayOfWeek
dayOfWeekTest localDate =
    dayOfWeek localDate
