module Morphir.Examples.Accounting.Journal exposing (..)
import Morphir.SDK.LocalDate exposing (LocalDate)

type alias Journal =
    { entries : List Entry
    , nextId : Int
    }

type alias Entry =
    { id : Int
    , date: LocalDate
    }