module Morphir.Examples.Accounting.Book exposing (..)

type alias Amount = Int
type alias BookName = String

type alias Book =
    { name: BookName
    , debits: List Amount
    , credits: List Amount
    }

make: BookName -> Book
make name =
    { name = name
    , debits = []
    , credits = []
    }
