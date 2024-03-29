module Morphir.Examples.Accounting.TAccount exposing (..)


type alias Amount =
    Int


type alias AccountName =
    String


type alias TAccount =
    { name : AccountName
    , balance : Amount
    , debits : List Amount
    , credits : List Amount
    }


make : AccountName -> Amount -> TAccount
make name balance =
    { name = name
    , balance = balance
    , debits = []
    , credits = []
    }


entry : TAccount -> TAccount -> Amount -> ( TAccount, TAccount )
entry debitAccount creditAccount amount =
    ( { debitAccount | debits = amount :: debitAccount.debits }
    , { creditAccount | credits = amount :: creditAccount.credits }
    )
