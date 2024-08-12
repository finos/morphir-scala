module Morphir.Examples.App.ExceptionTests exposing (..)

import Morphir.SDK.Decimal as Decimal exposing (..)


{-| Test: ExceptionTests/add
-}
sdkAddTest : number -> number -> number
sdkAddTest a b =
    let
        f x y =
            x + y
    in
    f a b


{-| Test: ExceptionTests/hundred
-}
decimalHundred : Int -> Decimal
decimalHundred value =
    Decimal.hundred
        value


{-| Test: ExceptionTests/ignoreArgReturnString
-}
ignoreArgReturnString : a -> String
ignoreArgReturnString _ =
    "test"
