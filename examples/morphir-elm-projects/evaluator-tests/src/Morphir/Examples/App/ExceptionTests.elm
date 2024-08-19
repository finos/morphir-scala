module Morphir.Examples.App.ExceptionTests exposing (..)

import Morphir.SDK.Decimal as Decimal exposing (..)
import Tuple exposing (..)


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


{-| Test: ExceptionTests/acceptTuple3
-}
acceptTuple3 : ( Int, Int, Int ) -> String
acceptTuple3 _ =
    "accept tuple test"


type alias XYRecord =
    { x : Int, y : Int }


{-| Test: ExceptionTests/accept XYRecord
-}
acceptRecord : XYRecord -> String
acceptRecord _ =
    "accept record test"


type alias AliasAlias =
    { xyRecord : XYRecord }


{-| Test: ExceptionTests/acceptAliasTuple
-}
acceptAliasAlias : AliasAlias -> String
acceptAliasAlias _ =
    "accept AliasAlias test"


{-| Test: ExceptionTests/nonExhustiveCase
-}
nonExhaustiveCase : Int -> String
nonExhaustiveCase a =
    case a of
        1 ->
            "1"

        2 ->
            "2"
