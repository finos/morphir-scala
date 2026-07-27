module Conformance.Types exposing
    ( Direction(..)
    , Pair
    , Registry
    , Shape(..)
    , describe
    )

{-| Every type expression form, and both declaration forms that hold them.
-}

import Dict exposing (Dict)


type alias Pair =
    ( Int, Int )


type alias Registry =
    Dict String (List Int)


type alias Handler a =
    a -> String


type alias Person =
    { name : String
    , age : Int
    }


type alias Extensible a =
    { a | name : String }


type alias Nothingness =
    ()


type Direction
    = North
    | South
    | East
    | West


type Shape
    = Circle Float
    | Rectangle Float Float
    | Compound (List Shape)


describe : Direction -> Handler Shape
describe _ shape =
    case shape of
        Circle _ ->
            "circle"

        Rectangle _ _ ->
            "rectangle"

        Compound _ ->
            "compound"
