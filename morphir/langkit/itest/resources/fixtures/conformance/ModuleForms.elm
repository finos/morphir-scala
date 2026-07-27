port module Conformance.ModuleForms exposing
    ( Model
    , Msg(..)
    , receive
    , send
    )

{-| A port module with an explicit exposing list, aliased and selective imports.
-}

import Dict
import Json.Decode as Decode exposing (Decoder, decodeString)
import Set exposing (Set)


type alias Model =
    { entries : Dict.Dict String Int
    , seen : Set String
    }


type Msg
    = Received String
    | Sent


port send : String -> Cmd msg


port receive : (String -> msg) -> Sub msg


decoder : Decoder Model
decoder =
    Decode.fail "not implemented"
