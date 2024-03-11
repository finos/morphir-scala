module Morphir.Examples.App.ExampleModule exposing (ModuleRecord, ModuleSingleUnion(..), ModuleUnion(..), TypeArgRecord(..), TypeArgUnion(..), five, inputRecordFunction, inputUnionFunction, outputRecordFunction, outputUnionFunction, parametricFunction, publicFunction, tupleReverse)

import Dict exposing (Dict)
import Morphir.Examples.App.TestUtils exposing (..)

type OpaqueInt = Opaque Int
wrap : Int -> OpaqueInt
wrap x = Opaque x
unwrap : OpaqueInt -> Int
unwrap o =
    let Opaque x = o in
    x

type alias ModuleRecord =
    { name : String, truth : Bool }


type ModuleUnion
    = Center
    | Up Int
    | Down Int


type ModuleSingleUnion
    = Only String Int


five : Int
five =
    5


tupleReverse : ( Int, Int ) -> ( Int, Int )
tupleReverse t =
    let
        ( a, b ) =
            t
    in
    ( b, a )


parametricFunction : a -> ( a, String )
parametricFunction arg =
    ( arg, "Hat" )


publicFunction : Int -> Int
publicFunction arg =
    privateFunction ( arg, arg )


privateFunction : ( Int, Int ) -> Int
privateFunction t =
    let
        ( x, y ) =
            t
    in
    x + y


outputRecordFunction : String -> ModuleRecord
outputRecordFunction s =
    { name = s, truth = False }


inputRecordFunction : ModuleRecord -> String
inputRecordFunction r =
    if r.truth then
        r.name

    else
        "Rumplestilskin"


outputUnionFunction : String -> Int -> ModuleUnion
outputUnionFunction dir dist =
    case dir of
        "Up" ->
            Up dist

        "Down" ->
            Down dist

        _ ->
            Center


inputUnionFunction : ModuleUnion -> Int
inputUnionFunction u =
    case u of
        Up dist ->
            dist

        Down dist ->
            -dist

        Center ->
            0


type TypeArgUnion a b
    = A a
    | B b
    | AB a b
    | MaybeA (Maybe a)
    | DictBA (Dict b a)


type alias TypeArgRecord a b c =
    { a : a, bList : List b, tuple : ( a, b, c ), union : TypeArgUnion c a }
