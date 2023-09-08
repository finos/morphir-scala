module Morphir.Examples.App.ExampleModule exposing (five, tupleReverse, parametricFunction, publicFunction, outputRecordFunction, inputRecordFunction, outputUnionFunction, inputUnionFunction, ModuleRecord, ModuleSingleUnion(..), ModuleUnion(..), TypeArgUnion(..), TypeArgRecord(..))
import Dict exposing (Dict)
import Morphir.Examples.App.TestUtils exposing (..)

type alias ModuleRecord = {name : String, truth : Bool}
type ModuleUnion = Center | Up Int | Down Int
type ModuleSingleUnion = Only String Int

five : Int
five = 5

tupleReverse : (Int, Int) -> (Int, Int)
tupleReverse t = 
    let
        (a, b) = t
    in
        (b, a)

parametricFunction : a -> (a, String)
parametricFunction arg = (arg, "Hat")

publicFunction : Int -> Int
publicFunction arg = privateFunction (arg, arg)

privateFunction : (Int, Int) -> Int
privateFunction t = 
    let
        (x, y) = t
    in
        x + y

outputRecordFunction : String -> ModuleRecord
outputRecordFunction s = {name = s, truth = False}

inputRecordFunction : ModuleRecord -> String
inputRecordFunction r = if r.truth then r.name else "Rumplestilskin"

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

        
type TypeArgUnion a b = A a | B b | AB a b | MaybeA (Maybe a) | DictBA (Dict b a)
type alias TypeArgRecord a b c = {a : a, bList : List b, tuple : (a, b, c), union : TypeArgUnion c a}

