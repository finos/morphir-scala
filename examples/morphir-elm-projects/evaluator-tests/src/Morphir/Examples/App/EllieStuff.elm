module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (map)


type alias TestContext =
    {}


test : TestContext -> a -> a
test context res =
    case context of
        _ ->
            res


infinite : TestContext -> Bool
infinite ctx =
    test ctx infinite {}


myTest : TestContext -> Bool
myTest ctx =
    test ctx
        True


output : Bool
output =
    myTest {}


main : Html ()
main =
    text (Debug.toString output)
