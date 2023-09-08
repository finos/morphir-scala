module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (map)

type alias TestContext = {}

test : TestContext -> a -> a
test context res = 
    case context of
        _ -> res
 
infinite : TestContext ->Bool
infinite ctx = test ctx infinite ()

test : TestContext ->Bool
test ctx = test ctx 
    let f = (&&) False in
    f (infinite ())
--expected = ()

main : Html ()
main = text (Debug.toString (test () ))