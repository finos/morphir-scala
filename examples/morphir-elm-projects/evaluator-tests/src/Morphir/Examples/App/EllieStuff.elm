module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (map)

 
infinite : () -> Bool
infinite _ = infinite ()

test : () -> Bool
test _ = 
    let f = (&&) False in
    f (infinite ())
--expected = ()

main : Html ()
main = text (Debug.toString (test () ))