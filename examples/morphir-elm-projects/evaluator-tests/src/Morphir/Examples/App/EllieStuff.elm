module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (map)

 
     
--Test: Simple/Unit
simpleUnitTest : () ->()
simpleUnitTest _ = 
    ()
--expected = ()

main : Html ()
main = text (Debug.toString (simpleUnitTest () ))