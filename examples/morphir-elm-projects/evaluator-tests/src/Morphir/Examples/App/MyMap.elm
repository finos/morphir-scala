module Morphir.Examples.App.MyMap exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)


myMap : (a -> b) -> List a -> List b
myMap f l =
    case l of
        head :: tail ->
            f head :: myMap f tail

        [] ->
            []
