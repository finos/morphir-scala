module Morphir.Examples.App.TestUtils exposing (..)

type alias TestContext = {}

test : TestContext -> a -> a
test context res = 
    case context of
        _ -> res